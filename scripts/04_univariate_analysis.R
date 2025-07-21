# 04_univariate_analysis.R
# 단변량 Cox 회귀분석을 통한 생존 관련 변수 선별

# 1. 패키지 및 데이터 로드 =========================================================

# lintr 경고 방지
utils::globalVariables(c(".data", "%>%", "생존기간_일", "사망여부", "p.value", "row_type"))

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, survival, gtsummary, cardx, magrittr, broom.helpers, flextable, conflicted)

# 충돌 해결
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")

# 결과 저장 디렉터리 생성
output_dir <- "results/univariate_analysis"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# 데이터 로드 함수
load_data <- function(path) {
  readr::read_csv(path, col_types = readr::cols(.default = "c")) %>%
    dplyr::mutate(
      생존기간_일 = as.numeric(생존기간_일),
      사망여부 = as.numeric(사망여부 == "Y"),
      dplyr::across(tidyselect::where(is.character), as.factor)
    ) %>% 
    dplyr::filter(!is.na(생존기간_일), !is.na(사망여부))
}

# 각 그룹 데이터 로드
datasets <- list(
  total = load_data("data/modeling_datasets/total/total_train.csv"),
  eocrc = load_data("data/modeling_datasets/eocrc/eocrc_train.csv"),
  locrc = load_data("data/modeling_datasets/locrc/locrc_train.csv")
)

# 2. 단변량 분석 수행 =============================================================

# 분석 대상 변수 리스트 (데이터 타입에 맞게 조정 필요)
# 예시: 기본환자병기값, 성별, 연령, 수술여부, 항암치료여부, CEA 수치 등
# 실제 변수명은 데이터셋 확인 후 정확히 기입해야 함
predictor_vars <- c(
  "기본환자성별코드", "기본환자진단시연령", "기본환자병기값",
  "기본환자T병기값", "기본환자N병기값", "기본환자M병기값",
  "수술여부", "항암치료여부", "방사선치료여부",
  "진단시점_CEA", "최고_CEA",
  "KRAS_MUTATION", "MSI_STATUS"
)



# 단변량 Cox 회귀분석 함수
# p-value에 따라 유의미한 결과를 필터링하고 저장하는 함수
filter_and_save_significant <- function(table_body_df, group_name, output_dir) {
  # p.value가 있는 'level' 행만 필터링
  significant_vars <- table_body_df %>% 
    dplyr::filter(row_type == 'level' & !is.na(p.value))

  # p < 0.05 필터링 및 저장
  significant_p05 <- significant_vars %>% 
    dplyr::filter(p.value < 0.05)
  
  if (nrow(significant_p05) > 0) {
    output_path_p05 <- file.path(output_dir, paste0(group_name, "_significant_p05.csv"))
    readr::write_csv(significant_p05, output_path_p05)
    cat(paste("Saved significant (p<0.05) results to:", output_path_p05, "\n"))
  }

  # p < 0.20 필터링 및 저장
  significant_p20 <- significant_vars %>% 
    dplyr::filter(p.value < 0.20)
  
  if (nrow(significant_p20) > 0) {
    output_path_p20 <- file.path(output_dir, paste0(group_name, "_significant_p20.csv"))
    readr::write_csv(significant_p20, output_path_p20)
    cat(paste("Saved significant (p<0.20) results to:", output_path_p20, "\n"))
  }
}

run_univariate_cox <- function(data, predictors, group_name) {
  cat(paste("\n--- Univariate Analysis for:", toupper(group_name), "---\n"))
  
  # 결과 저장 경로 설정
  csv_output_dir <- "results/univariate_analysis"
  md_output_dir <- "docs/univariate_analysis_reports"
  dir.create(csv_output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(md_output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # 모든 예측 변수에 대해 단변량 분석 수행
  result_summary <- gtsummary::tbl_uvregression(
    data = data,
    method = survival::coxph,
    y = survival::Surv(생존기간_일, 사망여부),
    include = dplyr::all_of(predictors),
    exponentiate = TRUE, # HR을 위해 지수 변환
    pvalue_fun = function(x) gtsummary::style_pvalue(x, digits = 3)
  ) %>% 
  gtsummary::add_nevent()

  # 1. 마크다운 보고서 저장 (사람 확인용)
  md_report_path <- file.path(md_output_dir, paste0(group_name, "_univariate_report.md"))
  result_summary %>% 
    gtsummary::as_kable() %>% 
    writeLines(con = md_report_path)
  cat(paste("Markdown report saved to:", md_report_path, "\n"))

  # 2. CSV 파일 저장 (기계 처리용)
  # gtsummary 객체의 내부 테이블($table_body)을 데이터프레임으로 사용
  result_df <- result_summary$table_body
  
  # 전체 결과 CSV 파일로 저장
  output_path_full <- file.path(csv_output_dir, paste0(group_name, "_univariate_cox_full_results.csv"))
  readr::write_csv(result_df, output_path_full)
  cat(paste("Full univariate analysis results for", group_name, "saved to:", output_path_full, "\n"))

  # 유의미한 결과 필터링 및 저장 (CSV)
  filter_and_save_significant(result_df, group_name, csv_output_dir)
  
  return(result_summary)
}

# 각 데이터셋에 대해 분석 실행 및 결과 저장
all_results <- list()

for (group_name in names(datasets)) {
  current_data <- datasets[[group_name]]
  
  # 데이터에 없는 변수는 제외
  valid_predictors <- predictor_vars[predictor_vars %in% names(current_data)]
  
  # 수치형 변수 타입 변환
  current_data <- current_data %>% 
    dplyr::mutate(dplyr::across(dplyr::any_of(c("기본환자진단시연령", "진단시점_CEA", "최고_CEA")), as.numeric))

  # 단일 값만 갖는 변수 제외 (2개 이상의 고유한 non-NA 값을 가져야 함)
  final_predictors <- valid_predictors[sapply(current_data[valid_predictors], function(x) length(unique(na.omit(x)))) > 1]
  
  excluded_vars <- setdiff(valid_predictors, final_predictors)
  if (length(excluded_vars) > 0) {
    cat(paste("Info: For group '", group_name, "', excluding variables with a single unique value: ", paste(excluded_vars, collapse = ", "), "\n", sep = ""))
  }

  # 분석 실행 및 결과 저장
  if (length(final_predictors) > 0) {
    all_results[[group_name]] <- run_univariate_cox(current_data, final_predictors, group_name)
  } else {
    cat(paste("Info: For group '", group_name, "', no valid predictors to analyze.\n", sep = ""))
  }
}

cat("\n--- 모든 그룹에 대한 단변량 분석 완료 ---\n")

# 결과 출력 (RStudio Viewer에서 확인)
all_results$total
all_results$eocrc
all_results$locrc
