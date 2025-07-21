# 05. 다변량 콕스 회귀 분석 (Multivariate Cox Regression Analysis)

# 1. 라이브러리 로드
library(survival)
library(dplyr)
library(gtsummary)
library(readr)

# 2. 데이터 로드
# 각 그룹별(total, eocrc, locrc)로 train/validation 데이터를 로드하여 하나로 합치는 함수
load_and_combine_data <- function(group_name) {
  base_path <- file.path("data", "modeling_datasets", group_name)
  train_path <- file.path(base_path, paste0(group_name, "_train.csv"))
  valid_path <- file.path(base_path, paste0(group_name, "_valid.csv"))

  train_data <- readr::read_csv(train_path, col_types = cols(.default = "c"))
  valid_data <- readr::read_csv(valid_path, col_types = cols(.default = "c"))

  combined_data <- dplyr::bind_rows(train_data, valid_data)
  
  # 데이터 타입 변환
  combined_data <- combined_data %>% 
    mutate(
      # 생존 분석 변수: Y -> 1, N -> 0
      사망여부 = ifelse(.data$사망여부 == 'Y', 1, 0),
      # 다른 변수들은 숫자형으로
      across(c("기본환자진단시연령", "생존기간_일", "진단시점_CEA", "최고_CEA"), as.numeric)
    ) %>% 
    # 생존기간이나 사망여부에 결측치가 있는 데이터는 제거
    filter(!is.na(.data$생존기간_일) & !is.na(.data$사망여부))
  
  return(combined_data)
}

# 3. 다변량 콕스 회귀 분석 수행 및 결과 저장 함수
run_multivariate_cox <- function(data, group_name, model_type, predictors) {
  cat(paste0("\n--- Running Multivariate Analysis for: ", toupper(group_name), " (Model: ", model_type, ") ---\n"))

  # 결과 저장 디렉토리 생성
  csv_output_dir <- "results/multivariate_analysis"
  md_output_dir <- "docs/multivariate_analysis_reports"
  dir.create(csv_output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(md_output_dir, recursive = TRUE, showWarnings = FALSE)

  # 포뮬러 생성
  formula_str <- paste("Surv(생존기간_일, 사망여부) ~", paste(predictors, collapse = " + "))
  cox_formula <- as.formula(formula_str)

  # 모델에 필요한 데이터만 선택하고 결측치 제거
  # 생존 분석 변수도 포함해야 함
  required_vars <- all.vars(cox_formula)
  model_data <- data[, intersect(required_vars, names(data))]
  model_data_complete <- na.omit(model_data)

  if (nrow(model_data_complete) < 20) { # 최소 관측치 수 설정
    cat(paste("Skipping for", group_name, "Model:", model_type, ": Not enough complete observations (", nrow(model_data_complete), ").\n"))
    return(NULL)
  }

  # 다변량 콕스 모델 적합
  cox_model <- tryCatch({
    survival::coxph(cox_formula, data = model_data_complete)
  }, error = function(e) {
    cat(paste("Error in coxph for", group_name, "Model:", model_type, ":", e$message, "\n"))
    return(NULL)
  })

  if (is.null(cox_model)) return(NULL)

  # 결과 요약 (gtsummary)
  result_summary <- gtsummary::tbl_regression(
    cox_model,
    exponentiate = TRUE,
    pvalue_fun = function(x) gtsummary::style_pvalue(x, digits = 3)
  )

  # 결과 저장
  file_suffix <- paste0(group_name, "_multivariate_", model_type, "_model")
  md_report_path <- file.path(md_output_dir, paste0(file_suffix, "_report.md"))
  csv_output_path <- file.path(csv_output_dir, paste0(file_suffix, "_results.csv"))

  result_summary %>% 
    gtsummary::as_kable() %>% 
    writeLines(con = md_report_path)
  cat(paste("Markdown report saved to:", md_report_path, "\n"))

  result_df <- as.data.frame(result_summary$table_body)
  readr::write_csv(result_df, csv_output_path)
  cat(paste("CSV results saved to:", csv_output_path, "\n"))

  return(result_summary)
}

# 4. 분석 실행
# 데이터 그룹 정의 및 로드
data_groups <- list(
  total = load_and_combine_data("total"),
  eocrc = load_and_combine_data("eocrc"),
  locrc = load_and_combine_data("locrc")
)

# 분석 모델에 포함할 변수 목록 정의
# 모델 1: 핵심 변수 모델 (결측치가 거의 없는 변수)
core_predictors <- c("기본환자진단시연령", "기본환자성별코드", "수술여부", "항암치료여부", "방사선치료여부", "진단시점_CEA", "최고_CEA")

# 모델 2: 병기 포함 모델
staging_predictors <- c(core_predictors, "기본환자병기값")

# 분석 모델 리스트
model_list <- list(
  core = core_predictors,
  staging = staging_predictors
)

# 각 그룹과 모델에 대해 분석 실행
for (group in names(data_groups)) {
  for (model_name in names(model_list)) {
    # 그룹 데이터에 해당 변수들이 모두 있는지 확인 후 실행
    available_predictors <- intersect(model_list[[model_name]], names(data_groups[[group]]))
    if(length(available_predictors) > 1) { # 최소 2개 이상의 예측 변수가 있을 때 실행
        run_multivariate_cox(
            data = data_groups[[group]], 
            group_name = group, 
            model_type = model_name, 
            predictors = available_predictors
        )
    }
  }
}

cat("\n--- Multivariate analysis complete. ---\n")
