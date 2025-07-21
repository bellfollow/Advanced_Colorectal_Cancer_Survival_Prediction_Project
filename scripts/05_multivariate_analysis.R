# 05. 다변량 콕스 회귀 분석 (Multivariate Cox Regression Analysis)

# 1. 라이브러리 로드
library(survival)
library(dplyr)
library(gtsummary)
library(readr)

# 2. 데이터 로드
# 기본 데이터셋 로드
final_data <- readr::read_csv("data/final_data.csv", col_types = cols(.default = "c"))

# 데이터 타입 변환 (분석에 필요한 열들을 숫자형으로)
final_data <- final_data %>%
  mutate(across(c(기본환자진단시연령, 생존기간_일, 사망여부, 진단시점_CEA, 최고_CEA), as.numeric))

# 3. 다변량 콕스 회귀 분석 수행 및 결과 저장 함수
run_multivariate_cox <- function(data, group_name, p_threshold) {
  cat(paste0("\n--- Running Multivariate Analysis for: ", toupper(group_name), " (p < ", p_threshold, ") ---\n"))

  # 결과 저장 디렉토리 생성
  csv_output_dir <- "results/multivariate_analysis"
  md_output_dir <- "docs/multivariate_analysis_reports"
  dir.create(csv_output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(md_output_dir, recursive = TRUE, showWarnings = FALSE)

  # 단변량 분석에서 선택된 변수 파일 경로
  p_val_str <- sub("0\\.", "", as.character(p_threshold))
  significant_vars_file <- file.path("results", "univariate_analysis", paste0(group_name, "_significant_p", p_val_str, ".csv"))

  if (!file.exists(significant_vars_file)) {
    cat(paste("Skipping: Significant variable file not found at", significant_vars_file, "\n"))
    return(NULL)
  }

  # 유의미한 변수 목록 읽기
  significant_vars_df <- readr::read_csv(significant_vars_file, col_types = cols(.default = "c"))
  
  if (nrow(significant_vars_df) == 0) {
    cat(paste("Skipping: No significant variables found in", significant_vars_file, "\n"))
    return(NULL)
  }
  
  predictors <- unique(significant_vars_df$variable)

  # 포뮬러 생성
  formula_str <- paste("Surv(생존기간_일, 사망여부) ~", paste(predictors, collapse = " + "))
  cox_formula <- as.formula(formula_str)

  # 다변량 콕스 모델 적합
  cox_model <- tryCatch({
    survival::coxph(cox_formula, data = data)
  }, error = function(e) {
    cat(paste("Error in coxph for", group_name, "p <", p_threshold, ":", e$message, "\n"))
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
  # Markdown 리포트
  md_report_path <- file.path(md_output_dir, paste0(group_name, "_multivariate_p", p_val_str, "_report.md"))
  result_summary %>%
    gtsummary::as_kable() %>%
    writeLines(con = md_report_path)
  cat(paste("Markdown report saved to:", md_report_path, "\n"))

  # CSV 결과
  result_df <- as.data.frame(result_summary$table_body)
  csv_output_path <- file.path(csv_output_dir, paste0(group_name, "_multivariate_p", p_val_str, "_results.csv"))
  readr::write_csv(result_df, csv_output_path)
  cat(paste("CSV results saved to:", csv_output_path, "\n"))

  return(result_summary)
}

# 4. 분석 실행
# 데이터 그룹 정의
data_groups <- list(
  total = final_data,
  eocrc = final_data %>% filter(EOCRC_LOCRC_GROUP == "EOCRC"),
  locrc = final_data %>% filter(EOCRC_LOCRC_GROUP == "LOCRC")
)

# p-value 임계값 정의
p_thresholds <- c(0.05, 0.20)

# 각 그룹과 임계값에 대해 분석 실행
for (group in names(data_groups)) {
  for (p_val in p_thresholds) {
    run_multivariate_cox(data = data_groups[[group]], group_name = group, p_threshold = p_val)
  }
}

cat("\n--- Multivariate analysis complete. ---\n")
