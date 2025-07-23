# 05_multivariate_analysis.R
# 단변량 분석 결과(p-value)를 기반으로 다변량 콕스 회귀 분석을 수행하는 스크립트
# Firth's Penalized Regression을 사용하여 '완전 분리' 문제 해결

# 1. 라이브러리 로드
# coxphf 패키지가 없으면 설치
if (!require("coxphf")) install.packages("coxphf", repos = "https://cloud.r-project.org")

library(survival)
library(dplyr)
library(gtsummary)
library(readr)
library(coxphf)

# 2. 데이터 로드 함수
load_and_combine_data <- function(group_name) {
  base_path <- file.path("data", "modeling_datasets", group_name)
  train_path <- file.path(base_path, paste0(group_name, "_train.csv"))
  valid_path <- file.path(base_path, paste0(group_name, "_valid.csv"))

  if (!file.exists(train_path) || !file.exists(valid_path)) {
    warning(paste("Data files not found for group:", group_name))
    return(NULL)
  }

  train_data <- readr::read_csv(train_path, col_types = cols(.default = "c"))
  valid_data <- readr::read_csv(valid_path, col_types = cols(.default = "c"))

  combined_data <- dplyr::bind_rows(train_data, valid_data)

  combined_data <- combined_data %>% 
    mutate(
      사망여부 = ifelse(.data$사망여부 == 'Y', 1, 0),
      across(c("기본환자진단시연령", "생존기간_일", "진단시점_CEA", "최고_CEA"), as.numeric)
    ) %>% 
    filter(!is.na(.data$생존기간_일) & !is.na(.data$사망여부))
  
  return(combined_data)
}

# 3. 단변량 분석 결과에서 유의한 변수 추출 함수
get_significant_predictors <- function(group_name, p_threshold) {
  univariate_results_path <- file.path("results", "univariate_analysis", paste0(group_name, "_univariate_cox_full_results.csv"))
  
  if (!file.exists(univariate_results_path)) {
    stop(paste("Univariate results file not found for group:", group_name))
  }
  
  univariate_results <- readr::read_csv(univariate_results_path)
  
  significant_vars <- univariate_results %>% 
    filter(.data$p.value < p_threshold) %>% 
    pull(.data$variable)
  
  return(significant_vars)
}

# 4. 다변량 콕스 회귀 분석 수행 및 결과 저장 함수 (Firth's Regression 사용)
run_multivariate_cox <- function(data, group_name, model_type, predictors) {
  cat(paste0("\n--- Running Multivariate Analysis for: ", toupper(group_name), " (Model: ", model_type, ", Engine: Firth) ---\n"))

  csv_output_dir <- "results/multivariate_analysis"
  md_output_dir <- "docs/multivariate_analysis_reports"
  dir.create(csv_output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(md_output_dir, recursive = TRUE, showWarnings = FALSE)

  if (length(predictors) == 0) {
    cat(paste("Skipping for", group_name, "Model:", model_type, ": No significant predictors found.\n"))
    return(NULL)
  }

  formula_str <- paste("Surv(생존기간_일, 사망여부) ~", paste(predictors, collapse = " + "))
  cox_formula <- as.formula(formula_str)

  required_vars <- all.vars(cox_formula)
  model_data <- data[, intersect(required_vars, names(data)), drop = FALSE]
  model_data_complete <- na.omit(model_data)

  if (nrow(model_data_complete) < 20) {
    cat(paste("Skipping for", group_name, "Model:", model_type, ": Not enough complete observations (", nrow(model_data_complete), ").\n"))
    return(NULL)
  }

  cox_model <- tryCatch({
    # coxphf 함수 사용
    coxphf(formula = cox_formula, data = model_data_complete)
  }, error = function(e) {
    cat(paste("Error in coxphf for", group_name, "Model:", model_type, ":", e$message, "\n"))
    return(NULL)
  })

  if (is.null(cox_model)) return(NULL)

  # 수동으로 결과 추출
  results_df <- data.frame(
    variable = names(coef(cox_model)),
    HR = exp(coef(cox_model)),
    lower_ci = exp(confint(cox_model)[, 1]),
    upper_ci = exp(confint(cox_model)[, 2]),
    p_value = cox_model$prob
  )
  
  # CSV 저장 (모델 이름에 _firth 추가)
  csv_filename <- file.path(csv_output_dir, paste0(group_name, "_multivariate_", model_type, "_firth_results.csv"))
  write.csv(results_df, csv_filename, row.names = FALSE)
  cat(paste("CSV results saved to:", csv_filename, "\n"))

  # Markdown 리포트 생성은 gtsummary 오류로 인해 비활성화
  # md_filename <- file.path(md_output_dir, paste0(group_name, "_multivariate_", model_type, "_firth_report.md"))
  # gtsummary::as_gt(model_summary) %>% gt::as_raw_html() %>% 
  #   cat(file = md_filename)
  # cat(paste("Markdown report saved to:", md_filename, "\n"))
}

# 5. 메인 실행 로직
main <- function() {
  data_groups <- c("total", "eocrc", "locrc")
  p_thresholds <- list(p_lt_005 = 0.05, p_lt_020 = 0.20)

  for (group in data_groups) {
    cat(paste0("\n================ Processing Group: ", toupper(group), " =================\n"))
    
    dataset <- load_and_combine_data(group)
    if (is.null(dataset)) next

    for (model_name in names(p_thresholds)) {
      p_value <- p_thresholds[[model_name]]
      
      predictors <- get_significant_predictors(group, p_value)
      
      run_multivariate_cox(dataset, group, model_name, predictors)
    }
  }
  cat("\n\n--- All analyses complete. ---\n")
}

# 스크립트 실행
main()
