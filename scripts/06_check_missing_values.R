# 06. 유의미한 변수들의 결측치 비율 확인

# 1. 라이브러리 로드
library(dplyr)
library(readr)

# 2. 데이터 로드 함수
load_and_combine_data <- function(group_name) {
  base_path <- file.path("data", "modeling_datasets", group_name)
  train_path <- file.path(base_path, paste0(group_name, "_train.csv"))
  valid_path <- file.path(base_path, paste0(group_name, "_valid.csv"))

  train_data <- readr::read_csv(train_path, col_types = cols(.default = "c"))
  valid_data <- readr::read_csv(valid_path, col_types = cols(.default = "c"))

  combined_data <- dplyr::bind_rows(train_data, valid_data)
  return(combined_data)
}

# 3. 결측치 확인 및 보고서 생성 함수
check_missing_values <- function(data, group_name, p_threshold) {
  cat(paste0("\n--- Checking Missing Values for: ", toupper(group_name), " (p < ", p_threshold, ") ---\n"))

  # 결과 저장 디렉토리 생성
  output_dir <- "results/missing_value_reports"
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # 단변량 분석에서 선택된 변수 파일 경로
  p_val_str <- sub("0\\.", "", format(p_threshold, nsmall = 2))
  significant_vars_file <- file.path("results", "univariate_analysis", paste0(group_name, "_significant_p", p_val_str, ".csv"))

  if (!file.exists(significant_vars_file)) {
    cat(paste("Skipping: Significant variable file not found at", significant_vars_file, "\n"))
    return(NULL)
  }

  significant_vars_df <- readr::read_csv(significant_vars_file, col_types = cols(.default = "c"))
  
  if (nrow(significant_vars_df) == 0) {
    cat(paste("Skipping: No significant variables found in", significant_vars_file, "\n"))
    return(NULL)
  }
  
  predictors <- unique(significant_vars_df$variable)
  
  # 각 변수의 결측치 비율 계산
  missing_report <- data %>%
    select(all_of(predictors)) %>%
    summarise(across(everything(), ~sum(is.na(.)) / n() * 100)) %>%
    tidyr::pivot_longer(everything(), names_to = "variable", values_to = "missing_percentage") %>%
    arrange(desc(missing_percentage))

  # 콘솔에 출력
  print(missing_report)

  # CSV 파일로 저장
  output_path <- file.path(output_dir, paste0(group_name, "_missing_report_p", p_val_str, ".csv"))
  write_csv(missing_report, output_path)
  cat(paste("Missing value report saved to:", output_path, "\n"))
}

# 4. 데이터 그룹 정의 및 로드
data_groups <- list(
  total = load_and_combine_data("total"),
  eocrc = load_and_combine_data("eocrc"),
  locrc = load_and_combine_data("locrc")
)

# p-value 임계값 정의
p_thresholds <- c(0.05, 0.20)

# 각 그룹과 임계값에 대해 결측치 확인 실행
for (group in names(data_groups)) {
  for (p_val in p_thresholds) {
    check_missing_values(data = data_groups[[group]], group_name = group, p_threshold = p_val)
  }
}

cat("\n--- Missing value check complete. ---\n")
