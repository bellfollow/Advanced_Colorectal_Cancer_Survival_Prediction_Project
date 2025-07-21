# 디버깅 스크립트: 데이터 로딩 및 전처리 과정 확인

# 1. 라이브러리 로드
library(dplyr)
library(readr)

# 2. 데이터 로드 (total 그룹 대상)
cat("--- Loading 'total' group data ---\n")
base_path <- file.path("data", "modeling_datasets", "total")
train_path <- file.path(base_path, "total_train.csv")
valid_path <- file.path(base_path, "total_valid.csv")

train_data <- readr::read_csv(train_path, col_types = cols(.default = "c"))
valid_data <- readr::read_csv(valid_path, col_types = cols(.default = "c"))

combined_data <- dplyr::bind_rows(train_data, valid_data)

cat("Total rows loaded:", nrow(combined_data), "\n\n")

# 3. 숫자형 변환 전, 생존 관련 변수 값 확인
cat("--- Checking survival variables BEFORE numeric conversion ---\n")
print(head(combined_data[, c("생존기간_일", "사망여부")]))
cat("\nUnique values in '생존기간_일':\n")
print(unique(combined_data$생존기간_일))
cat("\nUnique values in '사망여부':\n")
print(unique(combined_data$사망여부))

# 4. 숫자형 변환 수행
cat("\n--- Converting to numeric type ---\n")
processed_data <- combined_data %>%
  mutate(across(c(생존기간_일, 사망여부), as.numeric))

# 5. 숫자형 변환 후, 결측치 확인
cat("\n--- Checking for NAs AFTER numeric conversion ---\n")
na_survival_time <- sum(is.na(processed_data$생존기간_일))
na_status <- sum(is.na(processed_data$사망여부))

cat("Number of NAs in '생존기간_일':", na_survival_time, "(", round(100*na_survival_time/nrow(processed_data), 2), "%)\n")
cat("Number of NAs in '사망여부':", na_status, "(", round(100*na_status/nrow(processed_data), 2), "%)\n")

# 6. 핵심 변수 모델(Core Model) 기준 결측치 확인
cat("\n--- Checking missing data for Core Model variables ---\n")
core_predictors <- c("생존기간_일", "사망여부", "기본환자진단시연령", "기본환자성별코드", "수술여부", "항암치료여부", "방사선치료여부", "진단시점_CEA", "최고_CEA")

# 변환이 필요한 다른 변수들도 숫자형으로 변경
processed_data <- processed_data %>% 
    mutate(across(c(기본환자진단시연령, 진단시점_CEA, 최고_CEA), as.numeric))

model_data <- processed_data[, intersect(core_predictors, names(processed_data))]
complete_cases <- na.omit(model_data)

cat("Number of rows before na.omit():", nrow(model_data), "\n")
cat("Number of rows after na.omit():", nrow(complete_cases), "\n")
cat("This is the number of 'complete observations' for the core model.\n")
