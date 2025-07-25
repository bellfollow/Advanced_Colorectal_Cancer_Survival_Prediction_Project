# 0. 환경 설정: 라이브러리 로드 및 데이터 불러오기

# 필요한 라이브러리 로드
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(tidyverse)
  library(plotly)
  library(survival)
  library(survminer)
  library(DT)
  library(reshape2)
})

# ==============================================================================
# 1. 데이터 로딩 및 전처리
# ==============================================================================

# 데이터 불러오기
file_path <- file.path("..", "data", "preprocessed", "total_preprocessed.csv")

if (file.exists(file_path)) {
  full_data <- readr::read_csv(file_path, col_types = readr::cols(.default = "c"))
} else {
  # 데이터 파일이 없으면 앱을 중지하고 에러 메시지를 표시합니다.
  stop(paste("CRITICAL ERROR: 데이터 파일을 찾을 수 없습니다:", file_path))
}

# 기본 전처리
full_data <- full_data %>% 
    mutate(
        # 숫자형으로 변환해야 할 컬럼들
        기본환자진단시연령 = as.numeric(기본환자진단시연령),
        사망여부 = as.numeric(사망여부),
        생존기간_일 = as.numeric(생존기간_일),
        
        # EOCRC (< 50)와 LOCRC (>= 50) 그룹 정의
        Group = ifelse(기본환자진단시연령 < 50, "EOCRC", "LOCRC"),
        
        # 병기(Stage) 값 표준화
        Stage = as.character(기본환자병기값),
        Stage = case_when(
            is.na(Stage) | Stage %in% c("", "NA", "모름") ~ "Unknown",
            TRUE ~ paste0("Stage ", Stage)
        ),
        Stage = factor(Stage, levels = c("Stage 0", "Stage 1", "Stage 2", "Stage 3", "Stage 4", "Unknown"))
    ) %>% 
    # 생존 분석에 필요한 유효한 데이터만 필터링
    filter(!is.na(생존기간_일), 생존기간_일 > 0, !is.na(사망여부))

# ==============================================================================
# 2. 전역 변수 및 요약 통계 계산
# ==============================================================================
# InfoBox에 사용될 요약 통계
total_patients <- n_distinct(full_data$환자대체번호)
eocrc_patients <- full_data %>% filter(Group == "EOCRC") %>% n_distinct()
locrc_patients <- full_data %>% filter(Group == "LOCRC") %>% n_distinct()
death_count <- full_data %>% filter(사망여부 == 1) %>% n_distinct()

# --- Home 탭을 위한 추가 지표 ---

# 1. 중앙 생존 기간
surv_object_global <- Surv(time = full_data$생존기간_일, event = full_data$사망여부)
fit_global <- survfit(surv_object_global ~ 1, data = full_data)
median_survival_months <- round(surv_median(fit_global)$median / 30.44, 1)

# 2. 치료 비율
treatment_rates <- full_data %>% 
    summarise(
        surgery = round(mean(as.numeric(수술여부) %in% c(1, 2, 3, 4, 5), na.rm = TRUE) * 100, 1),
        chemo = round(mean(항암치료여부 == "Y", na.rm = TRUE) * 100, 1),
        radio = round(mean(방사선치료여부 == "Y", na.rm = TRUE) * 100, 1)
    )
