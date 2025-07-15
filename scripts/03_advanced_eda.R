# 04_advanced_eda.R
# 심층 탐색적 데이터 분석 (Advanced EDA)

# 1. 환경 설정
# -----------------------------------------------------------------------------
# 라이브러리 로드
library(tidyverse)
library(survival)
library(survminer)
library(ggcorrplot)
library(conflicted)

# 충돌 해결
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# 결과 저장 디렉토리 생성
output_dir <- file.path("results", "advanced_eda")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 분석 결과 저장을 위한 텍스트 파일 설정
summary_file <- file.path(output_dir, "00_advanced_eda_summary.txt")
sink(summary_file)
cat("--- 고급 탐색적 데이터 분석(Advanced EDA) 요약 보고서 ---
\n")

# 데이터 로드
# 데이터 로드
# 데이터 로드
train_df_raw <- read_csv("data/modeling_datasets/total/total_train.csv")
cat("--- Raw Data Loaded ---\n")
cat(paste("Raw data dimensions:", nrow(train_df_raw), "rows,", ncol(train_df_raw), "cols\n"))
cat("\nTable of raw '사망여부':\n")
print(table(train_df_raw$사망여부, useNA = "always"))
cat("\nSummary of raw '전체생존기간일수':\n")
print(summary(train_df_raw$전체생존기간일수))


# 데이터 정제
train_df <- train_df_raw %>%
  mutate(사망여부 = as.numeric(ifelse(사망여부 == "Y", 1, 0))) %>%
  filter(!is.na(전체생존기간일수), !is.na(사망여부))

cat("\n--- Data Cleaned ---\n")
cat(paste("Cleaned data dimensions:", nrow(train_df), "rows,", ncol(train_df), "cols\n"))
cat("\nTable of cleaned '사망여부':\n")
print(table(train_df$사망여부, useNA = "always"))
cat("\nSummary of cleaned '전체생존기간일수':\n")
print(summary(train_df$전체생존기간일수))

# 데이터 유효성 검사
if(nrow(train_df) == 0) {
  stop("No data remaining after cleaning. Halting execution.")
}
if(length(unique(train_df$사망여부)) < 2) {
    cat("\nWARNING: The '사망여부' column has less than 2 unique values after cleaning.\nThis will result in a null model for survival analysis.\n")
}

# 2. 데이터 전처리 및 그룹 생성
# -----------------------------------------------------------------------------
# 연령 그룹(EOCRC/LOCRC) 생성
train_df <- train_df %>%
  mutate(
    age_group = case_when(
      기본환자진단시연령 <= 50 ~ "EOCRC",
      기본환자진단시연령 > 50  ~ "LOCRC",
      TRUE ~ NA_character_
    )
  ) %>% 
  filter(!is.na(age_group)) # 연령 정보 없는 경우 제외

# 병기(Stage) 변수 정리 ('x' -> NA)
train_df <- train_df %>% 
  mutate(기본환자병기값 = if_else(기본환자병기값 == 'x', NA_character_, 기본환자병기값))

cat("\n--- 연령 그룹 생성 완료 ---\n")
print(table(train_df$age_group))

# 3. 그룹별 특성 비교 (EOCRC vs LOCRC)
# -----------------------------------------------------------------------------
cat("\n--- 그룹별 특성 비교 분석 시작 ---\n")

# 3.1. 병기(Stage) 분포 비교
# ---------------------------
stage_dist_plot <- train_df %>%
  filter(!is.na(기본환자병기값)) %>%
  ggplot(aes(x = 기본환자병기값, fill = age_group)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "연령 그룹별 암 병기(Stage) 분포 비교",
    x = "암 병기 (Stage)",
    y = "비율",
    fill = "연령 그룹"
  ) +
  theme_minimal()

# 그래프 저장
ggsave(
  file.path(output_dir, "01_stage_distribution_by_age_group.png"),
  plot = stage_dist_plot,
  width = 8, height = 6
)

cat("\n--- 2.1. 그룹별 병기 분포 ---\n")
print(prop.table(table(train_df$age_group, train_df$기본환자병기값), margin = 1))

# 3.2. 치료 시행률 비교 (수술, 항암, 방사선)
# --------------------------------------------
treatment_rate_plot <- train_df %>% 
  select(age_group, 수술여부, 항암치료여부, 방사선치료여부) %>% 
  pivot_longer(
    cols = -age_group,
    names_to = "treatment_type",
    values_to = "status"
  ) %>% 
  mutate(treatment_type = factor(treatment_type, levels = c("수술여부", "항암치료여부", "방사선치료여부"))) %>% 
  ggplot(aes(x = age_group, fill = factor(status))) +
  geom_bar(position = "fill") +
  facet_wrap(~treatment_type) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "연령 그룹별 치료 시행률 비교",
    x = "연령 그룹",
    y = "비율",
    fill = "시행 여부 (1=Yes, 0=No)"
  ) +
  theme_minimal()

# 그래프 저장
ggsave(
  file.path(output_dir, "02_treatment_rate_by_age_group.png"),
  plot = treatment_rate_plot,
  width = 12, height = 6
)

cat("\n\n--- 2.2. 그룹별 치료 시행률 ---\n")
treatment_summary <- train_df %>% 
  select(age_group, 수술여부, 항암치료여부, 방사선치료여부) %>% 
  pivot_longer(
    cols = -age_group,
    names_to = "treatment_type",
    values_to = "status"
  ) %>% 
  group_by(age_group, treatment_type) %>% 
  summarise(
    rate = mean(status, na.rm = TRUE)
  ) %>% 
  pivot_wider(
    names_from = treatment_type,
    values_from = rate
  )
print(treatment_summary)

# 3.3. 분자병리 진단율 비교 (MSI, KRAS)
# --------------------------------------
# 검사 시행 여부를 나타내는 변수 생성
mol_path_df <- train_df %>% 
  mutate(
    MSI_tested = !is.na(MSI_STATUS),
    KRAS_tested = !is.na(KRAS_MUTATION)
  )

mol_path_plot <- mol_path_df %>% 
  select(age_group, MSI_tested, KRAS_tested) %>% 
  pivot_longer(
    cols = -age_group,
    names_to = "test_type",
    values_to = "is_tested"
  ) %>% 
  ggplot(aes(x = age_group, fill = is_tested)) +
  geom_bar(position = "fill") +
  facet_wrap(~test_type) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "연령 그룹별 분자병리 검사 시행률 비교",
    x = "연령 그룹",
    y = "비율",
    fill = "검사 시행 여부"
  ) +
  theme_minimal()

# 그래프 저장
ggsave(
  file.path(output_dir, "03_mol_path_testing_rate_by_age_group.png"),
  plot = mol_path_plot,
  width = 10, height = 6
)

cat("\n\n--- 2.3. 그룹별 분자병리 진단율 ---\n")
mol_path_summary <- mol_path_df %>% 
  select(age_group, MSI_tested, KRAS_tested) %>% 
  pivot_longer(
    cols = -age_group,
    names_to = "test_type",
    values_to = "is_tested"
  ) %>% 
  group_by(age_group, test_type) %>% 
  summarise(
    rate = mean(is_tested, na.rm = TRUE)
  ) %>% 
  pivot_wider(
    names_from = test_type,
    values_from = rate
  )
print(mol_path_summary)

# 4. 변수 간 관계 탐색
# -----------------------------------------------------------------------------
cat("\n--- 변수 간 관계 탐색 시작 ---\n")

# 4.1. 수치형 변수 간 상관 분석
# -------------------------------
# 분석에 사용할 수치형 변수 선택
numeric_vars_corr <- train_df %>% 
  select(기본환자진단시연령, 진단시점_CEA, 최고_CEA, 전체생존기간일수, 수술횟수, 항암치료횟수, 방사선치료횟수) %>% 
  na.omit() # 결측치 있는 행 제거

# 상관계수 행렬 계산
corr_matrix <- round(cor(numeric_vars_corr), 2)

# 상관계수 히트맵 생성
corr_plot <- ggcorrplot(
  corr_matrix,
  hc.order = TRUE,
  type = "lower",
  lab = TRUE, # 상관계수 값 표시
  lab_size = 3,
  method = "circle",
  colors = c("tomato", "white", "steelblue"),
  title = "주요 수치형 변수 간 상관관계 히트맵"
) 

# 그래프 저장
ggsave(
  file.path(output_dir, "04_correlation_heatmap_numeric_vars.png"),
  plot = corr_plot,
  width = 10, height = 8
)

cat("\n\n--- 4.1. 수치형 변수 간 상관 분석 ---\n")
print(corr_matrix)

# 4.2. 병기별 CEA 수치 분포
# ---------------------------
# 진단시점_CEA가 1000 이상인 값은 이상치로 간주하고 제외하여 시각화
cea_boxplot <- train_df %>% 
  filter(!is.na(기본환자병기값) & !is.na(진단시점_CEA) & 진단시점_CEA < 1000) %>% 
  ggplot(aes(x = factor(기본환자병기값, levels = c("0", "1", "2", "3", "4")), y = 진단시점_CEA, fill = 기본환자병기값)) +
  geom_boxplot() +
  labs(
    title = "암 병기별 진단 시점 CEA 수치 분포",
    x = "암 병기 (Stage)",
    y = "진단 시점 CEA 수치 (ng/mL)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# 그래프 저장
ggsave(
  file.path(output_dir, "05_cea_by_stage_boxplot.png"),
  plot = cea_boxplot,
  width = 10, height = 6
)

cat("\n\n--- 4.2. 병기별 CEA 수치 요약 ---\n")
cea_summary <- train_df %>% 
  filter(!is.na(기본환자병기값) & !is.na(진단시점_CEA) & 진단시점_CEA < 1000) %>% 
  group_by(기본환자병기값) %>% 
  summarise(
    n = n(),
    mean_CEA = mean(진단시점_CEA, na.rm = TRUE),
    median_CEA = median(진단시점_CEA, na.rm = TRUE),
    sd_CEA = sd(진단시점_CEA, na.rm = TRUE)
  )
print(cea_summary)

# 5. 생존분석 초석 EDA
# -----------------------------------------------------------------------------
cat("\n--- 생존 분석 EDA 시작 ---\n")

# 생존 분석을 위한 데이터 준비 (결측치 제거 및 타입 변환)
surv_df <- train_df %>%
  filter(!is.na(전체생존기간일수) & !is.na(사망여부)) %>%
  mutate(
    # '사망여부'를 안정적으로 숫자형 0/1로 변환
    # 값이 '1'이면 1, 그 외에는 0으로 처리하여 NA 발생 방지
    사망여부 = case_when(
      as.character(사망여부) == "1" ~ 1,
      TRUE ~ 0
    )
  )

# 5.1. Kaplan-Meier 생존 곡선 (전체, 연령 그룹, 병기)
# -----------------------------------------------------
# 생존 객체 생성
surv_obj <- Surv(time = surv_df$전체생존기간일수, event = surv_df$사망여부)

# 1) 전체 그룹 생존 곡선
fit_total <- survfit(surv_obj ~ 1, data = surv_df)
km_total_plot <- ggsurvplot(
  fit_total,
  data = surv_df,

  risk.table = TRUE,
  title = "전체 환자 생존 곡선 (Kaplan-Meier)",
  xlab = "시간 (일)",
  ylab = "생존 확률"
)

ggsave(
  file.path(output_dir, "06_km_curve_total.png"),
  print(km_total_plot),
  width = 10, height = 8
)

# 2) 연령 그룹별 생존 곡선
fit_age_group <- survfit(surv_obj ~ age_group, data = surv_df)
km_age_plot <- ggsurvplot(
  fit_age_group,
  data = surv_df,
  pval = TRUE,
  risk.table = TRUE,
  legend.title = "연령 그룹",
  legend.labs = c("EOCRC", "LOCRC"),
  title = "연령 그룹별 생존 곡선 (Kaplan-Meier)",
  xlab = "시간 (일)",
  ylab = "생존 확률"
)

ggsave(
  file.path(output_dir, "07_km_curve_by_age_group.png"),
  print(km_age_plot),
  width = 10, height = 8
)

# 3) 병기별 생존 곡선
surv_df_stage <- surv_df %>% filter(!is.na(기본환자병기값))
fit_stage <- survfit(Surv(전체생존기간일수, 사망여부) ~ 기본환자병기값, data = surv_df_stage)
km_stage_plot <- ggsurvplot(
  fit_stage,
  data = surv_df_stage,
  pval = TRUE,
  risk.table = TRUE,
  legend.title = "암 병기",
  title = "병기별 생존 곡선 (Kaplan-Meier)",
  xlab = "시간 (일)",
  ylab = "생존 확률"
)

ggsave(
  file.path(output_dir, "08_km_curve_by_stage.png"),
  print(km_stage_plot),
  width = 12, height = 9
)

cat("\n\n--- 5. 생존 분석 요약 ---\n")

cat("\n### 5.1 전체 생존 곡선 요약 (중앙 생존 기간 등) ###\n")
print(fit_total)

cat("\n\n--- 5.1. 연령 그룹별 생존 분석 (Log-rank test) ---\n")
print(fit_age_group)

cat("\n\n--- 5.2. 병기별 생존 분석 (Log-rank test) ---\n")
print(fit_stage)

# --- 파일 출력 종료 ---
sink()
cat(paste("분석 요약 파일이", summary_file, "에 저장되었습니다.\n"))
