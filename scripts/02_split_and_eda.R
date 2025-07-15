# ==============================================================================
# 02_split_and_eda.R
#
# 1. 데이터 분할 (전체, EOCRC, LOCRC)
# 2. 기초 탐색적 데이터 분석 (EDA) 수행
#
# 이 스크립트는 전처리된 데이터를 훈련/검증 세트로 분할하고,
# 생성된 훈련 데이터셋에 대한 기초 EDA를 수행하여 결과를 저장합니다.
# ==============================================================================

# 1. 설정 (Setup)
# ------------------------------------------------------------------------------
# 필요한 패키지 로드
options(repos = c(CRAN = "https://cran.rstudio.com/")) # CRAN 미러 설정
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, caret, naniar, DataExplorer, ggcorrplot, ggpubr, conflicted)

# 충돌 해결 우선순위 설정
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")


# 2. 데이터 분할 (Data Splitting)
# ------------------------------------------------------------------------------
cat("--- 1. 데이터 분할 시작 ---\n")

# --- 데이터 분할 및 저장 함수 ---
split_and_save <- function(df, dataset_name, output_dir) {
  target_dir <- file.path(output_dir, dataset_name)
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  
  set.seed(2023)
  
    train_indices <- caret::createDataPartition(df$사망여부, p = 0.8, list = FALSE)
  
  train_df <- df[train_indices, ]
  valid_df <- df[-train_indices, ]
  
  train_path <- file.path(target_dir, paste0(dataset_name, "_train.csv"))
  valid_path <- file.path(target_dir, paste0(dataset_name, "_valid.csv"))
  
    readr::write_csv(train_df, train_path)
    readr::write_csv(valid_df, valid_path)
  
  cat(paste("\n---", toupper(dataset_name), "데이터셋 저장 완료 ---\n"))
  cat(paste("  훈련 데이터:", train_path, "(", nrow(train_df), "명)\n"))
  cat(paste("  검증 데이터:", valid_path, "(", nrow(valid_df), "명)\n"))
}

# --- 전처리된 데이터 로드 ---
total_df <- read_csv("data/preprocessed/total_preprocessed.csv")
cat("\n전체 전처리 데이터 로드 완료.\n")

# --- 연령 기반 서브셋 생성 ---
eocrc_df <- total_df %>% filter(기본환자진단시연령 <= 50)
locrc_df <- total_df %>% filter(기본환자진단시연령 > 50)
cat("EOCRC 및 LOCRC 데이터셋 생성 완료.\n")

# --- 모든 데이터셋 분할 및 저장 실행 ---
output_base_dir <- "data/modeling_datasets"
split_and_save(total_df, "total", output_base_dir)
split_and_save(eocrc_df, "eocrc", output_base_dir)
split_and_save(locrc_df, "locrc", output_base_dir)

cat("\n--- 모든 데이터셋 분할 및 저장 완료 ---\n")


# 3. 기초 탐색적 데이터 분석 (Basic EDA)
# ------------------------------------------------------------------------------
cat("\n\n--- 2. 기초 EDA 시작 (전체 훈련 데이터 대상) ---\n")

# --- 이전 단계에서 생성된 훈련 데이터 로드 ---
train_df <- read_csv("data/modeling_datasets/total/total_train.csv")
cat("EDA를 위한 훈련 데이터 로드 완료.\n")

# --- 결과 저장 폴더 생성 ---
eda_output_dir <- "results/eda"
dir.create(eda_output_dir, showWarnings = FALSE, recursive = TRUE)

# --- 3.1. 결측치 분석 ---
cat("\n--- 결측치 분석 중... ---\n")
# gg_miss_upset()은 ggplot 객체가 아니므로 ggsave 대신 png 장치를 사용해 저장
png(
  file.path(eda_output_dir, "01_missing_value_upset_plot.png"),
  width = 10, height = 7, units = "in", res = 300
)
print(gg_miss_upset(train_df, nsets = 10))
dev.off()
cat("결측치 분석 그래프 저장 완료.\n")

# --- 3.2. 수치형 변수 이상치 분석 ---
cat("\n--- 수치형 변수 이상치 분석 중... ---\n")
# plot_boxplot은 ggplot 객체 리스트를 반환하므로, ggarrange로 합친 후 저장
outlier_plot_list <- plot_boxplot(
  train_df,
  by = "사망여부",
  geom_boxplot_args = list(outlier.color = "red", outlier.size = 1)
)

combined_outlier_plot <- ggarrange(plotlist = outlier_plot_list)

ggsave(
  file.path(eda_output_dir, "02_numeric_outlier_boxplot.png"),
  plot = combined_outlier_plot,
  width = 12, height = 9
)
cat("이상치 분석 박스플롯 저장 완료.\n")

# --- 3.3. 요약 통계 ---
cat("\n--- 요약 통계 생성 중... ---\n")
summary_file_path <- file.path(eda_output_dir, "00_summary_statistics.txt")
sink(summary_file_path)

cat("--- 수치형 변수 요약 ---\n")
numeric_vars <- c("기본환자진단시연령", "전체생존기간일수", "진단시점_CEA", "최고_CEA")
print(summary(train_df[numeric_vars]))

cat("\n\n--- 범주형 변수 빈도 분석 ---\n")
categorical_vars <- c("기본환자성별코드", "기본환자최초진단명", "기본환자병기값", "수술여부", "항암치료여부", "방사선치료여부", "사망여부")
for (var in categorical_vars) {
  if (var %in% names(train_df)) {
    cat(paste("\n###", var, "###\n"))
    print(table(train_df[[var]], useNA = "ifany"))
  }
}

# 파일 쓰기 종료
sink()
cat("요약 통계 파일 저장 완료.\n")

cat("\n--- 데이터 분할 및 EDA 분석 완료. 결과는 'results/eda' 폴더에 저장되었습니다. ---\n")
