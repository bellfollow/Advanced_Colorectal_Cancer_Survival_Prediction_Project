# 02_split_and_eda.R: 데이터 분할 및 기초 탐색적 데이터 분석

## 1. 스크립트 개요

`02_split_and_eda.R` 스크립트는 `01_data_preprocessing.R`에서 생성된 통합 전처리 데이터를 입력받아 모델링을 위한 데이터셋을 구축하고, 기초적인 탐색적 데이터 분석(EDA)을 수행합니다.

## 2. 주요 기능

### 2.1. 데이터 분할 (Data Splitting)

-   **입력 데이터**: `data/preprocessed/total_preprocessed.csv`
-   **작업 내용**:
    1.  전체 데이터를 불러온 후, 진단 시 연령을 기준으로 **조기 발병(EOCRC, 50세 미만)** 및 **후기 발병(LOCRC, 50세 이상)** 그룹으로 나눕니다.
    2.  `total`, `EOCRC`, `LOCRC` 세 가지 데이터셋 각각에 대해 `사망여부`를 기준으로 층화추출(stratified sampling)을 적용하여 **훈련 데이터(80%)**와 **검증 데이터(20%)**로 분할합니다.
-   **출력 데이터**:
    -   `data/modeling_datasets/total/`
    -   `data/modeling_datasets/eocrc/`
    -   `data/modeling_datasets/locrc/`
    -   각 폴더 내에 `_train.csv`와 `_valid.csv` 파일 생성

### 2.2. 기초 탐색적 데이터 분석 (Basic EDA)

-   **대상 데이터**: `data/modeling_datasets/total/total_train.csv` (전체 훈련 데이터)
-   **분석 항목**:
    1.  **결측치 분석**: 데이터 내 변수별 결측치 비율을 시각화하여 확인합니다.
    2.  **이상치 분석**: 주요 수치형 변수(연령, CEA 수치, 생존기간 등)에 대한 박스플롯(Boxplot)을 생성하여 이상치 분포를 확인합니다.
    3.  **요약 통계**: 데이터의 기술 통계량(평균, 중앙값, 표준편차 등)을 계산하여 저장합니다.
-   **출력물**:
    -   결측치 및 이상치 분석 그래프 (`results/eda/` 폴더)
    -   요약 통계 텍스트 파일 (`results/eda/00_eda_summary.txt`)

## 3. 실행 흐름

1.  필요한 패키지(`pacman`, `tidyverse`, `caret` 등)를 로드합니다.
2.  `split_and_save` 함수를 정의하여 데이터 분할 및 저장을 자동화합니다.
3.  전체, EOCRC, LOCRC 데이터셋에 대해 `split_and_save` 함수를 실행합니다.
4.  `perform_basic_eda` 함수를 정의하여 기초 EDA 과정을 자동화합니다.
5.  전체 훈련 데이터를 대상으로 `perform_basic_eda` 함수를 실행하여 분석 결과를 저장합니다.
