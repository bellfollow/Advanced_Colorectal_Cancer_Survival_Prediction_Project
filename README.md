# 대장암 생존 예후 분석 프로젝트

본 프로젝트는 다기관 대장암 임상 데이터를 통합 분석하여, 환자의 생존 예후에 영향을 미치는 주요 인자를 규명하는 것을 목표로 합니다. 특히 조기 발병(EOCRC, 50세 이하)과 후기 발병(LOCRC, 50세 초과) 대장암의 임상적 특성과 예후 인자를 비교 분석하는 데 중점을 둡니다.
- Python과 R을 섞어 만드려고 하였으나 Python의 ML과정에 맞는 추가적인 데이터 전처리가 필요해 R의 통합으로 진행하는 것이 좋아보여 R과 R_shiny로 만들었습니다. 
## 📊 주요 분석 결과

- **병기(Tumor Stage)**: 모든 환자 그룹에서 생존 예후를 결정하는 가장 압도적인 요인입니다. (4기 vs 0기, 사망 위험 약 6.5배)
- **수술(Surgery)**: 가장 일관되고 강력한 생존 보호 인자로, 모든 그룹에서 사망 위험을 크게 감소시켰습니다.
- **조기 발병(EOCRC) vs 후기 발병(LOCRC) 특성**: 
  - **EOCRC**: `성별(남성)`이 뚜렷한 생존 이점(사망 위험 36% 감소)을 보였으나, `나이` 자체는 예후에 큰 영향을 주지 않았습니다.
  - **LOCRC**: `나이`가 유의미한 위험 인자였으며, `진단 시 CEA 수치` 또한 중요한 예후 예측 인자였습니다.
- **치료의 역설 규명**: 병기 보정 전 항암/방사선 치료가 사망 위험을 높이는 것처럼 보였던 현상은, 진행성 암 환자들이 해당 치료를 더 많이 받는 데서 기인한 **'교란 효과(Confounding effect)'** 임을 통계적으로 확인했습니다.

## 🚀 시작하기

### 사전 요구사항

- R (version 4.2 이상)
- RStudio (권장)

### 설치 및 설정

1.  **저장소 복제**
2.  **데이터 배치**: 원본 CSV 파일들을 `data/raw/` 폴더 내 기관별 하위 폴더에 배치합니다.
3.  **패키지 설치**: R 콘솔에서 아래 명령어를 실행하여 필요한 모든 패키지를 설치합니다.
    ```R
    if (!require("pacman")) install.packages("pacman")
    pacman::p_load(tidyverse, caret, survival, survminer, ggcorrplot, gtsummary, conflicted)
    ```

### 🔬 분석 파이프라인 실행

`scripts/` 폴더의 R 스크립트를 아래 순서대로 실행하여 전체 데이터 처리 및 분석 과정을 재현할 수 있습니다.

```R
# R 콘솔 또는 RStudio에서 실행
source("scripts/01_data_preprocessing.R")
source("scripts/02_split_and_eda.R")
source("scripts/03_advanced_eda.R")
source("scripts/04_univariate_analysis.R")
source("scripts/05_multivariate_analysis.R")
```

## 📂 프로젝트 구조

```
.
├── data/                 # 원본, 중간, 최종 데이터 (Git 관리 제외)
├── docs/                 # 분석 단계별 상세 문서
│   └── *.md              # 각 분석 과정 요약 문서
├── results/              # 분석 결과물 (CSV, plot 등, Git 관리 제외)
├── scripts/              # 분석 R 스크립트
└── README.md             # 프로젝트 개요 및 가이드
```

## 📜 스크립트 개요

- **`01_data_preprocessing.R`**: 여러 기관의 원본 데이터를 통합하고 정제하여 분석용 데이터셋을 생성합니다. ([문서](./docs/01_data_preprocessing_documentation.md))
- **`02_split_and_eda.R`**: 데이터를 훈련/검증용으로 분할하고 기초 탐색적 데이터 분석(EDA)을 수행합니다. ([문서](./docs/02_split_and_eda_documentation.md))
- **`03_advanced_eda.R`**: 카플란-마이어 생존 곡선 등 심층 EDA를 수행합니다. ([문서](./docs/03_advanced_eda_summary.md))
- **`04_univariate_analysis.R`**: 단변량 콕스 회귀 분석을 통해 각 변수의 독립적인 예후 연관성을 평가합니다. ([문서](./docs/04_univariate_analysis_summary.md))
- **`05_multivariate_analysis.R`**: 다변량 콕스 회귀 분석을 통해 여러 변수를 보정한 상태에서의 핵심 예후 인자를 도출합니다. ([문서](./docs/05_multivariate_analysis_total.md))
