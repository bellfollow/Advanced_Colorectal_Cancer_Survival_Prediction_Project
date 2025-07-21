# 대장암 생존 예측 프로젝트

본 프로젝트는 다기관의 대장암 임상 데이터를 통합하고 분석하여 환자의 생존 기간을 예측하는 모델을 개발하는 것을 목표로 합니다. 최종적으로는 환자 데이터를 입력받아 생존 예측 보고서를 자동 생성하는 시스템 구축을 지향합니다.

## 🚀 시작하기

### 사전 요구사항

- R (version 4.2 이상)
- RStudio (권장)

### 설치 및 설정

1.  **저장소 복제**:
    ```bash
    git clone <repository-url>
    cd <repository-name>
    ```

2.  **데이터 배치**:
    각 기관에서 받은 원본 CSV 파일들을 `data/raw/` 폴더 내에 기관별 하위 폴더를 만들어 배치합니다. (예: `data/raw/국립암센터/`)

3.  **패키지 설치**:
    프로젝트에 필요한 R 패키지를 설치합니다. R 콘솔에서 아래 명령어를 실행하세요.
    ```R
    if (!require("pacman")) install.packages("pacman")
    pacman::p_load(tidyverse, caret, survival, survminer, ggcorrplot, conflicted)
    ```

### 분석 파이프라인 실행

`scripts/` 폴더의 R 스크립트들을 아래 순서대로 실행하여 전체 데이터 처리 및 분석 과정을 재현할 수 있습니다.

```R
# R 콘솔 또는 RStudio에서 실행
source("scripts/01_data_preprocessing.R")
source("scripts/02_split_and_eda.R")
source("scripts/03_advanced_eda.R")
```

## 📂 프로젝트 구조

```
.
├── data/
│   ├── raw/                # 원본 데이터 (Git 관리 제외)
│   ├── processed/          # 기관별 전처리 데이터 (Git 관리 제외)
│   ├── preprocessed/       # 통합 전처리 데이터 (Git 관리 제외)
│   └── modeling_datasets/  # 모델링용 최종 데이터셋
├── docs/                   # 프로젝트 문서
│   ├── 01_data_preprocessing.md
│   ├── 02_split_and_eda.md
│   └── 03_advanced_eda.md
├── results/                # 분석 결과물 (Git 관리 제외)
│   ├── eda/
│   └── advanced_eda/
├── scripts/                # 분석 스크립트
│   ├── 01_data_preprocessing.R
│   ├── 02_split_and_eda.R
│   └── 03_advanced_eda.R
├── .gitignore
└── README.md
```

## 📜 스크립트 개요

-   **`01_data_preprocessing.R`**: 여러 기관의 원본 데이터를 로드하여 변수명을 통일하고, 임상 정보(진단, 수술, 치료, 병리 등)를 처리 및 결합하여 분석용 데이터셋을 생성합니다. 상세 내용은 [문서](./docs/01_data_preprocessing.md)를 참고하세요.
-   **`02_split_and_eda.R`**: 전처리된 데이터를 훈련(train)/검증(validation) 데이터로 분할하고, 기초 탐색적 데이터 분석(EDA)을 수행합니다. 상세 내용은 [문서](./docs/02_split_and_eda.md)를 참고하세요.
-   **`03_advanced_eda.R`**: 훈련 데이터를 사용하여 그룹 간 임상 특성 비교, 상관관계 분석, 카플란-마이어 생존 분석 등 심층 EDA를 수행합니다. 상세 내용은 [문서](./docs/03_advanced_eda.md)를 참고하세요.
