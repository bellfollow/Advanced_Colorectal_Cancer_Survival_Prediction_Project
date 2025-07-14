# 00_setup.R
# 프로젝트에 필요한 패키지를 설치하고 로드합니다.

# 필요한 패키지 목록
required_packages <- c("tidyverse", "conflicted")

# 설치되지 않은 패키지 확인 및 설치
new_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(new_packages) > 0) {
  install.packages(new_packages)
}

# 패키지 로드
library(tidyverse)
library(conflicted)

# 충돌 해결: dplyr의 함수를 우선 사용하도록 설정
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

cat("패키지 준비 완료.\n")
