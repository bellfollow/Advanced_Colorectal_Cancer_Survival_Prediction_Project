# 01_feature_engineering.R
# 다기관 임상 데이터 피처 엔지니어링 스크립트

# 0. 환경 설정
source("scripts/00_setup.R")

# 1. 헬퍼 함수 정의

# 안전하게 파일을 읽는 함수 (인코딩 처리 포함)
read_data <- function(path, data_path) {
  if (file.exists(path)) {
    # 연세대학교병원 특정 파일에만 CP949 인코딩 적용
    encoding <- if (stringr::str_detect(data_path, "연세대학교병원") && stringr::str_detect(path, "clrc_trtm_rd.csv")) "CP949" else "UTF-8"
    readr::read_csv(path, col_types = readr::cols(.default = "c"), locale = readr::locale(encoding = encoding))
  } else {
    NULL
  }
}

# 진단검사 데이터 처리 함수
process_lab_data <- function(file_map, base_data, data_path) {
  lab_data_path <- file_map[["clrc_ex_diag.csv"]]
  if (is.null(lab_data_path) || !file.exists(lab_data_path)) return(base_data)

  lab_data <- read_data(lab_data_path, data_path) %>% 
    # 기관별로 컬럼명이 다를 수 있으므로, 존재하는 컬럼을 동적으로 선택
    dplyr::rename_with(~.x, .cols = dplyr::any_of(c("진단검사일자", "진단검사처방일자")), .fn = ~ "검사일자") %>%
    dplyr::rename_with(~.x, .cols = dplyr::any_of(c("진단검사코드", "진단검사명")), .fn = ~ "검사코드명") %>%
    dplyr::select(환자대체번호, 검사일자, 검사코드명, 진단검사결과내용)

  # 날짜 및 숫자형 변환, 주요 검사 필터링
  lab_data <- lab_data %>%
    dplyr::mutate(
      검사일 = lubridate::ymd(검사일자),
      결과값 = as.numeric(진단검사결과내용)
    ) %>%
    # 검사코드 또는 검사명으로 필터링 (CEA, CA19-9, Hb)
    dplyr::filter(!is.na(결과값), 
           검사코드명 %in% c("LBC00032", "LBC00035", "LBC00008") | 
           stringr::str_detect(검사코드명, "CEA|CA19-9|Hemoglobin"))

  # 진단일 기준 검사값 추출을 위해 base_data의 진단일 정보 가져오기
  patient_diag_dates <- base_data %>% dplyr::select(환자대체번호, 기본환자최초진단일자) %>% dplyr::mutate(진단일 = lubridate::ymd(기본환자최초진단일자))

  # 진단일 ±30일 이내 검사값
  diag_lab_features <- lab_data %>%
    dplyr::left_join(patient_diag_dates, by = "환자대체번호") %>%
    dplyr::filter(abs(as.numeric(difftime(검사일, 진단일, units = "days"))) <= 30) %>%
    dplyr::group_by(환자대체번호, 검사코드명) %>%
    dplyr::summarise(진단시점_결과 = mean(결과값, na.rm = TRUE), .groups = 'drop') %>%
    tidyr::pivot_wider(names_from = 검사코드명, values_from = 진단시점_결과, names_prefix = "진단시점_")

  # 전체 기간 최대값
  max_lab_features <- lab_data %>%
    dplyr::group_by(환자대체번호, 검사코드명) %>%
    dplyr::summarise(최고값 = max(결과값, na.rm = TRUE), .groups = 'drop') %>%
    tidyr::pivot_wider(names_from = 검사코드명, values_from = 최고값, names_prefix = "최고_")

  # 기본 데이터에 병합
  base_data <- base_data %>%
    dplyr::left_join(diag_lab_features, by = "환자대체번호") %>%
    dplyr::left_join(max_lab_features, by = "환자대체번호")
  
  cat("Processed lab data and merged features.\n")
  return(base_data)
}

# 수술 데이터 처리 함수
process_surgery_data <- function(file_map, base_data, data_path) {
  surgery_data_path <- file_map[["clrc_oprt_nfrm.csv"]]
  if (is.null(surgery_data_path) || !file.exists(surgery_data_path)) return(base_data)

  surgery_data <- read_data(surgery_data_path, data_path) %>%
    dplyr::rename_with(~.x, .cols = dplyr::any_of(c("수술일자", "대장암수술일자")), .fn = ~ "수술일자_표준") %>%
    dplyr::rename_with(~.x, .cols = dplyr::any_of(c("수술대장암수술종류명", "대장암수술명")), .fn = ~ "수술명_표준") %>%
    dplyr::select(환자대체번호, 수술일자_표준, 수술명_표준) %>%
    dplyr::mutate(수술일 = lubridate::ymd(수술일자_표준)) %>%
    dplyr::filter(!is.na(수술일))

  # 수술 횟수 계산
  surgery_counts <- surgery_data %>%
    dplyr::group_by(환자대체번호) %>%
    dplyr::summarise(수술횟수 = n(), .groups = 'drop')

  # 첫 수술 정보 추출 (진단일 이후)
  patient_diag_dates <- base_data %>% dplyr::select(환자대체번호, 기본환자최초진단일자) %>% dplyr::mutate(진단일 = lubridate::ymd(기본환자최초진단일자))
  
  first_surgery_features <- surgery_data %>%
    dplyr::left_join(patient_diag_dates, by = "환자대체번호") %>%
    dplyr::filter(수술일 >= 진단일) %>%
    dplyr::arrange(환자대체번호, 수술일) %>%
    dplyr::group_by(환자대체번호) %>%
    dplyr::summarise(
      첫수술일자 = dplyr::first(수술일),
      첫수술명 = dplyr::first(수술명_표준),
      .groups = 'drop'
    ) %>% 
    dplyr::mutate(수술여부 = "Y")

  # 기본 데이터에 병합
  base_data <- base_data %>%
    dplyr::left_join(surgery_counts, by = "환자대체번호") %>%
    dplyr::left_join(first_surgery_features, by = "환자대체번호") %>%
    dplyr::mutate(
      수술여부 = ifelse(is.na(수술여부), "N", 수술여부),
      수술횟수 = ifelse(is.na(수술횟수), 0, 수술횟수)
    )
  
  cat("Processed surgery data and merged features.\n")
  return(base_data)
}

# 항암/방사선 치료 데이터 처리 함수
process_treatment_data <- function(file_map, base_data, data_path) {
  # --- 항암 치료 (Chemotherapy) ---
  chemo_data_path <- file_map[["clrc_trtm_casb.csv"]]
  if (!is.null(chemo_data_path) && file.exists(chemo_data_path)) {
    chemo_data <- read_data(chemo_data_path, data_path) %>%
      dplyr::rename_with(~.x, .cols = dplyr::any_of(c("항암제치료시작일자", "항암치료시작일자")), .fn = ~ "치료시작일_표준") %>%
      dplyr::rename_with(~.x, .cols = dplyr::any_of(c("항암제치료요법명", "항암치료요법명")), .fn = ~ "치료요법명_표준") %>%
      dplyr::select(환자대체번호, 치료시작일_표준, 치료요법명_표준) %>%
      dplyr::mutate(치료시작일 = lubridate::ymd(치료시작일_표준)) %>%
      dplyr::filter(!is.na(치료시작일))

    chemo_features <- chemo_data %>%
      dplyr::group_by(환자대체번호) %>%
      dplyr::summarise(
        최초항암치료일 = min(치료시작일, na.rm = TRUE),
        항암치료횟수 = n(),
        FOLOFOX_여부 = dplyr::if_else(any(stringr::str_detect(치료요법명_표준, regex("FOLF(OX|IRI)", ignore_case = TRUE))), "Y", "N"),
        CAPEOX_여부 = dplyr::if_else(any(stringr::str_detect(치료요법명_표준, regex("CAPEOX|XELOX", ignore_case = TRUE))), "Y", "N"),
        .groups = 'drop'
      ) %>%
      dplyr::mutate(항암치료여부 = "Y")

    base_data <- base_data %>%
      dplyr::left_join(chemo_features, by = "환자대체번호")
    
    cat("Processed chemotherapy data and merged features.\n")
  }

  # --- 방사선 치료 (Radiotherapy) ---
  radio_data_path <- file_map[["clrc_trtm_rd.csv"]]
  if (!is.null(radio_data_path) && file.exists(radio_data_path)) {
    radio_data <- read_data(radio_data_path, data_path) %>%
      dplyr::select(환자대체번호, 방사선치료시작일자) %>%
      dplyr::mutate(치료시작일 = lubridate::ymd(방사선치료시작일자)) %>%
      dplyr::filter(!is.na(치료시작일))

    radio_features <- radio_data %>%
      dplyr::group_by(환자대체번호) %>%
      dplyr::summarise(
        최초방사선치료일 = min(치료시작일, na.rm = TRUE),
        방사선치료횟수 = n(),
        .groups = 'drop'
      ) %>%
      dplyr::mutate(방사선치료여부 = "Y")

    base_data <- base_data %>%
      dplyr::left_join(radio_features, by = "환자대체번호")
      
    cat("Processed radiotherapy data and merged features.\n")
  }

  # 결측치 처리
  base_data <- base_data %>%
    dplyr::mutate(
      항암치료여부 = ifelse(is.na(항암치료여부), "N", 항암치료여부),
      항암치료횟수 = ifelse(is.na(항암치료횟수), 0, 항암치료횟수),
      FOLOFOX_여부 = ifelse(is.na(FOLOFOX_여부), "N", FOLOFOX_여부),
      CAPEOX_여부 = ifelse(is.na(CAPEOX_여부), "N", CAPEOX_여부),
      방사선치료여부 = ifelse(is.na(방사선치료여부), "N", 방사선치료여부),
      방사선치료횟수 = ifelse(is.na(방사선치료횟수), 0, 방사선치료횟수)
    )

  return(base_data)
}

# 병리 데이터 처리 함수
process_pathology_data <- function(file_map, base_data, data_path) {
  # 동적으로 병리 파일 경로 찾기
  molecular_path_key <- names(file_map)[stringr::str_detect(names(file_map), "mlcr|invs")]
  ihc_path_key <- names(file_map)[stringr::str_detect(names(file_map), "mnty|brst")]

  # 데이터 프레임 초기화
  kras_status <- tibble(환자대체번호 = integer(0), KRAS_MUTATION = character(0))
  msi_status <- tibble(환자대체번호 = integer(0), MSI_STATUS = character(0))
  msi_molecular <- tibble(환자대체번호 = integer(0), MSI_STATUS_MOLECULAR = character(0))
  msi_ihc <- tibble(환자대체번호 = integer(0), MSI_STATUS_IHC = character(0))

  # 분자병리 데이터 처리 (KRAS, MSI)
  if (length(molecular_path_key) > 0 && file.exists(file_map[[molecular_path_key]])) {
    molecular_data_raw <- read_data(file_map[[molecular_path_key]], data_path)

    kras_cols <- names(molecular_data_raw)[stringr::str_detect(names(molecular_data_raw), "KRAS.*결과명")]
    msi_cols <- names(molecular_data_raw)[stringr::str_detect(names(molecular_data_raw), "MSI.*결과명")]

    molecular_data <- molecular_data_raw %>% 
      dplyr::mutate(
        KRAS_RESULT = if(length(kras_cols) > 0) coalesce(!!!syms(kras_cols)) else NA_character_,
        MSI_RESULT = if(length(msi_cols) > 0) coalesce(!!!syms(msi_cols)) else NA_character_
      ) %>% 
      dplyr::select(환자대체번호, dplyr::any_of(c("KRAS_RESULT", "MSI_RESULT")))

    if ("KRAS_RESULT" %in% names(molecular_data)) {
        kras_status <- molecular_data %>%
            dplyr::filter(!is.na(KRAS_RESULT)) %>%
            dplyr::group_by(환자대체번호) %>%
            dplyr::summarise(KRAS_MUTATION = dplyr::if_else(any(stringr::str_detect(KRAS_RESULT, "Mutation|Positive")), "Y", "N"), .groups = 'drop')
    }
    if ("MSI_RESULT" %in% names(molecular_data)) {
        msi_molecular <- molecular_data %>%
            dplyr::filter(!is.na(MSI_RESULT)) %>%
            dplyr::group_by(환자대체번호) %>%
            dplyr::summarise(MSI_STATUS_MOLECULAR = dplyr::first(MSI_RESULT), .groups = 'drop')
    }
  }

  # 면역조직화학검사 데이터 처리 (MSI)
  if (length(ihc_path_key) > 0 && file.exists(file_map[[ihc_path_key]])) {
    ihc_data <- read_data(file_map[[ihc_path_key]], data_path) %>%
      dplyr::rename_with(~"MLH1", .cols = matches("H?MLH1.*명")) %>%
      dplyr::rename_with(~"MSH2", .cols = matches("H?MSH2.*명")) %>%
      dplyr::rename_with(~"MSH6", .cols = matches("H?MSH6.*명")) %>%
      dplyr::rename_with(~"PMS2", .cols = matches("H?PMS2.*명")) %>%
      dplyr::select(환자대체번호, dplyr::any_of(c("MLH1", "MSH2", "MSH6", "PMS2")))

    if (ncol(ihc_data) > 1) {
        msi_ihc <- ihc_data %>%
            pivot_longer(cols = -환자대체번호, names_to = "marker", values_to = "status") %>%
            dplyr::filter(!is.na(status)) %>%
            dplyr::group_by(환자대체번호) %>%
            dplyr::summarise(MSI_IHC_DEFECT = any(stringr::str_detect(status, "결손|Loss")), .groups = 'drop') %>%
            dplyr::mutate(MSI_STATUS_IHC = dplyr::if_else(MSI_IHC_DEFECT, "MSI-H", "MSS"))
    }
  }

  # MSI 정보 통합 (분자병리 우선)
  if (nrow(msi_molecular) > 0 || nrow(msi_ihc) > 0) {
      msi_status <- full_join(msi_molecular, msi_ihc, by = "환자대체번호") %>%
          dplyr::mutate(MSI_STATUS = coalesce(MSI_STATUS_MOLECULAR, MSI_STATUS_IHC)) %>%
          dplyr::select(환자대체번호, MSI_STATUS)
  }

  # 최종 병합
  base_data <- base_data %>%
    dplyr::left_join(kras_status, by = "환자대체번호") %>%
    dplyr::left_join(msi_status, by = "환자대체번호")

  cat("Processed pathology data and merged features.\n")
  return(base_data)
}

# 사망 정보 처리 함수
process_death_data <- function(file_map, base_data, data_path) {
  death_data_path <- file_map[["clrc_dead_nfrm.csv"]]
  if (is.null(death_data_path) || !file.exists(death_data_path)) {
    cat("Death data not found, calculating survival from base data.\n")
  } 

  death_data <- read_data(death_data_path, data_path)
  if (!is.null(death_data)) {
      death_info <- death_data %>%
        dplyr::select(환자대체번호, 사망일자) %>%
        dplyr::mutate(
          사망여부_확인 = "Y",
          사망일자_확인 = lubridate::ymd(사망일자)
        ) %>%
        dplyr::select(환자대체번호, 사망여부_확인, 사망일자_확인)

      base_data <- base_data %>% dplyr::left_join(death_info, by = "환자대체번호")
  } else {
      base_data <- base_data %>% dplyr::mutate(사망여부_확인 = NA, 사망일자_확인 = NA)
  }

  # 최종 생존 정보 생성
  base_data <- base_data %>%
    dplyr::mutate(
      사망여부 = dplyr::if_else(!is.na(사망여부_확인) | !is.na(기본환자사망일자), "Y", "N"),
      최종사망일 = coalesce(사망일자_확인, lubridate::ymd(기본환자사망일자)),
      생존기간_일 = as.numeric(difftime(
        dplyr::if_else(사망여부 == "Y", 최종사망일, lubridate::ymd(센터최종방문일자)),
        lubridate::ymd(기본환자최초진단일자),
        units = "days"
      ))
    ) %>% 
    dplyr::select(-dplyr::any_of(c("사망여부_확인", "사망일자_확인")))

  cat("Processed death data and calculated survival outcomes.\n")
  return(base_data)
}

# 2. 메인 피처 엔지니어링 함수
create_features <- function(hospital_name, data_path, output_filename) {
  cat(paste("\n--- Feature Engineering for:", hospital_name, "---\n"))

  # 파일 경로 맵 생성
  file_list <- list.files(data_path, pattern = "*.csv", full.names = TRUE)
  file_map <- setNames(file_list, basename(file_list))

  # 1. 기본 테이블 로드 (환자 정보)
  base_data <- read_data(file_map[["clrc_pt_bsnf.csv"]], data_path)
  if (is.null(base_data)) {
    cat("Error: Base patient data 'clrc_pt_bsnf.csv' not found.\n")
    return(NULL)
  }
  cat(paste("Loaded base data with", nrow(base_data), "patients.\n"))

  # 각 데이터 유형별 처리 함수 호출
  base_data <- process_lab_data(file_map, base_data, data_path)
  base_data <- process_surgery_data(file_map, base_data, data_path)
  base_data <- process_treatment_data(file_map, base_data, data_path)
  base_data <- process_pathology_data(file_map, base_data, data_path)
  base_data <- process_death_data(file_map, base_data, data_path)

  # 기관별 데이터 형식 통일: NCC 생년월일 형식 변경 (YYYY-MM -> YYYYMM)
  if (hospital_name == "국립암센터") {
    # 컬럼 이름에 보이지 않는 문자가 있을 가능성에 대비하여, 패턴 매칭으로 컬럼을 찾아 강제로 rename 후 처리
    if (any(stringr::str_detect(names(base_data), "기본환자생년월"))) {
      base_data <- base_data %>%
        dplyr::rename_with(~"기본환자생년월", .cols = matches("기본환자생년월")) %>%
        dplyr::mutate(
          기본환자생년월 = ifelse(
            stringr::str_detect(기본환자생년월, "-"),
            {
              parts <- stringr::str_split_fixed(기본환자생년월, "-", 2)
              year_short <- as.numeric(parts[, 2])
              year_full <- 1900 + year_short
              month_num <- match(parts[, 1], month.abb)
              sprintf("%d-%02d", year_full, month_num)
            },
            기본환자생년월
          )
        )
      cat("Normalized NCC birth date format.\n")
    } else {
      cat("Warning: '기본환자생년월' column not found for NCC data normalization.\n")
    }
  }

  # 분석을 위해 '여부' 컬럼들을 숫자(1/0)로 변환 (존재하는 경우에만)
  cols_to_convert <- c("수술여부", "항암치료여부", "방사선치료여부", "KRAS_tested", "MSI_tested")
  base_data <- base_data %>% 
    dplyr::mutate(dplyr::across(dplyr::any_of(cols_to_convert), ~ as.integer(.x == "Y")))

  # 최종 데이터 저장
  output_path <- file.path("data", "processed", output_filename)
  write_csv(base_data, output_path)
  cat(paste("\nProcessed data saved to:", output_path, "\n"))
  cat(paste("Final dimensions:", nrow(base_data), "rows,", ncol(base_data), "columns\n"))

  return(base_data)
}

# 3. 각 병원 데이터 처리 실행
hospitals <- list(
  list(name = "국립암센터", path = "data/raw/Clrc data (국립암센터)", out = "ncc_clrc_features.csv"),
  list(name = "삼성서울병원", path = "data/raw/Clrc data (삼성서울병원)", out = "smc_clrc_features.csv"),
  list(name = "연세대학교병원", path = "data/raw/Clrc data (연세대학교병원)", out = "yonsei_clrc_features.csv")
)

all_processed_data <- purrr::map(hospitals, ~create_features(
  hospital_name = .x$name,
  data_path = .x$path,
  output_filename = .x$out
))

cat("\n--- All feature engineering finished. ---\n")

# 4. 모든 병원 데이터 통합 및 저장
# ------------------------------------------------------------------------------
cat("\n--- 4. 통합 데이터셋 생성 시작 ---\n")

# 결과 저장 폴더 생성
preprocessed_dir <- "data/preprocessed"
dir.create(preprocessed_dir, showWarnings = FALSE, recursive = TRUE)

# 리스트에 있는 모든 데이터프레임을 하나로 합치기
total_df <- dplyr::bind_rows(all_processed_data)

# 최종 통합 파일 저장
total_output_path <- file.path(preprocessed_dir, "total_preprocessed.csv")
write_csv(total_df, total_output_path)

cat(paste("\n통합 데이터가 다음 경로에 저장되었습니다:", total_output_path, "\n"))
cat(paste("최종 통합 데이터 차원:", nrow(total_df), "행,", ncol(total_df), "열\n"))
cat("\n--- 모든 전처리 과정 완료 ---\n")

