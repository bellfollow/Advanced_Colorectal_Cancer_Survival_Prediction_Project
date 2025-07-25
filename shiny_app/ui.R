# ==============================================================================
# UI Definition
# ==============================================================================
ui <- dashboardPage(
    dashboardHeader(title = "대장암 생존 예측 대시보드", titleWidth = 300),
    dashboardSidebar(
        sidebarMenu(
            menuItem("요약 대시보드 (Home)", tabName = "home", icon = icon("dashboard")),
            menuItem("환자군별 탐색 (EDA)", tabName = "eda", icon = icon("chart-bar")),
            menuItem("생존분석", tabName = "survival", icon = icon("heartbeat")),
            menuItem("생존예측 시나리오", tabName = "prediction", icon = icon("robot")),
            menuItem("변수 상관 및 인사이트", tabName = "insight", icon = icon("search")),
            menuItem("모델 성능/리포트", tabName = "performance", icon = icon("check-circle")),
            menuItem("데이터/코드 다운로드", tabName = "download", icon = icon("download"))
        )
    ),
    dashboardBody(
        tabItems(
            # 1. Home Tab
            tabItem(tabName = "home",
                h2("요약 대시보드: 전체 데이터 개요"),
                fluidRow(
                    infoBox("총 환자 수", total_patients, icon = icon("users"), color = "blue", width = 3),
                    infoBox("EOCRC 환자 (50세 미만)", paste0(eocrc_patients, " (", round(eocrc_patients/total_patients*100, 1), "%)"), icon = icon("user-plus"), color = "purple", width = 3),
                    infoBox("LOCRC 환자 (50세 이상)", paste0(locrc_patients, " (", round(locrc_patients/total_patients*100, 1), "%)"), icon = icon("user-friends"), color = "aqua", width = 3),
                    infoBox("사망자 수", paste0(death_count, " (", round(death_count/total_patients*100, 1), "%)"), icon = icon("cross"), color = "red", width = 3)
                ),
                fluidRow(
                    # --- New InfoBoxes for key metrics ---
                    infoBox("전체 중앙 생존기간", paste(median_survival_months, "개월"), icon = icon("calendar-check"), color = "green", width = 3),
                    infoBox("수술 시행률", paste0(treatment_rates$surgery, "%"), icon = icon("syringe"), color = "orange", width = 3),
                    infoBox("항암치료 시행률", paste0(treatment_rates$chemo, "%"), icon = icon("pills"), color = "yellow", width = 3),
                    infoBox("방사선치료 시행률", paste0(treatment_rates$radio, "%"), icon = icon("radiation"), color = "maroon", width = 3)
                ),
                fluidRow(
                    box(
                        title = "환자 그룹별 병기(Stage) 분포",
                        width = 6, # 너비 조정
                        solidHeader = TRUE,
                        status = "primary",
                        plotlyOutput("stage_distribution_plot")
                    ),
                    box(
                        title = "생존/사망 분포",
                        width = 6, # 너비 조정
                        solidHeader = TRUE,
                        status = "primary",
                        plotlyOutput("survival_status_pie_chart")
                    )
                ),
                fluidRow(
                    box(
                        title = "주요 변수 결측률 현황",
                        width = 12,
                        solidHeader = TRUE,
                        status = "warning",
                        DTOutput("missing_data_table")
                    )
                )
            ),

            # 2. EDA Tab
            tabItem(tabName = "eda",
                h2("환자군별 변수 탐색 (EDA)"),
                fluidRow(
                    # --- 컨트롤 패널 ---
                    box(
                        title = "시각화 옵션",
                        width = 3,
                        solidHeader = TRUE,
                        status = "primary",
                        
                        h4("단변량 분석"),
                        selectInput("eda_variable_single", "분석할 변수 선택:", 
                                    choices = c("진단 시 연령" = "기본환자진단시연령", "진단 시 CEA 수치" = "진단시점_CEA")),
                        selectInput("eda_group_single", "환자 그룹으로 나누기:", 
                                    choices = c("All", "EOCRC", "LOCRC")),
                        
                        hr(),
                        
                        h4("다변량 비교 분석"),
                        selectInput("eda_x_variable", "X축 변수:", 
                                    choices = c("진단 시 연령" = "기본환자진단시연령", "진단 시 CEA 수치" = "진단시점_CEA"), 
                                    selected = "기본환자진단시연령"),
                        selectInput("eda_y_variable", "Y축 변수:", 
                                    choices = c("진단 시 연령" = "기본환자진단시연령", "진단 시 CEA 수치" = "진단시점_CEA"), 
                                    selected = "진단시점_CEA"),
                        selectInput("eda_color_variable", "그룹 색상:", 
                                    choices = c("없음" = "None", "환자군" = "Group", "병기" = "Stage", "성별" = "기본환자성별코드"), 
                                    selected = "Group")
                    ),
                    
                    # --- 플롯 출력 영역 ---
                    column(
                        width = 9,
                        box(
                            title = "단변량 분석 결과",
                            width = NULL, solidHeader = TRUE, status = "info",
                            plotlyOutput("eda_plot_single")
                        ),
                        box(
                            title = "다변량 분석 결과",
                            width = NULL, solidHeader = TRUE, status = "success",
                            plotlyOutput("eda_plot_multi")
                        )
                    )
                )
            ),

            # 3. Survival Analysis Tab
            tabItem(tabName = "survival",
                h2("Kaplan-Meier 생존분석"),
                fluidRow(
                    box(
                        title = "분석 옵션",
                        width = 3,
                        solidHeader = TRUE,
                        status = "primary",
                        selectInput("survival_variable", "그룹 비교 변수 선택:",
                                    choices = c(
                                        "환자 그룹 (EOCRC/LOCRC)" = "Group",
                                        "성별" = "기본환자성별코드",
                                        "암 병기" = "Stage",
                                        "수술 여부" = "수술여부",
                                        "항암치료 여부" = "항암치료여부",
                                        "방사선치료 여부" = "방사선치료여부",
                                        "KRAS 변이" = "KRAS_MUTATION",
                                        "MSI 상태" = "MSI_STATUS"
                                    ),
                                    selected = "Group")
                    ),
                    box(
                        title = "생존 곡선",
                        width = 9,
                        solidHeader = TRUE,
                        status = "primary",
                        plotOutput("survival_plot")
                    )
                )
            ),

            # Other tabs remain as placeholders
            tabItem(tabName = "prediction", h2("머신러닝 기반 생존예측")),

            # 5. Insight Tab
            tabItem(tabName = "insight",
                h2("변수 상관관계 및 인사이트"),
                fluidRow(
                    box(
                        title = "주요 수치형 변수 상관관계 히트맵",
                        width = 12,
                        solidHeader = TRUE,
                        status = "primary",
                        plotOutput("correlation_heatmap"),
                        p(strong("참고:"), "주요 수치형 변수(진단 시 연령, 진단 시 CEA, 수술 시간, 생존 기간) 간의 피어슨 상관계수를 나타냅니다.")
                    )
                ),
                fluidRow(
                    box(
                        title = "주요 변수 요약 통계",
                        width = 12,
                        solidHeader = TRUE,
                        status = "primary",
                        radioButtons("summary_group", "환자 그룹 선택:",
                                     choices = c("전체" = "All", "EOCRC" = "EOCRC", "LOCRC" = "LOCRC"),
                                     selected = "All", inline = TRUE),
                        DTOutput("summary_stats_table")
                    )
                )
            ),

            tabItem(tabName = "performance",
                h2("모델 성능 비교"),
                fluidRow(
                    box(
                        title = "모델 성능 요약", 
                        width = 12, 
                        solidHeader = TRUE, 
                        status = "primary",
                        DTOutput("performance_table"),
                        p(strong("참고:"), "현재 표시는 초기 모델(06_ml_modeling.R)의 결과이며, 최적화된 모델의 결과로 업데이트될 예정입니다.")
                    )
                )
            ),
            tabItem(tabName = "download", h2("데이터/코드 다운로드"))
        )
    )
)
