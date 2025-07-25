# ==============================================================================
# Server Logic
# ==============================================================================
server <- function(input, output) {

    # --- Home Tab Logic ---
    output$stage_distribution_plot <- renderPlotly({
        plot_data <- full_data %>%
            group_by(Group, Stage) %>% 
            summarise(Count = n(), .groups = 'drop')

        p <- ggplot(plot_data, aes(x = Stage, y = Count, fill = Group)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(title = "그룹별 암 병기 분포", x = "암 병기 (Stage)", y = "환자 수") +
            theme_minimal() +
            scale_fill_brewer(palette = "Set2")
        
        ggplotly(p)
    })

    # --- New plots/tables for Home tab ---
    # 1. Survival/Death Pie Chart
    output$survival_status_pie_chart <- renderPlotly({
        pie_data <- full_data %>% 
            mutate(Status = ifelse(사망여부 == 1, "사망", "생존")) %>% 
            count(Status)

        p <- plot_ly(pie_data, labels = ~Status, values = ~n, type = 'pie',
                     textinfo = 'label+percent', insidetextorientation = 'radial',
                     marker = list(colors = c("생존" = "#1f77b4", "사망" = "#d62728"))) %>% 
            layout(title = '전체 환자 생존/사망 비율')
        p
    })

    # 2. Missing Data Table
    output$missing_data_table <- renderDT({
        missing_summary <- full_data %>% 
            select(기본환자진단시연령, 기본환자성별코드, Stage, 진단시점_CEA, 수술여부, 항암치료여부, 방사선치료여부, KRAS_MUTATION, MSI_STATUS) %>% 
            summarise_all(~sum(is.na(.) | . == "" | . == "모름")) %>% 
            pivot_longer(everything(), names_to = "변수명", values_to = "결측값 수") %>% 
            mutate(결측률 = paste0(round(`결측값 수` / nrow(full_data) * 100, 1), "%")) %>% 
            arrange(desc(`결측값 수`))

        datatable(missing_summary, 
                  rownames = FALSE,
                  options = list(pageLength = 5, autoWidth = TRUE))
    })

    # --- EDA Tab Logic ---

    # 1. 단변량 분석
    output$eda_plot_single <- renderPlotly({
        req(input$eda_variable_single)
        
        plot_data <- if (input$eda_group_single == "All") {
            full_data
        } else {
            full_data %>% filter(Group == input$eda_group_single)
        }
        
        p <- ggplot(plot_data, aes_string(x = input$eda_variable_single)) +
            labs(title = paste(input$eda_variable_single, "분포"), x = input$eda_variable_single) +
            theme_minimal()

        # 변수 타입에 따라 다른 그래프 출력
        if (is.numeric(plot_data[[input$eda_variable_single]])) {
            p <- p + 
                geom_histogram(aes(y = ..density..), fill = "#3c8dbc", color = "white", alpha = 0.7, bins=30) +
                geom_density(alpha = .2, fill = "#FF6666") +
                labs(y = "밀도")
        } else {
            p <- p + 
                geom_bar(aes(fill = ..x..), alpha = 0.8) +
                theme(legend.position = "none") +
                labs(y = "빈도")
        }

        ggplotly(p)
    })

    # 2. 다변량 분석
    output$eda_plot_multi <- renderPlotly({
        req(input$eda_x_variable, input$eda_y_variable)

        p <- ggplot(full_data, aes_string(x = input$eda_x_variable, y = input$eda_y_variable))

        if (input$eda_color_variable != "None") {
            p <- p + aes_string(color = input$eda_color_variable)
        }

        p <- p + 
            geom_point(alpha = 0.6) +
            labs(
                title = paste(input$eda_y_variable, "vs.", input$eda_x_variable),
                x = input$eda_x_variable,
                y = input$eda_y_variable
            ) +
            theme_minimal() +
            theme(legend.position = "bottom")

        ggplotly(p)
    })

    # --- Survival Analysis Tab Logic ---
    output$survival_plot <- renderPlot({
        req(input$survival_variable)
        
        # Filter out NA/empty values for the selected grouping variable
        survival_data <- full_data %>% 
            filter(!is.na(.data[[input$survival_variable]]) & .data[[input$survival_variable]] != "")

        # Create survival object and fit model
        surv_object <- Surv(time = survival_data$생존기간_일, event = survival_data$사망여부)
        fit_formula <- as.formula(paste("surv_object ~", input$survival_variable))
        fit <- survfit(fit_formula, data = survival_data)
        
        # Get the display name for the legend title
        variable_choices <- c(
            "환자 그룹 (EOCRC/LOCRC)" = "Group", "성별" = "기본환자성별코드", "암 병기" = "Stage",
            "수술 여부" = "수술여부", "항암치료 여부" = "항암치료여부", "방사선치료 여부" = "방사선치료여부",
            "KRAS 변이" = "KRAS_MUTATION", "MSI 상태" = "MSI_STATUS"
        )
        legend_title <- names(variable_choices)[variable_choices == input$survival_variable]

        # Generate plot with enhanced options
        ggsurvplot(
            fit,
            data = survival_data,
            title = "Kaplan-Meier 생존 곡선",
            xlab = "시간 (일)", 
            ylab = "생존 확률",
            legend.title = legend_title,
            pval = TRUE,                # p-value 표시
            pval.method = TRUE,         # p-value 계산 방법 표시
            conf.int = TRUE,            # 신뢰 구간 표시
            risk.table = TRUE,          # 위험표 추가
            surv.median.line = "hv",    # 중앙 생존 기간 선 추가
            ggtheme = theme_minimal(base_size = 14),
            palette = "Set1",           # 색상 팔레트 지정
            tables.height = 0.25,
            tables.theme = theme_survminer(font.main = 12),
            risk.table.y.text = FALSE,  # 위험표 y축 텍스트 제거
            risk.table.y.text.col = TRUE
        )
    })

    # --- Insight Tab Logic ---
    output$correlation_heatmap <- renderPlot({
        # Select key numeric variables and convert them to numeric, handling potential errors
        cor_data <- full_data %>%
            mutate(
                진단시점_CEA = as.numeric(as.character(진단시점_CEA)),
                수술시간 = as.numeric(as.character(수술시간))
            ) %>%
            select(기본환자진단시연령, 진단시점_CEA, 수술시간, 생존기간_일) %>%
            rename(
                '진단 연령' = 기본환자진단시연령,
                'CEA 수치' = 진단시점_CEA,
                '수술 시간' = 수술시간,
                '생존 기간(일)' = 생존기간_일
            )

        # Calculate correlation matrix, handling missing values
        cormat <- round(cor(cor_data, use = "complete.obs"), 2)
        
        # Melt the correlation matrix for ggplot
        melted_cormat <- melt(cormat)
        
        # Create the heatmap
        ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
            geom_tile(color = "white") +
            scale_fill_gradient2(low = "#377EB8", high = "#E41A1C", mid = "white", 
                               midpoint = 0, limit = c(-1,1), space = "Lab",
                               name="상관계수") +
            theme_minimal(base_size = 14) +
            theme(
                axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                axis.text.y = element_text(size = 12),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.grid.major = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.ticks = element_blank()
            ) +
            geom_text(aes(label = value), color = "black", size = 5)
    })

    # --- Summary Statistics Table Logic ---
    output$summary_stats_table <- renderDT({
        # Filter data based on selected group
        summary_data <- if (input$summary_group == "All") {
            full_data
        } else {
            full_data %>% filter(Group == input$summary_group)
        }

        # Define variables to summarize - 수술시간 변수 제거 (데이터에 없음)
        numeric_vars <- c("기본환자진단시연령", "진단시점_CEA", "생존기간_일")
        categorical_vars <- c("기본환자성별코드", "Stage", "수술여부", "항암치료여부", "방사선치료여부", "KRAS_MUTATION", "MSI_STATUS")

        # Summarize numeric variables
        numeric_summary <- summary_data %>%
            select(all_of(numeric_vars)) %>%
            pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
            group_by(Variable) %>%
            summarise(
                N = sum(!is.na(Value)),
                `평균 ± 표준편차` = paste0(round(mean(Value, na.rm = TRUE), 1), " ± ", round(sd(Value, na.rm = TRUE), 1)),
                `중앙값 (IQR)` = paste0(round(median(Value, na.rm = TRUE), 1), " (", round(quantile(Value, 0.25, na.rm = TRUE), 1), "-", round(quantile(Value, 0.75, na.rm = TRUE), 1), ")")
            ) %>%
            mutate(분류 = "수치형")

        # Summarize categorical variables
        categorical_summary <- summary_data %>%
            select(all_of(categorical_vars)) %>%
            pivot_longer(everything(), names_to = "Variable", values_to = "Level") %>%
            filter(!is.na(Level) & Level != "") %>%
            count(Variable, Level) %>%
            group_by(Variable) %>%
            mutate(Percent = round(n / sum(n) * 100, 1)) %>%
            summarise(`값 (N, %)` = paste0(Level, " (", n, ", ", Percent, "%)", collapse = "<br>")) %>%
            mutate(분류 = "범주형")

        # Combine and format
        final_summary <- bind_rows(numeric_summary, categorical_summary) %>% 
            select(분류, 변수 = Variable, everything()) 

        datatable(final_summary, escape = FALSE, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
    })

    # --- Model Performance Tab Logic ---
    performance_data <- reactive({
        # Ensure the file exists before trying to read it
        perf_file <- "../results/ml_models/model_performance_summary.csv"
        if (file.exists(perf_file)) {
            read.csv(perf_file)
        } else {
            data.frame(Message = "모델 성능 요약 파일(model_performance_summary.csv)을 찾을 수 없습니다. 모델을 먼저 실행해주세요.")
        }
    })

    output$performance_table <- renderDT({
        datatable(performance_data(), 
                  options = list(pageLength = 10, autoWidth = TRUE),
                  rownames = FALSE,
                  caption = "모델별/그룹별 성능 지표 (AUC)")
    })
}
