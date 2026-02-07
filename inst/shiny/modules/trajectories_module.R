# Trajectories Module
# Lung function trajectory analysis and visualization

trajectoriesUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Controls
    column(
      width = 3,
      box(
        title = "Analysis Options",
        status = "primary",
        solidHeader = TRUE,
        width = NULL,
        
        selectInput(ns("plot_type"),
                    "Plot Type:",
                    choices = c(
                      "Individual Trajectories" = "individual",
                      "Population Curves" = "population",
                      "Growth Percentiles" = "percentiles",
                      "By Risk Factors" = "risk_factors"
                    ),
                    selected = "population"),
        
        conditionalPanel(
          condition = paste0("input['", ns("plot_type"), "'] == 'individual'"),
          numericInput(ns("patient_id"),
                       "Patient ID:",
                       value = 1, min = 1, max = 5000),
          checkboxInput(ns("show_reference"),
                        "Show Reference Curves",
                        value = TRUE)
        ),
        
        selectInput(ns("lung_function_measure"),
                    "Lung Function Measure:",
                    choices = c(
                      "FEV1" = "fev1",
                      "FVC" = "fvc",
                      "FEV1/FVC Ratio" = "fev1_fvc_ratio"
                    ),
                    selected = "fev1"),
        
        checkboxGroupInput(ns("group_by"),
                           "Group By:",
                           choices = c("Sex" = "sex", "Asthma" = "asthma_diagnosis"),
                           selected = c("sex")),
        
        hr(),
        
        actionButton(ns("update_plot"), "Update Plot", class = "btn-primary")
      ),
      
      box(
        title = "Statistics",
        status = "info",
        solidHeader = TRUE,
        width = NULL,
        verbatimTextOutput(ns("trajectory_stats"))
      )
    ),
    
    # Main plot area
    column(
      width = 9,
      box(
        title = "Lung Function Trajectories",
        status = "primary",
        solidHeader = TRUE,
        width = NULL,
        plotlyOutput(ns("trajectory_plot"), height = 500)
      ),
      
      fluidRow(
        box(
          title = "Age-Related Changes",
          status = "success",
          solidHeader = TRUE,
          width = 6,
          plotlyOutput(ns("age_change_plot"), height = 300)
        ),
        
        box(
          title = "Environmental Effects",
          status = "warning",
          solidHeader = TRUE,
          width = 6,
          plotlyOutput(ns("env_effect_plot"), height = 300)
        )
      )
    )
  )
}

trajectoriesServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Get measure label
    get_measure_label <- reactive({
      switch(input$lung_function_measure,
             "fev1" = "FEV1 (L)",
             "fvc" = "FVC (L)",
             "fev1_fvc_ratio" = "FEV1/FVC Ratio")
    })
    
    # Main trajectory plot
    output$trajectory_plot <- renderPlotly({
      input$update_plot
      
      df <- data()
      measure <- input$lung_function_measure
      measure_label <- get_measure_label()
      
      if (input$plot_type == "individual") {
        # Individual patient trajectory
        patient_data <- df[df$patient_id == input$patient_id, ]
        
        if (nrow(patient_data) == 0) {
          return(plotly_empty() %>% 
                   layout(title = "Patient not found"))
        }
        
        # Sort by visit
        patient_data <- patient_data[order(patient_data$visit_number), ]
        
        p <- ggplot(patient_data, aes_string(x = "age_years", y = measure)) +
          geom_line(color = "#2E86AB", size = 1) +
          geom_point(color = "#2E86AB", size = 3) +
          labs(title = paste("Patient", input$patient_id, "Trajectory"),
               x = "Age (years)", y = measure_label) +
          theme_minimal()
        
        # Add reference curve if requested
        if (input$show_reference) {
          ref_data <- df %>%
            filter(sex == patient_data$sex[1]) %>%
            group_by(age_years = round(age_years)) %>%
            summarise(ref_value = mean(.data[[measure]], na.rm = TRUE),
                      .groups = "drop")
          
          p <- p + geom_line(data = ref_data, 
                             aes(x = age_years, y = ref_value),
                             color = "gray", linetype = "dashed", size = 1)
        }
        
      } else if (input$plot_type == "population") {
        # Population curves
        group_vars <- c("age_years = round(age_years)")
        if ("sex" %in% input$group_by) group_vars <- c(group_vars, "sex")
        if ("asthma_diagnosis" %in% input$group_by) group_vars <- c(group_vars, "asthma_diagnosis")
        
        pop_data <- df %>%
          group_by(across(all_of(setdiff(group_vars, "age_years = round(age_years)")))) %>%
          mutate(age_years = round(age_years)) %>%
          group_by(across(all_of(c("age_years", setdiff(group_vars, "age_years = round(age_years)"))))) %>%
          summarise(
            mean_value = mean(.data[[measure]], na.rm = TRUE),
            se_value = sd(.data[[measure]], na.rm = TRUE) / sqrt(n()),
            n = n(),
            .groups = "drop"
          ) %>%
          filter(n >= 5)  # Only show points with sufficient data
        
        aes_mapping <- aes(x = age_years, y = mean_value)
        if (length(input$group_by) > 0) {
          aes_mapping <- aes(x = age_years, y = mean_value, 
                             color = interaction(!!!syms(input$group_by)),
                             group = interaction(!!!syms(input$group_by)))
        }
        
        p <- ggplot(pop_data, aes_mapping) +
          geom_line(size = 1) +
          geom_ribbon(aes(ymin = mean_value - 1.96*se_value, 
                          ymax = mean_value + 1.96*se_value),
                      alpha = 0.2) +
          geom_point(size = 2) +
          scale_color_viridis_d() +
          labs(title = "Population Lung Function Curves",
               x = "Age (years)", y = measure_label,
               color = "Group") +
          theme_minimal()
        
      } else if (input$plot_type == "percentiles") {
        # Growth percentiles
        percentile_data <- df %>%
          group_by(sex, age_group = cut(age_years, breaks = seq(2, 18, by = 2))) %>%
          summarise(
            p5 = quantile(.data[[measure]], 0.05, na.rm = TRUE),
            p25 = quantile(.data[[measure]], 0.25, na.rm = TRUE),
            p50 = quantile(.data[[measure]], 0.50, na.rm = TRUE),
            p75 = quantile(.data[[measure]], 0.75, na.rm = TRUE),
            p95 = quantile(.data[[measure]], 0.95, na.rm = TRUE),
            .groups = "drop"
          )
        
        p <- ggplot(percentile_data, aes(x = age_group)) +
          geom_ribbon(aes(ymin = p5, ymax = p95, fill = "5th-95th"), alpha = 0.2) +
          geom_ribbon(aes(ymin = p25, ymax = p75, fill = "25th-75th"), alpha = 0.3) +
          geom_line(aes(y = p50, color = "Median"), size = 1) +
          facet_wrap(~sex) +
          scale_fill_manual(values = c("5th-95th" = "#3498DB", "25th-75th" = "#2980B9")) +
          scale_color_manual(values = c("Median" = "#E74C3C")) +
          labs(title = "Growth-Adjusted Percentiles",
               x = "Age Group", y = measure_label) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
      } else if (input$plot_type == "risk_factors") {
        # By risk factors (PM2.5 quartiles)
        df$pm25_quartile <- cut(df$pm25, breaks = 4, labels = c("Q1 (Low)", "Q2", "Q3", "Q4 (High)"))
        
        risk_data <- df %>%
          group_by(pm25_quartile, age_years = round(age_years)) %>%
          summarise(mean_value = mean(.data[[measure]], na.rm = TRUE),
                    .groups = "drop")
        
        p <- ggplot(risk_data, aes(x = age_years, y = mean_value, color = pm25_quartile)) +
          geom_line(size = 1) +
          geom_point(size = 2) +
          scale_color_viridis_d() +
          labs(title = "Lung Function by PM2.5 Exposure Quartile",
               x = "Age (years)", y = measure_label,
               color = "PM2.5 Quartile") +
          theme_minimal()
      }
      
      ggplotly(p)
    })
    
    # Age-related changes plot
    output$age_change_plot <- renderPlotly({
      df <- data()
      
      age_changes <- df %>%
        group_by(age_years = round(age_years)) %>%
        summarise(
          mean_fev1 = mean(fev1, na.rm = TRUE),
          mean_fvc = mean(fvc, na.rm = TRUE),
          n = n(),
          .groups = "drop"
        ) %>%
        filter(n >= 10)
      
      age_changes_long <- tidyr::pivot_longer(
        age_changes,
        cols = c(mean_fev1, mean_fvc),
        names_to = "measure",
        values_to = "value"
      )
      age_changes_long$measure <- factor(age_changes_long$measure,
                                          labels = c("FEV1", "FVC"))
      
      p <- ggplot(age_changes_long, aes(x = age_years, y = value, color = measure)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        scale_color_viridis_d() +
        labs(title = "Mean Lung Function by Age",
             x = "Age (years)", y = "Volume (L)",
             color = "Measure") +
        theme_minimal()
      
      ggplotly(p)
    })
    
    # Environmental effects plot
    output$env_effect_plot <- renderPlotly({
      df <- data()
      
      env_effects <- df %>%
        mutate(pm25_group = cut(pm25, breaks = 5)) %>%
        group_by(pm25_group, asthma_diagnosis) %>%
        summarise(
          mean_fev1 = mean(fev1, na.rm = TRUE),
          n = n(),
          .groups = "drop"
        ) %>%
        filter(n >= 5)
      
      p <- ggplot(env_effects, aes(x = pm25_group, y = mean_fev1, fill = asthma_diagnosis)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("No" = "#2E86AB", "Yes" = "#A23B72")) +
        labs(title = "FEV1 by PM2.5 and Asthma Status",
             x = "PM2.5 Group", y = "Mean FEV1 (L)",
             fill = "Asthma") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
    })
    
    # Trajectory statistics
    output$trajectory_stats <- renderPrint({
      df <- data()
      measure <- input$lung_function_measure
      
      cat("Lung Function Statistics\n")
      cat("========================\n\n")
      
      cat("Measure:", get_measure_label(), "\n\n")
      
      # Overall stats
      cat("Overall:\n")
      cat("  Mean:", round(mean(df[[measure]], na.rm = TRUE), 3), "\n")
      cat("  SD:", round(sd(df[[measure]], na.rm = TRUE), 3), "\n")
      cat("  Range:", round(min(df[[measure]], na.rm = TRUE), 3), "-", 
          round(max(df[[measure]], na.rm = TRUE), 3), "\n\n")
      
      # By sex
      cat("By Sex:\n")
      sex_stats <- tapply(df[[measure]], df$sex, function(x) {
        c(Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE))
      })
      print(sex_stats)
      cat("\n")
      
      # By asthma status
      cat("By Asthma Status:\n")
      asthma_stats <- tapply(df[[measure]], df$asthma_diagnosis, function(x) {
        c(Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE))
      })
      print(asthma_stats)
    })
  })
}
