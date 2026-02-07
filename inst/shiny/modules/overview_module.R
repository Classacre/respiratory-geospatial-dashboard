# Overview Module
# Summary statistics and study cohort description

overviewUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Summary boxes
    valueBoxOutput(ns("total_patients"), width = 3),
    valueBoxOutput(ns("asthma_prevalence"), width = 3),
    valueBoxOutput(ns("avg_fev1"), width = 3),
    valueBoxOutput(ns("avg_exacerbations"), width = 3),
    
    # Demographics section
    box(
      title = "Demographics",
      status = "primary",
      solidHeader = TRUE,
      width = 6,
      plotlyOutput(ns("age_dist_plot"), height = 300),
      br(),
      plotlyOutput(ns("sex_dist_plot"), height = 250)
    ),
    
    # Environmental exposures
    box(
      title = "Environmental Exposures",
      status = "warning",
      solidHeader = TRUE,
      width = 6,
      plotlyOutput(ns("pm25_dist_plot"), height = 250),
      br(),
      plotlyOutput(ns("pollen_dist_plot"), height = 250)
    ),
    
    # Lung function summary
    box(
      title = "Lung Function Distribution",
      status = "success",
      solidHeader = TRUE,
      width = 6,
      plotlyOutput(ns("fev1_dist_plot"), height = 300)
    ),
    
    # Asthma outcomes
    box(
      title = "Asthma Outcomes",
      status = "danger",
      solidHeader = TRUE,
      width = 6,
      plotlyOutput(ns("exacerbation_plot"), height = 300)
    ),
    
    # Summary statistics table
    box(
      title = "Summary Statistics",
      status = "info",
      solidHeader = TRUE,
      width = 12,
      DTOutput(ns("summary_table"))
    )
  )
}

overviewServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Total patients
    output$total_patients <- renderValueBox({
      n <- length(unique(data()$patient_id))
      valueBox(
        format(n, big.mark = ","),
        "Total Patients",
        icon = icon("users"),
        color = "aqua"
      )
    })
    
    # Asthma prevalence
    output$asthma_prevalence <- renderValueBox({
      df <- data()
      baseline <- df[df$visit_number == 1 | is.na(df$visit_number), ]
      prev <- mean(baseline$asthma_diagnosis == "Yes", na.rm = TRUE) * 100
      valueBox(
        paste0(round(prev, 1), "%"),
        "Asthma Prevalence",
        icon = icon("stethoscope"),
        color = "red"
      )
    })
    
    # Average FEV1
    output$avg_fev1 <- renderValueBox({
      avg_fev1 <- mean(data()$fev1, na.rm = TRUE)
      valueBox(
        round(avg_fev1, 2),
        "Mean FEV1 (L)",
        icon = icon("lungs"),
        color = "green"
      )
    })
    
    # Average exacerbations
    output$avg_exacerbations <- renderValueBox({
      avg_exac <- mean(data()$exacerbation_count, na.rm = TRUE)
      valueBox(
        round(avg_exac, 1),
        "Mean Exacerbations",
        icon = icon("hospital"),
        color = "yellow"
      )
    })
    
    # Age distribution
    output$age_dist_plot <- renderPlotly({
      p <- ggplot(data(), aes(x = age_years, fill = sex)) +
        geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
        scale_fill_viridis_d() +
        labs(title = "Age Distribution by Sex",
             x = "Age (years)", y = "Count") +
        theme_minimal()
      ggplotly(p)
    })
    
    # Sex distribution
    output$sex_dist_plot <- renderPlotly({
      sex_counts <- data() %>%
        distinct(patient_id, .keep_all = TRUE) %>%
        count(sex)
      
      p <- ggplot(sex_counts, aes(x = sex, y = n, fill = sex)) +
        geom_bar(stat = "identity") +
        scale_fill_viridis_d() +
        labs(title = "Sex Distribution",
             x = "Sex", y = "Count") +
        theme_minimal()
      ggplotly(p)
    })
    
    # PM2.5 distribution
    output$pm25_dist_plot <- renderPlotly({
      p <- ggplot(data(), aes(x = pm25, fill = asthma_diagnosis)) +
        geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
        scale_fill_manual(values = c("No" = "#2E86AB", "Yes" = "#A23B72")) +
        labs(title = "PM2.5 Distribution by Asthma Status",
             x = "PM2.5 (μg/m³)", y = "Count") +
        theme_minimal()
      ggplotly(p)
    })
    
    # Pollen distribution
    output$pollen_dist_plot <- renderPlotly({
      p <- ggplot(data(), aes(x = pollen_index, fill = asthma_diagnosis)) +
        geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
        scale_fill_manual(values = c("No" = "#2E86AB", "Yes" = "#A23B72")) +
        labs(title = "Pollen Index Distribution by Asthma Status",
             x = "Pollen Index", y = "Count") +
        theme_minimal()
      ggplotly(p)
    })
    
    # FEV1 distribution
    output$fev1_dist_plot <- renderPlotly({
      p <- ggplot(data(), aes(x = fev1, fill = sex)) +
        geom_density(alpha = 0.5) +
        scale_fill_viridis_d() +
        labs(title = "FEV1 Distribution",
             x = "FEV1 (L)", y = "Density") +
        theme_minimal()
      ggplotly(p)
    })
    
    # Exacerbation plot
    output$exacerbation_plot <- renderPlotly({
      exac_data <- data() %>%
        distinct(patient_id, .keep_all = TRUE) %>%
        count(exacerbation_count)
      
      p <- ggplot(exac_data, aes(x = exacerbation_count, y = n)) +
        geom_bar(stat = "identity", fill = "#F18F01") +
        labs(title = "Exacerbation Count Distribution",
             x = "Number of Exacerbations", y = "Number of Patients") +
        theme_minimal()
      ggplotly(p)
    })
    
    # Summary statistics table
    output$summary_table <- renderDT({
      df <- data()
      
      summary_stats <- data.frame(
        Variable = c("Age (years)", "FEV1 (L)", "FVC (L)", "FEV1/FVC Ratio",
                     "PM2.5 (μg/m³)", "Pollen Index", "Temperature (°C)",
                     "Exacerbations", "Hospitalizations"),
        Mean = c(
          mean(df$age_years, na.rm = TRUE),
          mean(df$fev1, na.rm = TRUE),
          mean(df$fvc, na.rm = TRUE),
          mean(df$fev1_fvc_ratio, na.rm = TRUE),
          mean(df$pm25, na.rm = TRUE),
          mean(df$pollen_index, na.rm = TRUE),
          mean(df$temperature, na.rm = TRUE),
          mean(df$exacerbation_count, na.rm = TRUE),
          mean(df$hospitalization_count, na.rm = TRUE)
        ),
        SD = c(
          sd(df$age_years, na.rm = TRUE),
          sd(df$fev1, na.rm = TRUE),
          sd(df$fvc, na.rm = TRUE),
          sd(df$fev1_fvc_ratio, na.rm = TRUE),
          sd(df$pm25, na.rm = TRUE),
          sd(df$pollen_index, na.rm = TRUE),
          sd(df$temperature, na.rm = TRUE),
          sd(df$exacerbation_count, na.rm = TRUE),
          sd(df$hospitalization_count, na.rm = TRUE)
        ),
        Min = c(
          min(df$age_years, na.rm = TRUE),
          min(df$fev1, na.rm = TRUE),
          min(df$fvc, na.rm = TRUE),
          min(df$fev1_fvc_ratio, na.rm = TRUE),
          min(df$pm25, na.rm = TRUE),
          min(df$pollen_index, na.rm = TRUE),
          min(df$temperature, na.rm = TRUE),
          min(df$exacerbation_count, na.rm = TRUE),
          min(df$hospitalization_count, na.rm = TRUE)
        ),
        Max = c(
          max(df$age_years, na.rm = TRUE),
          max(df$fev1, na.rm = TRUE),
          max(df$fvc, na.rm = TRUE),
          max(df$fev1_fvc_ratio, na.rm = TRUE),
          max(df$pm25, na.rm = TRUE),
          max(df$pollen_index, na.rm = TRUE),
          max(df$temperature, na.rm = TRUE),
          max(df$exacerbation_count, na.rm = TRUE),
          max(df$hospitalization_count, na.rm = TRUE)
        )
      )
      
      summary_stats[, -1] <- round(summary_stats[, -1], 2)
      
      datatable(summary_stats, options = list(pageLength = 10, dom = 't'))
    })
  })
}
