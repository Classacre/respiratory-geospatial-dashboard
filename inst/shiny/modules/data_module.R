# Data Module
# Interactive data table and download options

dataUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Data controls
    column(
      width = 3,
      box(
        title = "Data Options",
        status = "primary",
        solidHeader = TRUE,
        width = NULL,
        
        selectInput(ns("dataset_select"),
                    "Select Dataset:",
                    choices = c(
                      "All Patients" = "all",
                      "Asthma Patients Only" = "asthma",
                      "Baseline Visits Only" = "baseline"
                    ),
                    selected = "all"),
        
        hr(),
        
        h4("Download Data"),
        
        selectInput(ns("download_format"),
                    "Format:",
                    choices = c(
                      "CSV" = "csv",
                      "Excel" = "xlsx",
                      "RDS" = "rds"
                    ),
                    selected = "csv"),
        
        downloadButton(ns("download_data"), "Download", class = "btn-success")
      ),
      
      box(
        title = "Column Selection",
        status = "info",
        solidHeader = TRUE,
        width = NULL,
        
        checkboxGroupInput(ns("selected_columns"),
                           "Show Columns:",
                           choices = c(),  # Will be populated dynamically
                           selected = c())
      ),
      
      box(
        title = "Quick Stats",
        status = "warning",
        solidHeader = TRUE,
        width = NULL,
        verbatimTextOutput(ns("quick_stats"))
      )
    ),
    
    # Data table
    column(
      width = 9,
      box(
        title = "Data Explorer",
        status = "primary",
        solidHeader = TRUE,
        width = NULL,
        DTOutput(ns("data_table"), height = 600)
      )
    )
  )
}

dataServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Update column choices when data loads
    observe({
      df <- data()
      cols <- names(df)
      
      # Default selected columns
      default_cols <- c("patient_id", "sex", "age_years", "asthma_diagnosis",
                        "fev1", "fvc", "pm25", "exacerbation_count")
      default_cols <- intersect(default_cols, cols)
      
      updateCheckboxGroupInput(session, "selected_columns",
                               choices = cols,
                               selected = default_cols)
    })
    
    # Filtered dataset based on selection
    filtered_dataset <- reactive({
      df <- data()
      
      if (input$dataset_select == "asthma") {
        df <- df[df$asthma_diagnosis == "Yes", ]
      } else if (input$dataset_select == "baseline") {
        # Get first visit for each patient
        df <- df %>%
          group_by(patient_id) %>%
          slice_min(order_by = visit_number, n = 1) %>%
          ungroup()
      }
      
      return(df)
    })
    
    # Data table
    output$data_table <- renderDT({
      df <- filtered_dataset()
      
      # Select columns if any chosen
      if (length(input$selected_columns) > 0) {
        df <- df[, input$selected_columns, drop = FALSE]
      }
      
      datatable(
        df,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          scrollY = "500px",
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        filter = 'top',
        rownames = FALSE
      )
    })
    
    # Quick stats
    output$quick_stats <- renderPrint({
      df <- filtered_dataset()
      
      cat("Dataset Summary\n")
      cat("===============\n\n")
      
      cat("Total rows:", nrow(df), "\n")
      cat("Unique patients:", length(unique(df$patient_id)), "\n")
      cat("Columns:", ncol(df), "\n\n")
      
      if ("asthma_diagnosis" %in% names(df)) {
        asthma_count <- sum(df$asthma_diagnosis == "Yes", na.rm = TRUE)
        cat("Asthma cases:", asthma_count, "(", 
            round(100 * asthma_count / nrow(df), 1), "%)\n")
      }
      
      if ("sex" %in% names(df)) {
        cat("\nSex distribution:\n")
        print(table(df$sex))
      }
    })
    
    # Download handler
    output$download_data <- downloadHandler(
      filename = function() {
        prefix <- switch(input$dataset_select,
                         "all" = "all_patients",
                         "asthma" = "asthma_patients",
                         "baseline" = "baseline_visits")
        
        ext <- switch(input$download_format,
                      "csv" = "csv",
                      "xlsx" = "xlsx",
                      "rds" = "rds")
        
        paste0(prefix, "_", Sys.Date(), ".", ext)
      },
      content = function(file) {
        df <- filtered_dataset()
        
        # Select columns if any chosen
        if (length(input$selected_columns) > 0) {
          df <- df[, input$selected_columns, drop = FALSE]
        }
        
        switch(input$download_format,
               "csv" = write.csv(df, file, row.names = FALSE),
               "xlsx" = {
                 if (requireNamespace("writexl", quietly = TRUE)) {
                   writexl::write_xlsx(df, file)
                 } else {
                   write.csv(df, file, row.names = FALSE)
                   showNotification("writexl not available, using CSV format", type = "warning")
                 }
               },
               "rds" = saveRDS(df, file)
        )
      }
    )
  })
}
