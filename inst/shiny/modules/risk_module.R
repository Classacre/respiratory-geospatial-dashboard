# Risk Module
# Risk prediction models and environmental risk factor analysis

riskUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Model controls
    column(
      width = 3,
      box(
        title = "Model Settings",
        status = "primary",
        solidHeader = TRUE,
        width = NULL,
        
        selectInput(ns("model_type"),
                    "Model Type:",
                    choices = c(
                      "Asthma Risk (Logistic)" = "asthma",
                      "Lung Function (Linear)" = "lung_function"
                    ),
                    selected = "asthma"),
        
        checkboxGroupInput(ns("predictors"),
                           "Predictors:",
                           choices = c(
                             "PM2.5" = "pm25",
                             "Pollen Index" = "pollen_index",
                             "Temperature" = "temperature",
                             "Sex" = "sex",
                             "Age" = "age_years",
                             "SES Score" = "ses_score"
                           ),
                           selected = c("pm25", "sex", "age_years")),
        
        conditionalPanel(
          condition = paste0("input['", ns("model_type"), "'] == 'lung_function'"),
          selectInput(ns("outcome_measure"),
                      "Outcome:",
                      choices = c(
                        "FEV1" = "fev1",
                        "FVC" = "fvc",
                        "FEV1/FVC Ratio" = "fev1_fvc_ratio"
                      ),
                      selected = "fev1")
        ),
        
        hr(),
        
        actionButton(ns("run_model"), "Run Model", class = "btn-primary")
      ),
      
      box(
        title = "Model Performance",
        status = "info",
        solidHeader = TRUE,
        width = NULL,
        tableOutput(ns("performance_table"))
      )
    ),
    
    # Results area
    column(
      width = 9,
      
      # Odds ratios / Coefficients
      box(
        title = "Model Results",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        plotlyOutput(ns("coefficient_plot"), height = 400)
      ),
      
      # Forest plot
      box(
        title = "Forest Plot",
        status = "success",
        solidHeader = TRUE,
        width = 6,
        plotlyOutput(ns("forest_plot"), height = 400)
      ),
      
      # Predicted vs Observed
      box(
        title = "Predicted vs Observed",
        status = "warning",
        solidHeader = TRUE,
        width = 6,
        plotlyOutput(ns("pred_obs_plot"), height = 350)
      ),
      
      # Residuals
      box(
        title = "Residual Analysis",
        status = "danger",
        solidHeader = TRUE,
        width = 6,
        plotlyOutput(ns("residual_plot"), height = 350)
      ),
      
      # Model output
      box(
        title = "Model Summary",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        verbatimTextOutput(ns("model_summary"))
      )
    )
  )
}

riskServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive to store model results
    model_results <- reactiveVal(NULL)
    
    # Run model when button is clicked
    observeEvent(input$run_model, {
      df <- data()
      
      # Get unique patients (baseline data)
      df <- df %>%
        distinct(patient_id, .keep_all = TRUE)
      
      # Build formula
      predictors <- input$predictors
      if (length(predictors) == 0) {
        showNotification("Please select at least one predictor", type = "error")
        return()
      }
      
      if (input$model_type == "asthma") {
        # Logistic regression for asthma
        formula_str <- paste("asthma_diagnosis ~", paste(predictors, collapse = " + "))
        model <- glm(as.formula(formula_str), data = df, family = binomial(link = "logit"))
        
        # Calculate odds ratios
        coefs <- coef(model)
        se <- summary(model)$coefficients[, "Std. Error"]
        
        or <- exp(coefs)
        or_lower <- exp(coefs - 1.96 * se)
        or_upper <- exp(coefs + 1.96 * se)
        
        results <- list(
          model = model,
          type = "logistic",
          or_table = data.frame(
            variable = names(or),
            odds_ratio = or,
            ci_lower = or_lower,
            ci_upper = or_upper,
            p_value = summary(model)$coefficients[, "Pr(>|z|)"]
          ),
          predictions = predict(model, type = "response"),
          actual = as.numeric(df$asthma_diagnosis == "Yes")
        )
        
      } else {
        # Linear regression for lung function
        outcome <- input$outcome_measure
        formula_str <- paste(outcome, "~", paste(predictors, collapse = " + "))
        model <- lm(as.formula(formula_str), data = df)
        
        results <- list(
          model = model,
          type = "linear",
          coef_table = data.frame(
            variable = names(coef(model)),
            coefficient = coef(model),
            ci_lower = confint(model)[, 1],
            ci_upper = confint(model)[, 2],
            p_value = summary(model)$coefficients[, "Pr(>|t|)"]
          ),
          predictions = predict(model),
          actual = df[[outcome]],
          residuals = residuals(model)
        )
      }
      
      model_results(results)
    })
    
    # Coefficient plot
    output$coefficient_plot <- renderPlotly({
      req(model_results())
      
      results <- model_results()
      
      if (results$type == "logistic") {
        # Odds ratios
        df <- results$or_table[-1, ]  # Exclude intercept
        df$significant <- df$p_value < 0.05
        
        p <- ggplot(df, aes(x = reorder(variable, odds_ratio), y = odds_ratio)) +
          geom_bar(stat = "identity", aes(fill = significant)) +
          geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
          scale_fill_manual(values = c("TRUE" = "#27AE60", "FALSE" = "#95A5A6")) +
          coord_flip() +
          labs(title = "Odds Ratios",
               x = "Variable", y = "Odds Ratio") +
          theme_minimal() +
          theme(legend.position = "none")
      } else {
        # Coefficients
        df <- results$coef_table[-1, ]
        df$significant <- df$p_value < 0.05
        
        p <- ggplot(df, aes(x = reorder(variable, coefficient), y = coefficient)) +
          geom_bar(stat = "identity", aes(fill = significant)) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
          scale_fill_manual(values = c("TRUE" = "#27AE60", "FALSE" = "#95A5A6")) +
          coord_flip() +
          labs(title = "Regression Coefficients",
               x = "Variable", y = "Coefficient") +
          theme_minimal() +
          theme(legend.position = "none")
      }
      
      ggplotly(p)
    })
    
    # Forest plot
    output$forest_plot <- renderPlotly({
      req(model_results())
      
      results <- model_results()
      
      if (results$type == "logistic") {
        df <- results$or_table[-1, ]
        
        p <- ggplot(df, aes(x = odds_ratio, y = reorder(variable, odds_ratio))) +
          geom_point(size = 3, color = "#2E86AB") +
          geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
          geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
          labs(title = "Odds Ratios with 95% CI",
               x = "Odds Ratio", y = "Variable") +
          theme_minimal()
      } else {
        df <- results$coef_table[-1, ]
        
        p <- ggplot(df, aes(x = coefficient, y = reorder(variable, coefficient))) +
          geom_point(size = 3, color = "#2E86AB") +
          geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
          geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
          labs(title = "Coefficients with 95% CI",
               x = "Coefficient", y = "Variable") +
          theme_minimal()
      }
      
      ggplotly(p)
    })
    
    # Predicted vs Observed plot
    output$pred_obs_plot <- renderPlotly({
      req(model_results())
      
      results <- model_results()
      
      plot_data <- data.frame(
        observed = results$actual,
        predicted = results$predictions
      )
      
      if (results$type == "logistic") {
        # For logistic, show calibration plot
        plot_data$bin <- cut(plot_data$predicted, breaks = 10)
        calib_data <- plot_data %>%
          group_by(bin) %>%
          summarise(
            mean_pred = mean(predicted),
            mean_obs = mean(observed),
            n = n(),
            .groups = "drop"
          )
        
        p <- ggplot(calib_data, aes(x = mean_pred, y = mean_obs)) +
          geom_point(aes(size = n), alpha = 0.6) +
          geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
          labs(title = "Calibration Plot",
               x = "Mean Predicted Probability",
               y = "Observed Proportion") +
          theme_minimal()
      } else {
        # For linear, scatter plot
        p <- ggplot(plot_data, aes(x = observed, y = predicted)) +
          geom_point(alpha = 0.3) +
          geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
          geom_smooth(method = "lm", se = TRUE, color = "blue") +
          labs(title = "Predicted vs Observed",
               x = "Observed", y = "Predicted") +
          theme_minimal()
      }
      
      ggplotly(p)
    })
    
    # Residual plot
    output$residual_plot <- renderPlotly({
      req(model_results())
      
      results <- model_results()
      
      if (results$type == "linear") {
        plot_data <- data.frame(
          fitted = results$predictions,
          residuals = results$residuals
        )
        
        p <- ggplot(plot_data, aes(x = fitted, y = residuals)) +
          geom_point(alpha = 0.3) +
          geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
          geom_smooth(method = "loess", se = TRUE) +
          labs(title = "Residuals vs Fitted",
               x = "Fitted Values", y = "Residuals") +
          theme_minimal()
      } else {
        # For logistic, show ROC-like plot
        plot_data <- data.frame(
          predicted = results$predictions,
          actual = results$actual
        )
        
        # Sort by predicted probability
        plot_data <- plot_data[order(plot_data$predicted), ]
        plot_data$cumsum_actual <- cumsum(plot_data$actual) / sum(plot_data$actual)
        plot_data$cumsum_total <- 1:nrow(plot_data) / nrow(plot_data)
        
        p <- ggplot(plot_data, aes(x = cumsum_total, y = cumsum_actual)) +
          geom_line(size = 1, color = "#2E86AB") +
          geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
          labs(title = "Cumulative Gains Chart",
               x = "% of Population", y = "% of Cases Identified") +
          theme_minimal()
      }
      
      ggplotly(p)
    })
    
    # Performance table
    output$performance_table <- renderTable({
      req(model_results())
      
      results <- model_results()
      
      if (results$type == "logistic") {
        # Calculate AUC-like metric
        pred <- prediction(results$predictions, results$actual)
        perf <- performance(pred, "auc")
        auc <- perf@y.values[[1]]
        
        # Accuracy at 0.5 threshold
        pred_class <- ifelse(results$predictions > 0.5, 1, 0)
        accuracy <- mean(pred_class == results$actual)
        
        data.frame(
          Metric = c("AUC", "Accuracy (0.5 threshold)", "N"),
          Value = c(round(auc, 3), round(accuracy, 3), length(results$actual))
        )
      } else {
        # R-squared and RMSE
        r2 <- cor(results$actual, results$predictions)^2
        rmse <- sqrt(mean((results$actual - results$predictions)^2))
        mae <- mean(abs(results$actual - results$predictions))
        
        data.frame(
          Metric = c("R-squared", "RMSE", "MAE", "N"),
          Value = c(round(r2, 3), round(rmse, 3), round(mae, 3), length(results$actual))
        )
      }
    })
    
    # Model summary
    output$model_summary <- renderPrint({
      req(model_results())
      
      summary(model_results()$model)
    })
  })
}
