#' Risk Modeling Functions for Respiratory Health
#'
#' This file contains functions for mixed-effects modeling of lung function
#' trajectories and risk prediction models.

#' Fit Mixed-Effects Model for Lung Function Trajectories
#'
#' Fits a linear mixed-effects model to model lung function over time
#' with random effects for individual patients.
#'
#' @param data Data frame with longitudinal lung function data
#' @param outcome Outcome variable name (e.g., "fev1" or "fvc")
#' @param fixed_effects Character vector of fixed effect variable names
#' @param random_effects Character vector of random effect variable names
#' @param time_var Time variable name (default: "age_years")
#' @param id_var Patient ID variable name (default: "patient_id")
#' @return A fitted lmer model object
#' @export
#'
#' @examples
#' \dontrun{
#' model <- fit_lung_function_model(
#'   data = lung_data,
#'   outcome = "fev1",
#'   fixed_effects = c("sex", "pm25", "ics_use"),
#'   random_effects = c("age_years")
#' )
#' }
fit_lung_function_model <- function(data, outcome, fixed_effects,
                                     random_effects = c("age_years"),
                                     time_var = "age_years",
                                     id_var = "patient_id") {
  
  # Ensure required packages
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' is required but not installed.")
  }
  
  # Build formula
  fixed_part <- paste(fixed_effects, collapse = " + ")
  if (time_var %in% fixed_effects) {
    # Add polynomial time if not already included
    if (!paste0("I(", time_var, "^2)") %in% fixed_effects) {
      fixed_part <- paste(fixed_part, "+ I(", time_var, "^2)")
    }
  } else {
    fixed_part <- paste(fixed_part, "+", time_var, "+ I(", time_var, "^2)")
  }
  
  random_part <- paste("(", paste(random_effects, collapse = " + "), "|", id_var, ")")
  formula_str <- paste(outcome, "~", fixed_part, "+", random_part)
  
  # Fit model
  model <- lme4::lmer(as.formula(formula_str), data = data, 
                       control = lme4::lmerControl(optimizer = "bobyqa"))
  
  return(model)
}

#' Fit Logistic Model for Asthma Risk
#'
#' Fits a logistic regression model for asthma diagnosis risk.
#'
#' @param data Data frame with patient data
#' @param predictors Character vector of predictor variable names
#' @return A fitted glm object
#' @export
#'
#' @examples
#' \dontrun{
#' model <- fit_asthma_risk_model(
#'   data = patient_data,
#'   predictors = c("pm25", "pollen_index", "ses_score", "sex")
#' )
#' }
fit_asthma_risk_model <- function(data, predictors) {
  # Build formula
  formula_str <- paste("asthma_diagnosis ~", paste(predictors, collapse = " + "))
  
  # Fit model
  model <- glm(as.formula(formula_str), data = data, family = binomial(link = "logit"))
  
  return(model)
}

#' Calculate Odds Ratios with Confidence Intervals
#'
#' Extracts odds ratios and confidence intervals from a logistic model.
#'
#' @param model A fitted glm model object
#' @param conf_level Confidence level (default: 0.95)
#' @return Data frame with odds ratios and CIs
#' @export
#'
#' @examples
#' \dontrun{
#' model <- glm(asthma ~ pm25 + sex, data = data, family = binomial)
#' or_table <- calculate_odds_ratios(model)
#' }
calculate_odds_ratios <- function(model, conf_level = 0.95) {
  coefs <- coef(model)
  se <- summary(model)$coefficients[, "Std. Error"]
  
  alpha <- 1 - conf_level
  z <- qnorm(1 - alpha / 2)
  
  or <- exp(coefs)
  or_lower <- exp(coefs - z * se)
  or_upper <- exp(coefs + z * se)
  
  p_values <- summary(model)$coefficients[, "Pr(>|z|)"]
  
  result <- data.frame(
    variable = names(coefs),
    odds_ratio = round(or, 3),
    ci_lower = round(or_lower, 3),
    ci_upper = round(or_upper, 3),
    p_value = format.pval(p_values, eps = 0.001),
    significant = p_values < 0.05
  )
  
  return(result)
}

#' Predict Lung Function Trajectories
#'
#' Generates predictions from a fitted mixed-effects model for new data.
#'
#' @param model Fitted lmer model object
#' @param newdata New data for prediction
#' @param include_random Include random effects (default: FALSE for population predictions)
#' @return Vector of predicted values
#' @export
#'
#' @examples
#' \dontrun{
#' preds <- predict_lung_function(model, newdata = test_data)
#' }
predict_lung_function <- function(model, newdata, include_random = FALSE) {
  if (include_random) {
    preds <- predict(model, newdata = newdata, allow.new.levels = TRUE)
  } else {
    preds <- predict(model, newdata = newdata, re.form = NA)
  }
  return(preds)
}

#' Calculate Growth-Adjusted Percentiles
#'
#' Calculates percentiles for lung function measures adjusted for age and sex.
#'
#' @param data Data frame with lung function data
#' @param measure Lung function measure ("fev1", "fvc", or "fev1_fvc_ratio")
#' @param age_col Age column name
#' @param sex_col Sex column name
#' @return Vector of percentiles (0-100)
#' @export
#'
#' @examples
#' \dontrun{
#' data$fev1_percentile <- calculate_growth_percentiles(
#'   data, "fev1", "age_years", "sex"
#' )
#' }
calculate_growth_percentiles <- function(data, measure, age_col = "age_years",
                                          sex_col = "sex") {
  
  # Create age-sex groups
  data$age_group <- cut(data[[age_col]], breaks = seq(2, 18, by = 1))
  
  # Calculate percentiles within each group
  percentiles <- ave(data[[measure]], data[[sex_col]], data$age_group,
                     FUN = function(x) {
                       if (length(x) < 5) return(rep(NA, length(x)))
                       ecdf(x)(x) * 100
                     })
  
  return(percentiles)
}

#' Model Performance Metrics
#'
#' Calculates common performance metrics for regression models.
#'
#' @param actual Actual values
#' @param predicted Predicted values
#' @return List of performance metrics
#' @export
#'
#' @examples
#' \dontrun{
#' metrics <- model_metrics(test_data$fev1, predictions)
#' }
model_metrics <- function(actual, predicted) {
  # Remove NAs
  valid <- !is.na(actual) & !is.na(predicted)
  actual <- actual[valid]
  predicted <- predicted[valid]
  
  residuals <- actual - predicted
  
  metrics <- list(
    rmse = sqrt(mean(residuals^2)),
    mae = mean(abs(residuals)),
    mape = mean(abs(residuals / actual)) * 100,
    r_squared = 1 - sum(residuals^2) / sum((actual - mean(actual))^2),
    correlation = cor(actual, predicted)
  )
  
  return(metrics)
}

#' Cross-Validate Model
#'
#' Performs k-fold cross-validation for model assessment.
#'
#' @param data Data frame
#' @param formula Model formula
#' @param k Number of folds (default: 5)
#' @param family Model family (default: gaussian)
#' @return List with CV predictions and metrics
#' @export
#'
#' @examples
#' \dontrun{
#' cv_results <- cross_validate_model(
#'   data, fev1 ~ age_years + sex + pm25, k = 5
#' )
#' }
cross_validate_model <- function(data, formula, k = 5, family = gaussian()) {
  # Create folds
  set.seed(42)
  folds <- sample(rep(1:k, length.out = nrow(data)))
  
  predictions <- numeric(nrow(data))
  
  for (i in 1:k) {
    train_data <- data[folds != i, ]
    test_data <- data[folds == i, ]
    
    model <- glm(formula, data = train_data, family = family)
    predictions[folds == i] <- predict(model, newdata = test_data, type = "response")
  }
  
  # Calculate metrics
  actual <- model.response(model.frame(formula, data = data))
  metrics <- model_metrics(actual, predictions)
  
  return(list(
    predictions = predictions,
    metrics = metrics
  ))
}

#' Create Prediction Summary Table
#'
#' Summarizes model predictions by subgroups.
#'
#' @param data Data frame with predictions
#' @param prediction_col Prediction column name
#' @param actual_col Actual outcome column name
#' @param group_cols Character vector of grouping columns
#' @return Summary data frame
#' @export
#'
#' @examples
#' \dontrun{
#' summary <- prediction_summary(
#'   data, "predicted_fev1", "fev1",
#'   group_cols = c("sex", "age_group")
#' )
#' }
prediction_summary <- function(data, prediction_col, actual_col,
                                group_cols = c("sex")) {
  
  formula_str <- paste(c(actual_col, prediction_col), collapse = " + ")
  formula_str <- paste("cbind(", formula_str, ") ~", paste(group_cols, collapse = " + "))
  
  summary_stats <- aggregate(
    as.formula(formula_str),
    data = data,
    FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
  )
  
  return(summary_stats)
}
