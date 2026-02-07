#' Risk Modeling Functions for Respiratory Health
#'
#' This file contains functions for mixed-effects modeling of lung function
#' trajectories, risk prediction models, and risk stratification.

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

#' Calculate Composite Opportunity Index
#'
#' Creates a composite index combining socioeconomic, environmental, and
#' healthcare access factors. Based on Beck et al. (2017) Child Opportunity Index.
#'
#' @param data Data frame with patient or area-level data
#' @param ses_col Name of SES score column (default: "ses_score")
#' @param pm25_col Name of PM2.5 column (default: "pm25")
#' @param pollen_col Name of pollen index column (default: "pollen_index")
#' @param distance_col Name of distance to healthcare column (default: "distance_to_station")
#' @param income_col Name of income proxy column (optional)
#' @param weights Named vector of weights for each component
#'   (default: c(ses = 0.3, environment = 0.4, healthcare = 0.3))
#' @param higher_is_better Named vector indicating direction for each component
#'   (default: c(ses = TRUE, pm25 = FALSE, pollen = FALSE, distance = FALSE))
#' @return Data frame with opportunity index and component scores
#' @export
#'
#' @references
#' Beck, A. F., Huang, B., Wheeler, K., et al. (2017). The Child Opportunity
#' Index and Disparities in Pediatric Asthma Hospitalizations across one Ohio
#' Metropolitan Area, 2011-2013. The Journal of Pediatrics, 190:200-206.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   ses_score = rnorm(100, 50, 15),
#'   pm25 = rnorm(100, 12, 3),
#'   pollen_index = runif(100, 0, 10),
#'   distance_to_station = runif(100, 1, 20)
#' )
#' result <- calculate_opportunity_index(data)
#' }
calculate_opportunity_index <- function(data, ses_col = "ses_score",
                                         pm25_col = "pm25",
                                         pollen_col = "pollen_index",
                                         distance_col = "distance_to_station",
                                         income_col = NULL,
                                         weights = c(ses = 0.3, environment = 0.4, healthcare = 0.3),
                                         higher_is_better = c(ses = TRUE, pm25 = FALSE,
                                                              pollen = FALSE, distance = FALSE)) {
  # Input validation
  if (!ses_col %in% names(data)) {
    stop("ses_col not found in data")
  }
  if (!pm25_col %in% names(data)) {
    stop("pm25_col not found in data")
  }
  if (!pollen_col %in% names(data)) {
    stop("pollen_col not found in data")
  }
  if (!distance_col %in% names(data)) {
    stop("distance_col not found in data")
  }

  # Normalize all components to 0-100 scale
  normalize_score <- function(x, higher_better = TRUE) {
    # Handle missing values
    x <- ifelse(is.na(x), median(x, na.rm = TRUE), x)

    # Min-max normalization
    min_x <- min(x, na.rm = TRUE)
    max_x <- max(x, na.rm = TRUE)

    if (max_x == min_x) {
      return(rep(50, length(x)))
    }

    normalized <- (x - min_x) / (max_x - min_x) * 100

    # Invert if lower values are better
    if (!higher_better) {
      normalized <- 100 - normalized
    }

    return(normalized)
  }

  # Calculate component scores
  result <- data

  # SES component (higher SES = better opportunity)
  result$ses_component <- normalize_score(data[[ses_col]], higher_better = TRUE)

  # Add income if available
  if (!is.null(income_col) && income_col %in% names(data)) {
    income_score <- normalize_score(data[[income_col]], higher_better = TRUE)
    result$ses_component <- (result$ses_component + income_score) / 2
  }

  # Environmental component (lower PM2.5 and pollen = better)
  pm25_score <- normalize_score(data[[pm25_col]], higher_better = FALSE)
  pollen_score <- normalize_score(data[[pollen_col]], higher_better = FALSE)
  result$environment_component <- (pm25_score + pollen_score) / 2

  # Healthcare access component (closer to station = better)
  result$healthcare_component <- normalize_score(data[[distance_col]], higher_better = FALSE)

  # Calculate composite opportunity index
  result$opportunity_index <- (
    weights["ses"] * result$ses_component +
      weights["environment"] * result$environment_component +
      weights["healthcare"] * result$healthcare_component
  )

  # Create opportunity quintiles
  result$opportunity_quintile <- cut(
    result$opportunity_index,
    breaks = quantile(result$opportunity_index, probs = seq(0, 1, 0.2), na.rm = TRUE),
    labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4", "Q5 (Highest)"),
    include.lowest = TRUE
  )

  return(result)
}

#' Stratify Patients into Risk Tiers
#'
#' Categorizes patients into risk tiers (low/medium/high) based on a
#' composite risk score combining environmental, demographic, and clinical factors.
#'
#' @param data Data frame with patient data
#' @param environmental_vars Character vector of environmental variable names
#' @param demographic_vars Character vector of demographic variable names
#' @param clinical_vars Character vector of clinical variable names
#' @param outcome_var Name of outcome variable for validation (optional)
#' @param tier_thresholds Numeric vector of percentiles for tier cutoffs
#'   (default: c(0.33, 0.67) for three tiers)
#' @param tier_labels Character vector of tier labels
#'   (default: c("Low", "Medium", "High"))
#' @param weights Named list of weights for each component category
#'   (default: list(environmental = 0.3, demographic = 0.3, clinical = 0.4))
#' @return Data frame with risk scores and tier classifications
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   pm25 = rnorm(100, 12, 3),
#'   pollen_index = runif(100, 0, 10),
#'   age_years = runif(100, 5, 15),
#'   ses_score = rnorm(100, 50, 15),
#'   exacerbation_count = rpois(100, 2),
#'   hospitalization_count = rpois(100, 0.5)
#' )
#' result <- stratify_risk(
#'   data,
#'   environmental_vars = c("pm25", "pollen_index"),
#'   demographic_vars = c("age_years", "ses_score"),
#'   clinical_vars = c("exacerbation_count", "hospitalization_count")
#' )
#' }
stratify_risk <- function(data, environmental_vars = NULL,
                          demographic_vars = NULL, clinical_vars = NULL,
                          outcome_var = NULL,
                          tier_thresholds = c(0.33, 0.67),
                          tier_labels = c("Low", "Medium", "High"),
                          weights = list(environmental = 0.3, demographic = 0.3, clinical = 0.4)) {
  # Input validation
  if (is.null(environmental_vars) && is.null(demographic_vars) && is.null(clinical_vars)) {
    stop("At least one variable category must be provided")
  }

  result <- data

  # Helper function to calculate component score
  calculate_component_score <- function(vars, data) {
    if (is.null(vars) || length(vars) == 0) {
      return(NULL)
    }

    # Check all variables exist
    missing_vars <- setdiff(vars, names(data))
    if (length(missing_vars) > 0) {
      stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
    }

    # Standardize each variable (z-score)
    standardized <- sapply(vars, function(var) {
      x <- data[[var]]
      (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    })

    # Average across variables
    rowMeans(standardized, na.rm = TRUE)
  }

  # Calculate component scores
  env_score <- calculate_component_score(environmental_vars, data)
  demo_score <- calculate_component_score(demographic_vars, data)
  clin_score <- calculate_component_score(clinical_vars, data)

  # Calculate composite risk score
  total_weight <- 0
  weighted_sum <- rep(0, nrow(data))

  if (!is.null(env_score)) {
    weighted_sum <- weighted_sum + weights$environmental * env_score
    total_weight <- total_weight + weights$environmental
    result$environmental_risk_score <- env_score
  }

  if (!is.null(demo_score)) {
    weighted_sum <- weighted_sum + weights$demographic * demo_score
    total_weight <- total_weight + weights$demographic
    result$demographic_risk_score <- demo_score
  }

  if (!is.null(clin_score)) {
    weighted_sum <- weighted_sum + weights$clinical * clin_score
    total_weight <- total_weight + weights$clinical
    result$clinical_risk_score <- clin_score
  }

  # Normalize by total weight
  result$composite_risk_score <- weighted_sum / total_weight

  # Create risk tiers
  risk_quantiles <- quantile(result$composite_risk_score,
                              probs = c(0, tier_thresholds, 1),
                              na.rm = TRUE)

  result$risk_tier <- cut(
    result$composite_risk_score,
    breaks = risk_quantiles,
    labels = tier_labels,
    include.lowest = TRUE
  )

  # Calculate tier statistics
  tier_summary <- result %>%
    dplyr::group_by(risk_tier) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean_risk_score = mean(composite_risk_score, na.rm = TRUE),
      .groups = "drop"
    )

  # Validate against outcome if provided
  validation <- NULL
  if (!is.null(outcome_var) && outcome_var %in% names(data)) {
    validation <- result %>%
      dplyr::group_by(risk_tier) %>%
      dplyr::summarise(
        mean_outcome = mean(.data[[outcome_var]], na.rm = TRUE),
        outcome_rate = sum(.data[[outcome_var]], na.rm = TRUE) / dplyr::n(),
        .groups = "drop"
      )
  }

  structure(list(
    data = result,
    tier_summary = tier_summary,
    validation = validation,
    parameters = list(
      environmental_vars = environmental_vars,
      demographic_vars = demographic_vars,
      clinical_vars = clinical_vars,
      weights = weights,
      tier_thresholds = tier_thresholds
    )
  ), class = "risk_stratification")
}

#' Plot Opportunity Index Results
#'
#' Visualizes the relationship between opportunity index and health outcomes.
#'
#' @param data Data frame with opportunity_index and outcome variables
#' @param outcome_col Name of outcome column
#' @param opportunity_col Name of opportunity index column (default: "opportunity_index")
#' @return ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_opportunity_outcome(data, "asthma_diagnosis")
#' }
plot_opportunity_outcome <- function(data, outcome_col,
                                      opportunity_col = "opportunity_index") {
  if (!outcome_col %in% names(data)) {
    stop("outcome_col not found in data")
  }
  if (!opportunity_col %in% names(data)) {
    stop("opportunity_col not found in data")
  }

  # Create binned opportunity index for visualization
  data$opportunity_bin <- cut(data[[opportunity_col]],
                               breaks = 5,
                               labels = c("Very Low", "Low", "Medium", "High", "Very High"))

  # Calculate outcome by bin
  summary_data <- data %>%
    dplyr::group_by(opportunity_bin) %>%
    dplyr::summarise(
      mean_outcome = mean(.data[[outcome_col]], na.rm = TRUE),
      se = sd(.data[[outcome_col]], na.rm = TRUE) / sqrt(dplyr::n()),
      n = dplyr::n(),
      .groups = "drop"
    )

  p <- ggplot2::ggplot(summary_data, ggplot2::aes(x = opportunity_bin, y = mean_outcome)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = mean_outcome - se, ymax = mean_outcome + se),
                           width = 0.2) +
    ggplot2::labs(
      title = "Health Outcomes by Opportunity Index",
      subtitle = paste0("Outcome: ", outcome_col),
      x = "Opportunity Level",
      y = paste0("Mean ", outcome_col)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  return(p)
}

#' Print Method for Risk Stratification Results
#'
#' @param x Object of class "risk_stratification"
#' @param ... Additional arguments
#' @export
print.risk_stratification <- function(x, ...) {
  cat("Risk Stratification Results\n")
  cat("===========================\n\n")

  cat("Tier Distribution:\n")
  print(x$tier_summary)

  if (!is.null(x$validation)) {
    cat("\nValidation by Outcome:\n")
    print(x$validation)
  }
}

#' Summary Method for Risk Stratification
#'
#' @param object Object of class "risk_stratification"
#' @param ... Additional arguments
#' @export
summary.risk_stratification <- function(object, ...) {
  cat("Risk Stratification Summary\n")
  cat("===========================\n\n")

  cat("Parameters:\n")
  cat("  Environmental variables:",
      paste(object$parameters$environmental_vars, collapse = ", "), "\n")
  cat("  Demographic variables:",
      paste(object$parameters$demographic_vars, collapse = ", "), "\n")
  cat("  Clinical variables:",
      paste(object$parameters$clinical_vars, collapse = ", "), "\n\n")

  cat("Weights:\n")
  for (name in names(object$parameters$weights)) {
    cat("  ", name, ": ", object$parameters$weights[[name]], "\n", sep = "")
  }

  cat("\nRisk Score Distribution:\n")
  print(summary(object$data$composite_risk_score))
}
