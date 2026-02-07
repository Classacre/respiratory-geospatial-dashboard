# Test risk modeling functions
test_that("calculate_odds_ratios returns correct structure", {
  # Create simple logistic model
  set.seed(123)
  data <- data.frame(
    y = factor(rbinom(100, 1, 0.3)),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  model <- glm(y ~ x1 + x2, data = data, family = binomial)

  or_table <- calculate_odds_ratios(model)

  expect_s3_class(or_table, "data.frame")
  expect_true(all(c("variable", "odds_ratio", "ci_lower", "ci_upper", "p_value") %in% names(or_table)))
  expect_equal(nrow(or_table), 3)  # Intercept + 2 predictors
})

test_that("model_metrics calculates correctly", {
  actual <- c(1, 2, 3, 4, 5)
  predicted <- c(1.1, 1.9, 3.2, 3.8, 5.1)

  metrics <- model_metrics(actual, predicted)

  expect_type(metrics, "list")
  expect_true(all(c("rmse", "mae", "r_squared", "correlation") %in% names(metrics)))
  expect_gt(metrics$r_squared, 0.9)  # Should be high correlation
})

test_that("calculate_growth_percentiles returns valid percentiles", {
  data <- data.frame(
    age_years = rep(5:10, each = 20),
    sex = rep(c("Male", "Female"), 60),
    fev1 = rnorm(120, 1.5, 0.3)
  )

  percentiles <- calculate_growth_percentiles(data, "fev1", "age_years", "sex")

  expect_type(percentiles, "double")
  expect_length(percentiles, nrow(data))
  expect_true(all(percentiles >= 0 & percentiles <= 100, na.rm = TRUE))
})

# Test opportunity index calculation
test_that("calculate_opportunity_index returns correct structure", {
  data <- data.frame(
    ses_score = rnorm(100, 50, 15),
    pm25 = rnorm(100, 12, 3),
    pollen_index = runif(100, 0, 10),
    distance_to_station = runif(100, 1, 20)
  )

  result <- calculate_opportunity_index(data)

  expect_s3_class(result, "data.frame")
  expect_true("opportunity_index" %in% names(result))
  expect_true("opportunity_quintile" %in% names(result))
  expect_true("ses_component" %in% names(result))
  expect_true("environment_component" %in% names(result))
  expect_true("healthcare_component" %in% names(result))
  expect_true(all(result$opportunity_index >= 0 & result$opportunity_index <= 100, na.rm = TRUE))
})

test_that("calculate_opportunity_index validates inputs", {
  data <- data.frame(x = 1:10)

  expect_error(calculate_opportunity_index(data), "ses_col not found in data")

  data$ses_score <- 1:10
  expect_error(calculate_opportunity_index(data), "pm25_col not found in data")
})

test_that("calculate_opportunity_index handles optional income column", {
  data <- data.frame(
    ses_score = rnorm(50, 50, 15),
    pm25 = rnorm(50, 12, 3),
    pollen_index = runif(50, 0, 10),
    distance_to_station = runif(50, 1, 20),
    income = rnorm(50, 60000, 15000)
  )

  result <- calculate_opportunity_index(data, income_col = "income")

  expect_s3_class(result, "data.frame")
  expect_true("opportunity_index" %in% names(result))
})

# Test risk stratification
test_that("stratify_risk returns correct structure", {
  set.seed(42)
  data <- data.frame(
    pm25 = rnorm(100, 12, 3),
    pollen_index = runif(100, 0, 10),
    age_years = runif(100, 5, 15),
    ses_score = rnorm(100, 50, 15),
    exacerbation_count = rpois(100, 2),
    hospitalization_count = rpois(100, 0.5),
    longitude = runif(100, 115.7, 116.1),
    latitude = runif(100, -32.5, -31.6)
  )

  result <- stratify_risk(
    data,
    environmental_vars = c("pm25", "pollen_index"),
    demographic_vars = c("age_years", "ses_score"),
    clinical_vars = c("exacerbation_count", "hospitalization_count")
  )

  expect_s3_class(result, "risk_stratification")
  expect_true("data" %in% names(result))
  expect_true("tier_summary" %in% names(result))
  expect_true("parameters" %in% names(result))
  expect_true("composite_risk_score" %in% names(result$data))
  expect_true("risk_tier" %in% names(result$data))
  expect_equal(length(unique(result$data$risk_tier)), 3)
})

test_that("stratify_risk validates inputs", {
  data <- data.frame(x = 1:10)

  expect_error(stratify_risk(data), "At least one variable category must be provided")
  expect_error(
    stratify_risk(data, environmental_vars = c("nonexistent")),
    "Variables not found in data"
  )
})

test_that("stratify_risk handles single variable category", {
  data <- data.frame(
    pm25 = rnorm(50, 12, 3),
    longitude = runif(50, 115.7, 116.1),
    latitude = runif(50, -32.5, -31.6)
  )

  result <- stratify_risk(
    data,
    environmental_vars = c("pm25")
  )

  expect_s3_class(result, "risk_stratification")
  expect_true("composite_risk_score" %in% names(result$data))
})

test_that("stratify_risk with outcome validation works", {
  set.seed(42)
  data <- data.frame(
    pm25 = rnorm(100, 12, 3),
    age_years = runif(100, 5, 15),
    exacerbation_count = rpois(100, 2),
    asthma_diagnosis = rbinom(100, 1, 0.3),
    longitude = runif(100, 115.7, 116.1),
    latitude = runif(100, -32.5, -31.6)
  )

  result <- stratify_risk(
    data,
    environmental_vars = c("pm25"),
    demographic_vars = c("age_years"),
    clinical_vars = c("exacerbation_count"),
    outcome_var = "asthma_diagnosis"
  )

  expect_s3_class(result, "risk_stratification")
  expect_true("validation" %in% names(result))
  expect_false(is.null(result$validation))
})

test_that("stratify_risk print method works", {
  set.seed(42)
  data <- data.frame(
    pm25 = rnorm(50, 12, 3),
    age_years = runif(50, 5, 15),
    exacerbation_count = rpois(50, 2),
    longitude = runif(50, 115.7, 116.1),
    latitude = runif(50, -32.5, -31.6)
  )

  result <- stratify_risk(
    data,
    environmental_vars = c("pm25"),
    demographic_vars = c("age_years"),
    clinical_vars = c("exacerbation_count")
  )

  expect_output(print(result), "Risk Stratification Results")
  expect_output(print(result), "Tier Distribution:")
})

test_that("stratify_risk summary method works", {
  set.seed(42)
  data <- data.frame(
    pm25 = rnorm(50, 12, 3),
    age_years = runif(50, 5, 15),
    exacerbation_count = rpois(50, 2),
    longitude = runif(50, 115.7, 116.1),
    latitude = runif(50, -32.5, -31.6)
  )

  result <- stratify_risk(
    data,
    environmental_vars = c("pm25"),
    demographic_vars = c("age_years"),
    clinical_vars = c("exacerbation_count")
  )

  expect_output(summary(result), "Risk Stratification Summary")
  expect_output(summary(result), "Parameters:")
  expect_output(summary(result), "Weights:")
})

# Test plot_opportunity_outcome
test_that("plot_opportunity_outcome returns ggplot object", {
  set.seed(42)
  data <- data.frame(
    opportunity_index = runif(100, 0, 100),
    asthma_diagnosis = rbinom(100, 1, 0.3),
    longitude = runif(100, 115.7, 116.1),
    latitude = runif(100, -32.5, -31.6)
  )

  p <- plot_opportunity_outcome(data, "asthma_diagnosis")

  expect_s3_class(p, "ggplot")
})

test_that("plot_opportunity_outcome validates inputs", {
  data <- data.frame(x = 1:10, opportunity_index = 1:10)

  expect_error(plot_opportunity_outcome(data, "nonexistent"),
               "outcome_col not found in data")

  data$y <- 1:10
  expect_error(plot_opportunity_outcome(data, "y", opportunity_col = "nonexistent"),
               "opportunity_col not found in data")
})

# Test cross-validation
test_that("cross_validate_model returns correct structure", {
  set.seed(42)
  data <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )

  result <- cross_validate_model(data, y ~ x1 + x2, k = 5)

  expect_type(result, "list")
  expect_true("predictions" %in% names(result))
  expect_true("metrics" %in% names(result))
  expect_true("rmse" %in% names(result$metrics))
  expect_length(result$predictions, 100)
})

# Test prediction summary
test_that("prediction_summary returns correct structure", {
  set.seed(42)
  data <- data.frame(
    predicted = rnorm(100),
    actual = rnorm(100),
    sex = sample(c("Male", "Female"), 100, replace = TRUE)
  )

  result <- prediction_summary(data, "predicted", "actual", group_cols = c("sex"))

  expect_s3_class(result, "data.frame")
})
