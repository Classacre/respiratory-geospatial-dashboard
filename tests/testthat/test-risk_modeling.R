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
