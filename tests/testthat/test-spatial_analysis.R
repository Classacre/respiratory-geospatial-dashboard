# Test spatial analysis functions
test_that("calculate_spatial_weights creates valid matrix", {
  data <- data.frame(
    latitude = c(-32.0, -31.9, -32.1),
    longitude = c(115.8, 115.9, 115.7)
  )

  W <- calculate_spatial_weights(data, bandwidth = 10)

  expect_type(W, "double")
  expect_equal(dim(W), c(3, 3))
  expect_equal(sum(diag(W)), 0)  # Diagonal should be 0
})

test_that("create_interpolation_grid creates valid grid", {
  bbox <- list(xmin = 115.7, xmax = 116.1, ymin = -32.5, ymax = -31.6)
  grid <- create_interpolation_grid(bbox, resolution = 5)

  expect_s3_class(grid, "data.frame")
  expect_true(all(c("longitude", "latitude") %in% names(grid)))
  expect_gt(nrow(grid), 10)
})

test_that("distance_to_nearest calculates correctly", {
  patients <- data.frame(lat = c(-32.0, -31.9), lon = c(115.8, 115.9))
  facilities <- data.frame(lat = c(-31.95), lon = c(115.85))

  dists <- distance_to_nearest(patients, facilities, "lat", "lon", "lat", "lon")

  expect_type(dists, "double")
  expect_length(dists, 2)
  expect_true(all(dists > 0))
})

test_that("aggregate_by_bins aggregates correctly", {
  data <- data.frame(
    lat = runif(100, -32.5, -31.6),
    lon = runif(100, 115.7, 116.1),
    value = rnorm(100)
  )

  binned <- aggregate_by_bins(data, "value", "lat", "lon", n_bins = 5)

  expect_s3_class(binned, "data.frame")
  expect_true("value" %in% names(binned))
  expect_true("lat_center" %in% names(binned))
})

# Test Monte Carlo hotspot testing
test_that("monte_carlo_hotspot_test returns correct structure", {
  set.seed(42)
  data <- data.frame(
    latitude = runif(50, -32.5, -31.6),
    longitude = runif(50, 115.7, 116.1),
    outcome = rpois(50, 5)
  )

  result <- monte_carlo_hotspot_test(data, "outcome", n_simulations = 99)

  expect_s3_class(result, "monte_carlo_hotspot")
  expect_true("data" %in% names(result))
  expect_true("summary" %in% names(result))
  expect_true("p_values" %in% names(result))
  expect_true("hotspot_class" %in% names(result))
  expect_true("local_stat" %in% names(result$data))
  expect_true("p_value" %in% names(result$data))
  expect_true("hotspot_class" %in% names(result$data))
})

test_that("monte_carlo_hotspot_test validates inputs", {
  data <- data.frame(x = 1:10, y = 1:10)

  expect_error(monte_carlo_hotspot_test(data, "nonexistent"),
               "outcome_col not found in data")
})

test_that("monte_carlo_hotspot_test print method works", {
  set.seed(42)
  data <- data.frame(
    latitude = runif(30, -32.5, -31.6),
    longitude = runif(30, 115.7, 116.1),
    outcome = rpois(30, 5)
  )

  result <- monte_carlo_hotspot_test(data, "outcome", n_simulations = 49)

  expect_output(print(result), "Monte Carlo Hotspot Test Results")
  expect_output(print(result), "Simulations:")
})

# Test Getis-Ord Gi* statistic
test_that("calculate_getis_ord_gi returns correct structure", {
  set.seed(42)
  data <- data.frame(
    latitude = runif(50, -32.5, -31.6),
    longitude = runif(50, 115.7, 116.1),
    outcome = rpois(50, 5)
  )

  result <- calculate_getis_ord_gi(data, "outcome", bandwidth = 5)

  expect_s3_class(result, "data.frame")
  expect_true("gi_star" %in% names(result))
  expect_true("gi_zscore" %in% names(result))
  expect_true("gi_pvalue" %in% names(result))
  expect_true("gi_classification" %in% names(result))
  expect_equal(nrow(result), nrow(data))
})

test_that("calculate_getis_ord_gi validates inputs", {
  data <- data.frame(x = 1:10, latitude = 1:10, longitude = 1:10)

  expect_error(calculate_getis_ord_gi(data, "nonexistent"),
               "outcome_col not found in data")
})

# Test spatial scan statistic
test_that("spatial_scan_statistic returns correct structure", {
  set.seed(42)
  data <- data.frame(
    latitude = runif(50, -32.5, -31.6),
    longitude = runif(50, 115.7, 116.1),
    cases = rpois(50, 3),
    population = rpois(50, 50) + 20
  )

  result <- spatial_scan_statistic(data, "cases", "population", n_simulations = 49)

  expect_s3_class(result, "spatial_scan")
  expect_true("primary_cluster" %in% names(result))
  expect_true("log_likelihood" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true("parameters" %in% names(result))
})

test_that("spatial_scan_statistic validates inputs", {
  data <- data.frame(x = 1:10, latitude = 1:10, longitude = 1:10)

  expect_error(spatial_scan_statistic(data, "nonexistent"),
               "case_col not found in data")
})

test_that("spatial_scan_statistic print method works", {
  set.seed(42)
  data <- data.frame(
    latitude = runif(30, -32.5, -31.6),
    longitude = runif(30, 115.7, 116.1),
    cases = rpois(30, 3),
    population = rpois(30, 50) + 20
  )

  result <- spatial_scan_statistic(data, "cases", "population", n_simulations = 49)

  expect_output(print(result), "Spatial Scan Statistic Results")
})

# Test variogram analysis
test_that("variogram_analysis returns correct structure", {
  set.seed(42)
  data <- data.frame(
    latitude = runif(50, -32.5, -31.6),
    longitude = runif(50, 115.7, 116.1),
    outcome = rnorm(50, 12, 3)
  )

  result <- variogram_analysis(data, "outcome", max_dist = 15, n_lags = 10)

  expect_s3_class(result, "variogram_analysis")
  expect_true("empirical" %in% names(result))
  expect_true("fitted_model" %in% names(result))
  expect_true("parameters" %in% names(result))
  expect_true("spatial_dependence" %in% names(result))
  expect_true("nugget" %in% names(result$parameters))
  expect_true("sill" %in% names(result$parameters))
  expect_true("range" %in% names(result$parameters))
})

test_that("variogram_analysis validates inputs", {
  data <- data.frame(x = 1:10, latitude = 1:10, longitude = 1:10)

  expect_error(variogram_analysis(data, "nonexistent"),
               "outcome_col not found in data")
})

test_that("variogram_analysis print method works", {
  set.seed(42)
  data <- data.frame(
    latitude = runif(30, -32.5, -31.6),
    longitude = runif(30, 115.7, 116.1),
    outcome = rnorm(30, 12, 3)
  )

  result <- variogram_analysis(data, "outcome", max_dist = 10, n_lags = 5)

  expect_output(print(result), "Variogram Analysis Results")
  expect_output(print(result), "Model type:")
})

# Test dasymetric mapping
test_that("dasymetric_mapping returns correct structure", {
  data <- data.frame(
    longitude = c(115.8, 115.9, 116.0),
    latitude = c(-32.0, -31.9, -32.1),
    value = c(10, 15, 12)
  )

  bbox <- list(xmin = 115.7, xmax = 116.1, ymin = -32.5, ymax = -31.6)
  grid <- create_interpolation_grid(bbox, resolution = 2)

  result <- dasymetric_mapping(data, "value", grid)

  expect_s3_class(result, "data.frame")
  expect_true("interpolated" %in% names(result))
  expect_true("method" %in% names(result))
  expect_equal(result$method[1], "dasymetric")
})

test_that("dasymetric_mapping validates inputs", {
  grid <- data.frame(longitude = 115.8, latitude = -32.0)

  expect_error(dasymetric_mapping(data.frame(x = 1), "value", grid),
               "value_col not found in data")
})

# Test Moran's I
test_that("calculate_morans_i returns valid results", {
  set.seed(42)
  x <- rnorm(30)
  W <- matrix(runif(900), 30, 30)
  diag(W) <- 0
  W <- W / rowSums(W)

  result <- calculate_morans_i(x, W)

  expect_type(result, "list")
  expect_true("moran_i" %in% names(result))
  expect_true("expected_i" %in% names(result))
  expect_true("z_score" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_type(result$moran_i, "double")
  expect_type(result$p_value, "double")
})

# Test hotspot detection
test_that("detect_hotspots returns correct structure", {
  set.seed(42)
  data <- data.frame(
    latitude = runif(50, -32.5, -31.6),
    longitude = runif(50, 115.7, 116.1),
    outcome = rpois(50, 5)
  )

  result <- detect_hotspots(data, "outcome", bandwidth = 5)

  expect_s3_class(result, "data.frame")
  expect_true("local_stat" %in% names(result))
  expect_true("hotspot_class" %in% names(result))
  expect_true(all(result$hotspot_class %in% c("Not Significant", "Hotspot (High-High)", "Coldspot (Low-Low)")))
})

# Test IDW interpolation
test_that("idw_interpolate returns correct structure", {
  data <- data.frame(
    longitude = c(115.8, 115.9, 116.0),
    latitude = c(-32.0, -31.9, -32.1),
    value = c(10, 15, 12)
  )

  bbox <- list(xmin = 115.7, xmax = 116.1, ymin = -32.5, ymax = -31.6)
  grid <- create_interpolation_grid(bbox, resolution = 2)

  result <- idw_interpolate(data, "value", grid)

  expect_s3_class(result, "data.frame")
  expect_true("interpolated" %in% names(result))
  expect_equal(nrow(result), nrow(grid))
})
