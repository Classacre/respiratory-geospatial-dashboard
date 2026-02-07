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
