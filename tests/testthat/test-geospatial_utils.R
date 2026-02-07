# Test geospatial utility functions
test_that("calculate_travel_time returns correct structure", {
  patients <- data.frame(
    patient_id = 1:10,
    latitude = runif(10, -32.5, -31.6),
    longitude = runif(10, 115.7, 116.1)
  )

  facilities <- data.frame(
    facility_id = 1:3,
    facility_name = c("Hospital A", "Clinic B", "ER C"),
    latitude = c(-31.95, -32.1, -31.8),
    longitude = c(115.85, 115.9, 115.75)
  )

  result <- calculate_travel_time(patients, facilities)

  expect_s3_class(result, "data.frame")
  expect_true("distance_to_facility_km" %in% names(result))
  expect_true("travel_time_minutes" %in% names(result))
  expect_true("nearest_facility_idx" %in% names(result))
  expect_true("nearest_facility" %in% names(result))
  expect_equal(nrow(result), nrow(patients))
  expect_true(all(result$travel_time_minutes > 0))
  expect_true(all(result$distance_to_facility_km > 0))
})

test_that("calculate_travel_time validates inputs", {
  patients <- data.frame(x = 1:10)
  facilities <- data.frame(latitude = 1:3, longitude = 1:3)

  expect_error(calculate_travel_time(patients, facilities),
               "Patient coordinate columns not found")

  patients <- data.frame(latitude = 1:10, longitude = 1:10)
  facilities <- data.frame(x = 1:3)

  expect_error(calculate_travel_time(patients, facilities),
               "Facility coordinate columns not found")
})

test_that("calculate_travel_time handles different modes", {
  patients <- data.frame(
    patient_id = 1:5,
    latitude = runif(5, -32.5, -31.6),
    longitude = runif(5, 115.7, 116.1)
  )

  facilities <- data.frame(
    facility_id = 1:2,
    facility_name = c("Hospital A", "Clinic B"),
    latitude = c(-31.95, -32.1),
    longitude = c(115.85, 115.9)
  )

  # Test walking mode (slower)
  result_walking <- calculate_travel_time(patients, facilities, mode = "walking")
  result_driving <- calculate_travel_time(patients, facilities, mode = "driving")

  expect_true(all(result_walking$travel_time_minutes > result_driving$travel_time_minutes))
})

# Test identify_deserts
test_that("identify_deserts returns correct structure", {
  data <- data.frame(
    patient_id = 1:50,
    latitude = runif(50, -32.5, -31.6),
    longitude = runif(50, 115.7, 116.1),
    travel_time_minutes = c(runif(40, 5, 25), runif(10, 35, 60)),
    distance_to_facility_km = c(runif(40, 1, 12), runif(10, 15, 30)),
    asthma_diagnosis = sample(c(0, 1), 50, replace = TRUE),
    exacerbation_count = rpois(50, 2),
    ses_score = rnorm(50, 50, 15),
    pm25 = rnorm(50, 12, 3)
  )

  result <- identify_deserts(data)

  expect_s3_class(result, "healthcare_desert_analysis")
  expect_true("data" %in% names(result))
  expect_true("access_summary" %in% names(result))
  expect_true("n_deserts" %in% names(result))
  expect_true("pct_deserts" %in% names(result))
  expect_true("parameters" %in% names(result))
  expect_true("access_category" %in% names(result$data))
  expect_true("is_desert" %in% names(result$data))
})

test_that("identify_deserts validates inputs", {
  data <- data.frame(x = 1:10)

  expect_error(identify_deserts(data), "Travel time column not found in data")

  data$travel_time_minutes <- 1:10
  expect_error(identify_deserts(data), "Distance column not found in data")
})

test_that("identify_deserts print method works", {
  data <- data.frame(
    patient_id = 1:30,
    latitude = runif(30, -32.5, -31.6),
    longitude = runif(30, 115.7, 116.1),
    travel_time_minutes = runif(30, 10, 45),
    distance_to_facility_km = runif(30, 2, 20),
    asthma_diagnosis = sample(c(0, 1), 30, replace = TRUE)
  )

  result <- identify_deserts(data)

  expect_output(print(result), "Healthcare Desert Analysis Results")
  expect_output(print(result), "Thresholds:")
  expect_output(print(result), "Access Distribution:")
})

# Test create_perth_basemap
test_that("create_perth_basemap returns leaflet object", {
  skip_if_not_installed("leaflet")

  map <- create_perth_basemap()

  expect_s3_class(map, "leaflet")
})

# Test get_perth_bbox
test_that("get_perth_bbox returns correct structure", {
  bbox <- get_perth_bbox()

  expect_type(bbox, "list")
  expect_true(all(c("xmin", "xmax", "ymin", "ymax") %in% names(bbox)))
  expect_true(bbox$xmax > bbox$xmin)
  expect_true(bbox$ymax > bbox$ymin)
})

# Test filter_by_bounds
test_that("filter_by_bounds filters correctly", {
  data <- data.frame(
    longitude = c(115.75, 115.85, 115.95, 116.05),
    latitude = c(-32.4, -32.2, -32.0, -31.8)
  )

  bounds <- list(west = 115.7, east = 116.0, south = -32.5, north = -31.9)
  filtered <- filter_by_bounds(data, bounds)

  expect_equal(nrow(filtered), 3)
  expect_true(all(filtered$longitude >= bounds$west))
  expect_true(all(filtered$longitude <= bounds$east))
  expect_true(all(filtered$latitude >= bounds$south))
  expect_true(all(filtered$latitude <= bounds$north))
})

# Test calculate_map_view
test_that("calculate_map_view returns correct structure", {
  data <- data.frame(
    latitude = runif(50, -32.5, -31.6),
    longitude = runif(50, 115.7, 116.1)
  )

  view <- calculate_map_view(data)

  expect_type(view, "list")
  expect_true("center" %in% names(view))
  expect_true("zoom" %in% names(view))
  expect_true("lat" %in% names(view$center))
  expect_true("lng" %in% names(view$center))
  expect_true(view$zoom >= 3 && view$zoom <= 18)
})

# Test generate_color_palette
test_that("generate_color_palette returns color function", {
  skip_if_not_installed("leaflet")

  # Numeric values
  values_num <- runif(50, 0, 100)
  pal_num <- generate_color_palette(values_num)

  expect_type(pal_num, "closure")

  # Factor values
  values_fac <- factor(sample(c("A", "B", "C"), 50, replace = TRUE))
  pal_fac <- generate_color_palette(values_fac)

  expect_type(pal_fac, "closure")
})

# Test create_choropleth_data
test_that("create_choropleth_data returns correct structure", {
  data <- data.frame(
    latitude = runif(100, -32.5, -31.6),
    longitude = runif(100, 115.7, 116.1),
    value = rnorm(100)
  )

  result <- create_choropleth_data(data, "value", n_bins = 10)

  expect_s3_class(result, "data.frame")
  expect_true("value" %in% names(result))
  expect_true("count" %in% names(result))
  expect_true("lat_center" %in% names(result))
  expect_true("lon_center" %in% names(result))
})

# Test add_healthcare_access_layer (requires leaflet)
test_that("add_healthcare_access_layer requires valid inputs", {
  skip_if_not_installed("leaflet")

  # This is a visual function - just test that it doesn't error with valid inputs
  map <- leaflet::leaflet()

  access_data <- data.frame(
    latitude = runif(10, -32.5, -31.6),
    longitude = runif(10, 115.7, 116.1),
    access_category = sample(c("Good Access", "Moderate Access", "Poor Access", "Healthcare Desert"), 10, replace = TRUE),
    travel_time_minutes = runif(10, 5, 60),
    distance_to_facility_km = runif(10, 1, 30)
  )

  # Should not throw error
  result <- add_healthcare_access_layer(map, access_data)
  expect_s3_class(result, "leaflet")
})
