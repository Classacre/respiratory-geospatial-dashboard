# Test data processing functions
test_that("generate_synthetic_data creates valid dataset", {
  data <- generate_synthetic_data(n_patients = 100, seed = 123)
  
  expect_s3_class(data, "data.frame")
  expect_gt(nrow(data), 100)  # Should have multiple visits
  expect_true(all(c("patient_id", "sex", "age_years", "latitude", "longitude",
                    "fev1", "fvc", "asthma_diagnosis") %in% names(data)))
  expect_true(all(data$age_years >= 2 & data$age_years <= 18))
  expect_true(all(data$sex %in% c("Male", "Female")))
  expect_true(all(data$asthma_diagnosis %in% c("Yes", "No")))
})

test_that("generate_air_quality_stations creates valid dataset", {
  stations <- generate_air_quality_stations(n_stations = 10, seed = 123)
  
  expect_s3_class(stations, "data.frame")
  expect_equal(nrow(stations), 10)
  expect_true(all(c("station_id", "station_name", "latitude", "longitude") %in% names(stations)))
})

test_that("longitudinal visits are generated correctly", {
  data <- generate_synthetic_data(n_patients = 50, seed = 123)
  
  # Check that some patients have multiple visits
  visit_counts <- table(data$patient_id)
  expect_gt(max(visit_counts), 1)
  
  # Check visit numbers are sequential
  for (pid in unique(data$patient_id)) {
    patient_data <- data[data$patient_id == pid, ]
    expect_equal(sort(patient_data$visit_number), patient_data$visit_number)
  }
})
