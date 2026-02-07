# Test data processing functions
test_that("generate_synthetic_data returns correct structure", {
  data <- generate_synthetic_data(n_patients = 100, seed = 42)

  expect_s3_class(data, "data.frame")
  expect_true("patient_id" %in% names(data))
  expect_true("sex" %in% names(data))
  expect_true("age_years" %in% names(data))
  expect_true("latitude" %in% names(data))
  expect_true("longitude" %in% names(data))
  expect_true("ses_score" %in% names(data))
  expect_true("pm25" %in% names(data))
  expect_true("fev1" %in% names(data))
  expect_true("asthma_diagnosis" %in% names(data))

  # Check new health indicators
  expect_true("medication_use" %in% names(data))
  expect_true("controller_use" %in% names(data))
  expect_true("rescue_use" %in% names(data))
  expect_true("outpatient_visits" %in% names(data))
  expect_true("medication_adherence" %in% names(data))
  expect_true("quality_care_index" %in% names(data))
  expect_true("ed_visits" %in% names(data))
  expect_true("healthcare_utilization" %in% names(data))
})

test_that("generate_synthetic_data has correct factor levels", {
  data <- generate_synthetic_data(n_patients = 100, seed = 42)

  expect_true(is.factor(data$sex))
  expect_true(is.factor(data$asthma_diagnosis))
  expect_true(is.factor(data$ics_use))
  expect_true(is.factor(data$medication_use))
  expect_true(is.factor(data$controller_use))
  expect_true(is.factor(data$rescue_use))

  expect_equal(levels(data$sex), c("Male", "Female"))
  expect_equal(levels(data$asthma_diagnosis), c("No", "Yes"))
  expect_equal(levels(data$medication_use), c("None", "Rescue Only", "Controller"))
})

test_that("generate_synthetic_data produces longitudinal data", {
  data <- generate_synthetic_data(n_patients = 50, seed = 42)

  expect_true("visit_number" %in% names(data))
  expect_true("visit_date" %in% names(data))

  # Should have more rows than patients (multiple visits)
  expect_gt(nrow(data), 50)

  # Check visit numbers are reasonable
  expect_true(all(data$visit_number >= 1))
  expect_true(all(data$visit_number <= 5))
})

test_that("medication_use categories are correctly assigned", {
  data <- generate_synthetic_data(n_patients = 200, seed = 42)

  # Check that medication_use is derived from controller_use and rescue_use
  data_unique <- data %>%
    dplyr::distinct(patient_id, .keep_all = TRUE)

  # All asthmatics should have some medication category
  asthmatics <- data_unique[data_unique$asthma_diagnosis == "Yes", ]
  if (nrow(asthmatics) > 0) {
    expect_true(all(asthmatics$medication_use %in% c("Rescue Only", "Controller")))
  }

  # Non-asthmatics should mostly be "None" or "Rescue Only"
  non_asthmatics <- data_unique[data_unique$asthma_diagnosis == "No", ]
  if (nrow(non_asthmatics) > 0) {
    expect_true(all(non_asthmatics$medication_use %in% c("None", "Rescue Only")))
  }
})

test_that("quality_care_index is only calculated for asthmatics", {
  data <- generate_synthetic_data(n_patients = 100, seed = 42)

  data_unique <- data %>%
    dplyr::distinct(patient_id, .keep_all = TRUE)

  # Asthmatics should have quality_care_index values
  asthmatics <- data_unique[data_unique$asthma_diagnosis == "Yes", ]
  if (nrow(asthmatics) > 0) {
    expect_true(all(!is.na(asthmatics$quality_care_index)))
    expect_true(all(asthmatics$quality_care_index >= 0 & asthmatics$quality_care_index <= 100))
  }

  # Non-asthmatics should have NA quality_care_index
  non_asthmatics <- data_unique[data_unique$asthma_diagnosis == "No", ]
  if (nrow(non_asthmatics) > 0) {
    expect_true(all(is.na(non_asthmatics$quality_care_index)))
  }
})

test_that("healthcare_utilization is within valid range", {
  data <- generate_synthetic_data(n_patients = 100, seed = 42)

  data_unique <- data %>%
    dplyr::distinct(patient_id, .keep_all = TRUE)

  expect_true(all(data_unique$healthcare_utilization >= 0, na.rm = TRUE))
  expect_true(all(data_unique$healthcare_utilization <= 100, na.rm = TRUE))
})

test_that("generate_air_quality_stations returns correct structure", {
  stations <- generate_air_quality_stations(n_stations = 10, seed = 42)

  expect_s3_class(stations, "data.frame")
  expect_equal(nrow(stations), 10)
  expect_true("station_id" %in% names(stations))
  expect_true("station_name" %in% names(stations))
  expect_true("latitude" %in% names(stations))
  expect_true("longitude" %in% names(stations))
  expect_true("pm25_avg" %in% names(stations))
  expect_true("pm10_avg" %in% names(stations))
  expect_true("no2_avg" %in% names(stations))
  expect_true("o3_avg" %in% names(stations))
})

test_that("generate_synthetic_data respects seed for reproducibility", {
  data1 <- generate_synthetic_data(n_patients = 50, seed = 123)
  data2 <- generate_synthetic_data(n_patients = 50, seed = 123)

  expect_equal(data1$latitude, data2$latitude)
  expect_equal(data1$longitude, data2$longitude)
  expect_equal(data1$ses_score, data2$ses_score)
})

test_that("generate_synthetic_data produces valid coordinates", {
  data <- generate_synthetic_data(n_patients = 100, seed = 42)

  data_unique <- data %>%
    dplyr::distinct(patient_id, .keep_all = TRUE)

  # Perth metro bounds
  expect_true(all(data_unique$latitude >= -32.5 & data_unique$latitude <= -31.6))
  expect_true(all(data_unique$longitude >= 115.7 & data_unique$longitude <= 116.1))
})

test_that("generate_synthetic_data produces valid age range", {
  data <- generate_synthetic_data(n_patients = 100, seed = 42)

  data_unique <- data %>%
    dplyr::distinct(patient_id, .keep_all = TRUE)

  expect_true(all(data_unique$age_years >= 2 & data_unique$age_years <= 18))
})

test_that("ed_visits and hospitalization_count are non-negative", {
  data <- generate_synthetic_data(n_patients = 100, seed = 42)

  data_unique <- data %>%
    dplyr::distinct(patient_id, .keep_all = TRUE)

  expect_true(all(data_unique$ed_visits >= 0))
  expect_true(all(data_unique$hospitalization_count >= 0))
})

test_that("outpatient_visits are non-negative", {
  data <- generate_synthetic_data(n_patients = 100, seed = 42)

  data_unique <- data %>%
    dplyr::distinct(patient_id, .keep_all = TRUE)

  expect_true(all(data_unique$outpatient_visits >= 0))
})
