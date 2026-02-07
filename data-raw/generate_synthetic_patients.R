#!/usr/bin/env Rscript
# Generate Synthetic Patients Calibrated to Real Data
#
# This script creates individual-level synthetic patient data that is
# statistically calibrated to match real ABS/AIHW aggregate statistics
# and real environmental data from DWER/BOM.

library(dplyr)
library(readr)

# Load real data if available
load_real_data <- function() {
  
  real_data <- list()
  
  # Try to load air quality data
  if (file.exists("data/air_quality_historical.rds")) {
    real_data$air_quality <- readRDS("data/air_quality_historical.rds")
    cat("Loaded real air quality data\n")
  } else if (file.exists("data-raw/downloads/wa_air_quality_stations.csv")) {
    real_data$air_stations <- read_csv("data-raw/downloads/wa_air_quality_stations.csv", 
                                        show_col_types = FALSE)
    cat("Loaded air quality station metadata\n")
  }
  
  # Try to load weather data
  if (file.exists("data/weather_historical.rds")) {
    real_data$weather <- readRDS("data/weather_historical.rds")
    cat("Loaded real weather data\n")
  } else if (file.exists("data/weather_stations.rds")) {
    real_data$weather_stations <- readRDS("data/weather_stations.rds")
    cat("Loaded weather station metadata\n")
  }
  
  # Try to load SA2 boundaries
  if (file.exists("data/sa2_perth_2021.rds")) {
    real_data$sa2 <- readRDS("data/sa2_perth_2021.rds")
    cat("Loaded SA2 boundary data\n")
  }
  
  # Load health statistics
  if (file.exists("data/abs_health_statistics.rds")) {
    real_data$health_stats <- readRDS("data/abs_health_statistics.rds")
    cat("Loaded ABS health statistics\n")
  }
  
  return(real_data)
}

#' Generate Synthetic Patients
#'
#' Creates n synthetic patients with realistic characteristics
generate_synthetic_patients <- function(n_patients = 5000, real_data = NULL) {
  
  cat("\nGenerating", n_patients, "synthetic patients...\n")
  set.seed(42)
  
  # Get SA2 data (use real if available, otherwise create)
  if (!is.null(real_data$sa2)) {
    sa2_data <- real_data$sa2
    if ("geometry" %in% names(sa2_data)) {
      # Extract centroids from sf object
      sa2_coords <- sf::st_centroid(sa2_data)
      coords <- sf::st_coordinates(sa2_coords)
      sa2_data$centroid_lon <- coords[, 1]
      sa2_data$centroid_lat <- coords[, 2]
    }
  } else {
    # Use simplified SA2 data
    sa2_data <- readRDS("data/sa2_perth_2021.rds")
  }
  
  # Sample SA2s weighted by child population
  sa2_sample <- sample(1:nrow(sa2_data), n_patients,
                       replace = TRUE,
                       prob = sa2_data$population_0_18)
  
  # Create base patient dataframe
  patients <- data.frame(
    patient_id = 1:n_patients,
    sa2_code = sa2_data$sa2_code_2021[sa2_sample],
    sa2_name = sa2_data$sa2_name_2021[sa2_sample],
    stringsAsFactors = FALSE
  )
  
  # Add SA2 characteristics
  patients$socioeconomic_index <- sa2_data$irsd_score[sa2_sample]
  patients$sa2_asthma_prevalence <- sa2_data$asthma_prevalence[sa2_sample]
  
  # Generate coordinates (jittered around SA2 centroid)
  patients$latitude <- sa2_data$centroid_lat[sa2_sample] + rnorm(n_patients, 0, 0.02)
  patients$longitude <- sa2_data$centroid_lon[sa2_sample] + rnorm(n_patients, 0, 0.025)
  
  # Generate demographics
  # Age: 2-18 years, gamma distribution
  patients$age_years <- round(rgamma(n_patients, shape = 2.5, scale = 2.8), 1)
  patients$age_years <- pmin(pmax(patients$age_years, 2), 18)
  
  # Sex: Slightly more males in pediatric population
  patients$sex <- sample(c("Male", "Female"), n_patients, 
                         prob = c(0.515, 0.485), replace = TRUE)
  
  # Generate environmental exposures based on real data patterns
  patients <- add_environmental_exposures(patients, real_data)
  
  # Generate asthma diagnosis based on AIHW statistics
  patients <- add_asthma_diagnosis(patients, real_data)
  
  # Generate lung function using GLI equations
  patients <- add_lung_function(patients)
  
  # Generate health outcomes
  patients <- add_health_outcomes(patients, real_data)
  
  # Generate longitudinal visits
  patients_long <- add_longitudinal_visits(patients)
  
  return(patients_long)
}

#' Add Environmental Exposures
add_environmental_exposures <- function(patients, real_data) {
  
  # Get air quality stations
  if (!is.null(real_data$air_stations)) {
    stations <- real_data$air_stations
  } else {
    # Default stations
    stations <- data.frame(
      station_id = c("Caversham", "Duncraig", "Swanbourne", "Bentley", "Rockingham"),
      latitude = c(-31.8714, -31.8324, -31.9774, -32.0011, -32.2809),
      longitude = c(115.9411, 115.7789, 115.7610, 115.9149, 115.7260),
      pm25_typical = c(10.8, 9.5, 9.8, 12.5, 10.2)
    )
  }
  
  # Calculate distance to nearest station
  patient_coords <- as.matrix(patients[, c("longitude", "latitude")])
  station_coords <- as.matrix(stations[, c("longitude", "latitude")])
  
  nearest_idx <- apply(patient_coords, 1, function(p) {
    which.min(geosphere::distHaversine(p, station_coords))
  })
  
  # PM2.5 based on nearest station with spatial gradient
  base_pm25 <- stations$pm25_typical[nearest_idx]
  east_west <- (patients$longitude - 115.7) / (116.1 - 115.7)
  
  patients$pm25 <- base_pm25 + 
    east_west * 2 +  # Eastern areas higher
    rnorm(nrow(patients), 0, 1.5)
  patients$pm25 <- pmax(patients$pm25, 2)
  
  # PM10 (correlated with PM2.5)
  patients$pm10 <- patients$pm25 * 2.2 + rnorm(nrow(patients), 0, 3)
  
  # NO2
  patients$no2 <- 12 + east_west * 4 + rnorm(nrow(patients), 0, 2)
  
  # Pollen (higher in inland areas)
  coastal_dist <- sqrt((patients$latitude - (-31.95))^2 + 
                         (patients$longitude - 115.85)^2)
  patients$pollen_index <- runif(nrow(patients), 0, 6) + 
    coastal_dist * 5 +
    rnorm(nrow(patients), 0, 1)
  patients$pollen_index <- pmin(pmax(patients$pollen_index, 0), 10)
  
  # Temperature
  patients$temperature <- 20 - coastal_dist * 3 + rnorm(nrow(patients), 0, 3)
  
  return(patients)
}

#' Add Asthma Diagnosis
add_asthma_diagnosis <- function(patients, real_data) {
  
  # Get health statistics
  if (!is.null(real_data$health_stats)) {
    stats <- real_data$health_stats
    child_male_prev <- stats$value[stats$statistic == "children_0_14_male_asthma_pct"] / 100
    child_female_prev <- stats$value[stats$statistic == "children_0_14_female_asthma_pct"] / 100
    adult_male_prev <- stats$value[stats$statistic == "adults_15_plus_male_asthma_pct"] / 100
    adult_female_prev <- stats$value[stats$statistic == "adults_15_plus_female_asthma_pct"] / 100
  } else {
    # Default AIHW 2022 values
    child_male_prev <- 0.10
    child_female_prev <- 0.063
    adult_male_prev <- 0.094
    adult_female_prev <- 0.122
  }
  
  # Base probability by age and sex
  patients$asthma_prob <- ifelse(
    patients$age_years <= 14,
    ifelse(patients$sex == "Male", child_male_prev, child_female_prev),
    ifelse(patients$sex == "Female", adult_female_prev, adult_male_prev)
  )
  
  # Adjust by SA2 prevalence
  patients$asthma_prob <- patients$asthma_prob * 
    (patients$sa2_asthma_prevalence / 11)
  
  # Environmental effects
  patients$asthma_prob <- patients$asthma_prob + 
    (patients$pm25 - 10) * 0.002 +
    (patients$pollen_index - 5) * 0.003
  
  # SES effect
  patients$asthma_prob <- patients$asthma_prob + 
    (1000 - patients$socioeconomic_index) * 0.00002
  
  # Clamp probabilities
  patients$asthma_prob <- pmin(pmax(patients$asthma_prob, 0.02), 0.25)
  
  # Generate diagnosis
  patients$asthma_diagnosis <- ifelse(
    runif(nrow(patients)) < patients$asthma_prob,
    "Yes", "No"
  )
  
  # Diagnosis date
  patients$diagnosis_date <- as.Date("2020-01-01") + 
    sample(0:730, nrow(patients), replace = TRUE)
  
  return(patients)
}

#' Add Lung Function (GLI Equations)
add_lung_function <- function(patients) {
  
  # Estimate height from age
  patients$height_cm <- ifelse(
    patients$sex == "Male",
    85 + 6.2 * patients$age_years,
    83 + 5.8 * patients$age_years
  )
  height_m <- patients$height_cm / 100
  
  # GLI 2012 equations (simplified)
  age <- patients$age_years
  
  # FEV1 predicted
  patients$fev1_pred <- ifelse(
    patients$sex == "Male",
    -0.3328 + 0.04676 * height_m + (-0.00064) * age^2,
    -0.2668 + 0.03948 * height_m + (-0.00054) * age^2
  )
  
  # FVC predicted
  patients$fvc_pred <- ifelse(
    patients$sex == "Male",
    -0.5174 + 0.05766 * height_m + (-0.00081) * age^2,
    -0.4318 + 0.04876 * height_m + (-0.00071) * age^2
  )
  
  # Apply reductions for asthma and environment
  reduction <- ifelse(patients$asthma_diagnosis == "Yes", 0.12, 0)
  reduction <- reduction + pmax(0, (patients$pm25 - 10)) * 0.005
  
  patients$fev1 <- patients$fev1_pred * (1 - reduction) + 
    rnorm(nrow(patients), 0, 0.15)
  patients$fvc <- patients$fvc_pred * (1 - reduction * 0.7) + 
    rnorm(nrow(patients), 0, 0.18)
  
  patients$fev1_fvc_ratio <- patients$fev1 / patients$fvc
  patients$fev1_zscore <- (patients$fev1 - patients$fev1_pred) / 0.15
  
  return(patients)
}

#' Add Health Outcomes
add_health_outcomes <- function(patients, real_data) {
  
  # Exacerbation rates from AIHW
  base_rate <- ifelse(
    patients$age_years <= 14,
    225 / 100000 * 3,  # Children
    70 / 100000 * 3    # Adults
  )
  
  # Multipliers
  asthma_mult <- ifelse(patients$asthma_diagnosis == "Yes", 10, 0.5)
  pm25_mult <- 1 + (patients$pm25 - 10) * 0.02
  
  patients$exacerbation_rate <- base_rate * asthma_mult * pm25_mult
  patients$exacerbation_count <- rpois(nrow(patients), 
                                        patients$exacerbation_rate)
  
  patients$hospitalization_count <- rbinom(
    nrow(patients),
    patients$exacerbation_count,
    0.3
  )
  
  # Medication use
  patients$ics_use <- ifelse(
    patients$asthma_diagnosis == "Yes",
    sample(c("Yes", "No"), nrow(patients), prob = c(0.75, 0.25), replace = TRUE),
    "No"
  )
  
  # Action plan
  action_plan_prob <- ifelse(
    patients$asthma_diagnosis == "Yes",
    ifelse(patients$age_years <= 14, 0.67, 0.25),
    0.05
  )
  patients$written_action_plan <- ifelse(
    runif(nrow(patients)) < action_plan_prob,
    "Yes", "No"
  )
  
  return(patients)
}

#' Add Longitudinal Visits
add_longitudinal_visits <- function(patients) {
  
  visits_list <- list()
  
  for (i in 1:nrow(patients)) {
    patient <- patients[i, ]
    
    # Number of visits based on asthma
    n_visits <- ifelse(
      patient$asthma_diagnosis == "Yes",
      sample(2:6, 1, prob = c(0.2, 0.3, 0.25, 0.15, 0.1)),
      sample(1:3, 1, prob = c(0.5, 0.3, 0.2))
    )
    
    # Visit dates
    start_date <- as.Date("2020-01-01")
    end_date <- as.Date("2023-12-31")
    
    if (n_visits == 1) {
      visit_dates <- start_date + sample(0:as.numeric(end_date - start_date), 1)
    } else {
      visit_dates <- sort(sample(seq(start_date, end_date, by = "day"), n_visits))
    }
    
    for (v in 1:n_visits) {
      visit_data <- patient
      visit_data$visit_number <- v
      visit_data$visit_date <- visit_dates[v]
      
      # Update age
      days_elapsed <- as.numeric(visit_dates[v] - start_date)
      visit_data$age_years <- round(patient$age_years + days_elapsed / 365.25, 1)
      
      # Seasonal lung function variation
      month <- as.numeric(format(visit_dates[v], "%m"))
      seasonal <- ifelse(month %in% c(6, 7, 8), -0.05, 0.02)
      
      visit_data$fev1 <- patient$fev1 + seasonal + rnorm(1, 0, 0.05)
      visit_data$fvc <- patient$fvc + seasonal + rnorm(1, 0, 0.06)
      visit_data$fev1_fvc_ratio <- visit_data$fev1 / visit_data$fvc
      
      visits_list[[length(visits_list) + 1]] <- visit_data
    }
  }
  
  result <- bind_rows(visits_list)
  
  # Clean up
  result$asthma_prob <- NULL
  result$exacerbation_rate <- NULL
  result$fev1_pred <- NULL
  result$fvc_pred <- NULL
  
  return(result)
}

#' Validate Generated Data
validate_data <- function(data) {
  
  cat("\n=== Validation ===\n")
  
  # Check asthma prevalence
  prev <- mean(data$asthma_diagnosis == "Yes", na.rm = TRUE) * 100
  cat("Asthma prevalence:", round(prev, 1), "% (target: ~11%)\n")
  
  # Check age-sex patterns
  children <- data[data$age_years <= 14, ]
  cat("\nChildren (0-14):\n")
  cat("  Male asthma:", round(mean(children$asthma_diagnosis[children$sex == "Male"] == "Yes", na.rm = TRUE) * 100, 1), "% (target: 10%)\n")
  cat("  Female asthma:", round(mean(children$asthma_diagnosis[children$sex == "Female"] == "Yes", na.rm = TRUE) * 100, 1), "% (target: 6.3%)\n")
  
  # Check environmental ranges
  cat("\nEnvironmental exposures:\n")
  cat("  PM2.5:", round(mean(data$pm25), 1), "μg/m³ (typical: 8-15)\n")
  cat("  PM10:", round(mean(data$pm10), 1), "μg/m³\n")
  cat("  Temperature:", round(mean(data$temperature), 1), "°C\n")
  
  cat("\nTotal records:", nrow(data), "\n")
  cat("Unique patients:", length(unique(data$patient_id)), "\n")
}

# Main execution
cat("========================================\n")
cat("Generate Synthetic Patients\n")
cat("Calibrated to Real ABS/AIHW Data\n")
cat("========================================\n")

# Load real data
cat("\nLoading real data sources...\n")
real_data <- load_real_data()

# Generate patients
cat("\nGenerating synthetic patients...\n")
patients <- generate_synthetic_patients(n_patients = 5000, real_data = real_data)

# Validate
validate_data(patients)

# Save
cat("\nSaving data...\n")
saveRDS(patients, "data/respiratory_patients.rds")
write_csv(patients, "data-raw/processed/respiratory_patients.csv")

# Also save as package data
usethis::use_data(patients, overwrite = TRUE)

cat("\nSaved:\n")
cat("  - data/respiratory_patients.rds\n")
cat("  - data/respiratory_patients.csv\n")
cat("  - data/patients.rda (package data)\n")

cat("\n========================================\n")
cat("Generation Complete\n")
cat("========================================\n")
