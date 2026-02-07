#' Generate Synthetic Pediatric Respiratory Dataset
#'
#' Creates a synthetic dataset of 5000 pediatric patients with respiratory
#' health measures, demographics, environmental exposures, and geographic
#' coordinates for the Perth metropolitan area.
#'
#' @param n_patients Number of unique patients to generate (default: 5000)
#' @param seed Random seed for reproducibility (default: 42)
#' @return A data frame with patient data
#' @export
#'
#' @examples
#' \dontrun{
#' data <- generate_synthetic_data(n_patients = 1000)
#' }
generate_synthetic_data <- function(n_patients = 5000, seed = 42) {
  set.seed(seed)
  
  # Perth metro area bounding box (approximate)
  lat_min <- -32.5
  lat_max <- -31.6
  lon_min <- 115.7
  lon_max <- 116.1
  
  # Generate patient IDs
  patient_id <- 1:n_patients
  
  # Generate demographics
  sex <- sample(c("Male", "Female"), n_patients, replace = TRUE, prob = c(0.52, 0.48))
  
  # Age distribution: skewed toward younger children with asthma
  age_years <- rgamma(n_patients, shape = 3, scale = 2.5)
  age_years <- pmin(pmax(age_years, 2), 18)  # Clamp between 2-18 years
  
  # Geographic coordinates (Perth metro area with some clustering)
  n_clusters <- 8
  cluster_centers <- data.frame(
    lat = runif(n_clusters, lat_min, lat_max),
    lon = runif(n_clusters, lon_min, lon_max)
  )
  
  # Assign patients to clusters with some random scatter
  cluster_assignment <- sample(1:n_clusters, n_patients, replace = TRUE)
  latitude <- cluster_centers$lat[cluster_assignment] + rnorm(n_patients, 0, 0.05)
  longitude <- cluster_centers$lon[cluster_assignment] + rnorm(n_patients, 0, 0.05)
  
  # Ensure within bounds
  latitude <- pmin(pmax(latitude, lat_min), lat_max)
  longitude <- pmin(pmax(longitude, lon_min), lon_max)
  
  # Socioeconomic status (correlated with some locations)
  ses_score <- rnorm(n_patients, 50, 15)
  # Adjust SES based on location (coastal areas slightly higher SES)
  coastal_proximity <- sqrt((latitude - (-31.95))^2 + (longitude - 115.85)^2)
  ses_score <- ses_score + (1 - coastal_proximity / max(coastal_proximity)) * 10
  ses_score <- pmin(pmax(ses_score, 10), 90)
  ses_quintile <- cut(ses_score, breaks = 5, labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4", "Q5 (Highest)"))
  
  # Environmental exposures (spatially correlated)
  # PM2.5 higher in industrial/eastern areas
  pm25_base <- 8 + (longitude - lon_min) / (lon_max - lon_min) * 12
  pm25 <- pm25_base + rnorm(n_patients, 0, 2)
  pm25 <- pmax(pm25, 2)
  
  # Pollen higher in spring, varies by location
  pollen_index <- runif(n_patients, 0, 10)
  
  # Temperature (seasonal pattern)
  temperature <- 18 + rnorm(n_patients, 0, 5)
  
  # Baseline lung function (age and sex dependent)
  # FEV1 and FVC increase with age, different by sex
  fev1_base <- ifelse(sex == "Male",
                      0.5 + 0.15 * age_years + 0.002 * age_years^2,
                      0.45 + 0.13 * age_years + 0.002 * age_years^2)
  fvc_base <- ifelse(sex == "Male",
                     0.6 + 0.18 * age_years + 0.003 * age_years^2,
                     0.55 + 0.16 * age_years + 0.003 * age_years^2)
  
  # Add environmental effects on lung function
  fev1 <- fev1_base - 0.01 * pm25 + rnorm(n_patients, 0, 0.15)
  fvc <- fvc_base - 0.008 * pm25 + rnorm(n_patients, 0, 0.18)
  
  # Calculate FEV1/FVC ratio
  fev1_fvc_ratio <- fev1 / fvc
  
  # Asthma diagnosis (based on symptoms, environmental exposure)
  asthma_risk <- plogis(-2 + 0.1 * pm25 + 0.05 * pollen_index - 0.02 * ses_score + 0.5 * (fev1_fvc_ratio < 0.8))
  asthma_diagnosis <- rbinom(n_patients, 1, asthma_risk)
  
  # Exacerbation history (more likely with asthma and poor control)
  exacerbation_count <- rpois(n_patients, lambda = ifelse(asthma_diagnosis == 1, 2.5, 0.3))
  hospitalization_count <- rpois(n_patients, lambda = ifelse(exacerbation_count > 2, 0.8, 0.1))
  
  # Medication use
  ics_use <- ifelse(asthma_diagnosis == 1, rbinom(n_patients, 1, 0.7), 0)
  
  # Create baseline data
  baseline_data <- data.frame(
    patient_id = patient_id,
    sex = sex,
    age_years = round(age_years, 1),
    latitude = round(latitude, 6),
    longitude = round(longitude, 6),
    ses_score = round(ses_score, 1),
    ses_quintile = ses_quintile,
    pm25 = round(pm25, 2),
    pollen_index = round(pollen_index, 1),
    temperature = round(temperature, 1),
    fev1 = round(fev1, 3),
    fvc = round(fvc, 3),
    fev1_fvc_ratio = round(fev1_fvc_ratio, 3),
    asthma_diagnosis = factor(asthma_diagnosis, labels = c("No", "Yes")),
    exacerbation_count = exacerbation_count,
    hospitalization_count = hospitalization_count,
    ics_use = factor(ics_use, labels = c("No", "Yes")),
    baseline_date = as.Date("2020-01-01") + sample(0:365, n_patients, replace = TRUE)
  )
  
  # Generate longitudinal visits (1-5 visits per patient)
  longitudinal_data <- generate_longitudinal_visits(baseline_data)
  
  return(longitudinal_data)
}

#' Generate Longitudinal Visit Data
#'
#' Creates follow-up visit data for each patient with repeated lung function
#' measurements and exacerbation events.
#'
#' @param baseline_data Baseline patient data frame
#' @return A data frame with longitudinal data
#' @keywords internal
generate_longitudinal_visits <- function(baseline_data) {
  visits_list <- list()
  
  for (i in seq_len(nrow(baseline_data))) {
    patient <- baseline_data[i, ]
    n_visits <- sample(1:5, 1, prob = c(0.2, 0.3, 0.25, 0.15, 0.1))
    
    if (n_visits == 1) {
      visits_list[[i]] <- patient
      next
    }
    
    # Generate visit dates
    visit_intervals <- cumsum(c(0, rexp(n_visits - 1, rate = 1/90)))  # ~90 days between visits
    visit_dates <- patient$baseline_date + visit_intervals
    
    for (v in 1:n_visits) {
      visit_data <- patient
      visit_data$visit_number <- v
      visit_data$visit_date <- visit_dates[v]
      
      # Age increases with time
      days_elapsed <- as.numeric(visit_dates[v] - patient$baseline_date)
      visit_data$age_years <- round(patient$age_years + days_elapsed / 365.25, 1)
      
      # Lung function changes over time
      time_effect <- rnorm(1, 0, 0.05)  # Random variation
      seasonal_effect <- 0.02 * sin(2 * pi * as.numeric(format(visit_dates[v], "%j")) / 365)
      
      visit_data$fev1 <- round(patient$fev1 + time_effect + seasonal_effect, 3)
      visit_data$fvc <- round(patient$fvc + time_effect * 1.1 + seasonal_effect, 3)
      visit_data$fev1_fvc_ratio <- round(visit_data$fev1 / visit_data$fvc, 3)
      
      # Exacerbations between visits
      if (v > 1) {
        new_exacerbations <- rpois(1, lambda = ifelse(patient$asthma_diagnosis == "Yes", 0.5, 0.05))
        visit_data$exacerbation_count <- patient$exacerbation_count + new_exacerbations
      }
      
      visits_list[[length(visits_list) + 1]] <- visit_data
    }
  }
  
  result <- do.call(rbind, visits_list)
  rownames(result) <- NULL
  
  return(result)
}

#' Generate Air Quality Station Data
#'
#' Creates synthetic air quality monitoring station data for Perth metro area.
#'
#' @param n_stations Number of stations (default: 15)
#' @param seed Random seed
#' @return A data frame with station locations and measurements
#' @export
generate_air_quality_stations <- function(n_stations = 15, seed = 42) {
  set.seed(seed + 1)
  
  # Perth metro area
  lat_min <- -32.5
  lat_max <- -31.6
  lon_min <- 115.7
  lon_max <- 116.1
  
  stations <- data.frame(
    station_id = 1:n_stations,
    station_name = paste0("AQS_", sprintf("%02d", 1:n_stations)),
    latitude = runif(n_stations, lat_min, lat_max),
    longitude = runif(n_stations, lon_min, lon_max),
    pm25_avg = round(rnorm(n_stations, 12, 4), 2),
    pm10_avg = round(rnorm(n_stations, 25, 8), 2),
    no2_avg = round(rnorm(n_stations, 15, 5), 2),
    o3_avg = round(rnorm(n_stations, 35, 10), 2)
  )
  
  return(stations)
}
