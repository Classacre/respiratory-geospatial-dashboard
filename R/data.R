#' Respiratory Health Dataset
#'
#' A synthetic dataset of 5000 pediatric patients with respiratory health measures,
#' demographics, environmental exposures, and geographic coordinates for the Perth
#' metropolitan area.
#'
#' @format A data frame with approximately 15,000 rows (multiple visits per patient)
#' and 19 variables:
#' \describe{
#'   \item{patient_id}{Unique patient identifier}
#'   \item{sex}{Patient sex (Male/Female)}
#'   \item{age_years}{Age in years}
#'   \item{latitude}{Geographic latitude (Perth metro area)}
#'   \item{longitude}{Geographic longitude (Perth metro area)}
#'   \item{ses_score}{Socioeconomic status score (0-100)}
#'   \item{ses_quintile}{SES quintile (Q1-Q5)}
#'   \item{pm25}{PM2.5 exposure (μg/m³)}
#'   \item{pollen_index}{Pollen index (0-10)}
#'   \item{temperature}{Temperature (°C)}
#'   \item{fev1}{Forced Expiratory Volume in 1 second (L)}
#'   \item{fvc}{Forced Vital Capacity (L)}
#'   \item{fev1_fvc_ratio}{FEV1/FVC ratio}
#'   \item{asthma_diagnosis}{Asthma diagnosis (Yes/No)}
#'   \item{exacerbation_count}{Number of exacerbations}
#'   \item{hospitalization_count}{Number of hospitalizations}
#'   \item{ics_use}{Inhaled corticosteroid use (Yes/No)}
#'   \item{baseline_date}{Date of baseline visit}
#'   \item{visit_number}{Visit number (1-5)}
#'   \item{visit_date}{Date of visit}
#' }
#'
#' @source Synthetic data generated for demonstration purposes
"respiratory_data"

#' Air Quality Monitoring Stations
#'
#' Locations and average measurements for air quality monitoring stations
#' in the Perth metropolitan area.
#'
#' @format A data frame with 15 rows and 8 variables:
#' \describe{
#'   \item{station_id}{Unique station identifier}
#'   \item{station_name}{Station name}
#'   \item{latitude}{Station latitude}
#'   \item{longitude}{Station longitude}
#'   \item{pm25_avg}{Average PM2.5 (μg/m³)}
#'   \item{pm10_avg}{Average PM10 (μg/m³)}
#'   \item{no2_avg}{Average NO2 (ppb)}
#'   \item{o3_avg}{Average O3 (ppb)}
#' }
#'
#' @source Synthetic data generated for demonstration purposes
"air_quality_stations"
