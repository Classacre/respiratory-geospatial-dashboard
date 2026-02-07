#!/usr/bin/env Rscript
# Download Bureau of Meteorology Weather Data
# Uses bomrang package or direct download

# Try to install bomrang if not available
if (!requireNamespace("bomrang", quietly = TRUE)) {
  cat("Installing bomrang package...\n")
  install.packages("bomrang", repos = "https://cran.r-project.org")
}

library(bomrang)
library(dplyr)
library(readr)

# Create directories
if (!dir.exists("data-raw/downloads")) {
  dir.create("data-raw/downloads", recursive = TRUE)
}

#' Get Perth Metro Weather Stations
#'
#' Returns BOM station metadata for Perth metropolitan area
get_perth_stations <- function() {
  
  # Get all available stations
  stations <- bomrang::get_available_stations(state = "WA")
  
  # Filter for Perth metro area stations
  # Approximate bounding box for Perth metro
  perth_stations <- stations %>%
    filter(latitude >= -32.5, latitude <= -31.6,
           longitude >= 115.7, longitude <= 116.1) %>%
    filter(!is.na(latitude), !is.na(longitude))
  
  # Key stations for the dashboard
  key_stations <- c(
    "009021",  # Perth
    "009034",  # Perth Airport
    "009151",  # Jandakot
    "009170",  # Swanbourne
    "009193",  # Rottnest Island
    "009210",  # Bickley
    "009225",  # Gosnells City
    "009510",  # Pearce RAAF
    "009617",  # Mandurah
    "009741"   # Lancelin
  )
  
  # Combine key stations with other Perth metro stations
  perth_key <- perth_stations %>%
    filter(site %in% key_stations) %>%
    mutate(station_priority = "Key")
  
  perth_other <- perth_stations %>%
    filter(!(site %in% key_stations)) %>%
    mutate(station_priority = "Secondary")
  
  all_perth <- bind_rows(perth_key, perth_other) %>%
    arrange(station_priority, desc(end_year))
  
  return(all_perth)
}

#' Download Weather Data for Stations
#'
#' Downloads historical weather observations
download_weather_data <- function(stations, start_year = 2020, end_year = 2023) {
  
  all_weather <- list()
  
  for (i in 1:nrow(stations)) {
    station <- stations[i, ]
    station_id <- station$site
    station_name <- station$name
    
    cat("Downloading data for", station_name, "(", station_id, ")...\n")
    
    tryCatch({
      # Get daily observations
      weather_data <- bomrang::get_daily_weather(
        station_id = station_id,
        start_year = start_year,
        end_year = end_year
      )
      
      if (!is.null(weather_data) && nrow(weather_data) > 0) {
        weather_data$station_id <- station_id
        weather_data$station_name <- station_name
        weather_data$station_latitude <- station$latitude
        weather_data$station_longitude <- station$longitude
        
        all_weather[[length(all_weather) + 1]] <- weather_data
        cat("  Downloaded", nrow(weather_data), "records\n")
      } else {
        cat("  No data available\n")
      }
      
      # Rate limiting
      Sys.sleep(0.5)
      
    }, error = function(e) {
      cat("  Error downloading:", conditionMessage(e), "\n")
    })
  }
  
  if (length(all_weather) > 0) {
    combined <- bind_rows(all_weather)
    return(combined)
  } else {
    return(NULL)
  }
}

#' Process Weather Data
#'
#' Cleans and aggregates weather observations
process_weather_data <- function(weather_data) {
  
  if (is.null(weather_data)) {
    cat("No weather data to process\n")
    return(NULL)
  }
  
  cat("\nProcessing weather data...\n")
  
  # Select relevant columns
  processed <- weather_data %>%
    select(
      date = Date,
      station_id,
      station_name,
      station_latitude,
      station_longitude,
      min_temp = Minimum_temperature,
      max_temp = Maximum_temperature,
      rainfall = Rainfall_amount,
      evaporation = Evaporation,
      sunshine = Sunshine,
      max_wind_gust_dir = Direction_of_maximum_wind_gust,
      max_wind_gust_speed = Speed_of_maximum_wind_gust,
      max_wind_gust_time = Time_of_maximum_wind_gust
    ) %>%
    mutate(
      # Calculate mean temperature
      mean_temp = (min_temp + max_temp) / 2,
      
      # Extract month and year for aggregation
      year = as.numeric(format(date, "%Y")),
      month = as.numeric(format(date, "%m")),
      season = case_when(
        month %in% c(12, 1, 2) ~ "Summer",
        month %in% c(3, 4, 5) ~ "Autumn",
        month %in% c(6, 7, 8) ~ "Winter",
        month %in% c(9, 10, 11) ~ "Spring"
      )
    )
  
  # Save processed data
  saveRDS(processed, "data/weather_historical.rds")
  write_csv(processed, "data-raw/processed/weather_historical.csv")
  
  # Create station metadata
  station_meta <- processed %>%
    group_by(station_id, station_name) %>%
    summarise(
      latitude = first(station_latitude),
      longitude = first(station_longitude),
      start_date = min(date),
      end_date = max(date),
      record_count = n(),
      mean_max_temp = mean(max_temp, na.rm = TRUE),
      mean_min_temp = mean(min_temp, na.rm = TRUE),
      total_rainfall = sum(rainfall, na.rm = TRUE),
      .groups = "drop"
    )
  
  saveRDS(station_meta, "data/weather_stations.rds")
  write_csv(station_meta, "data-raw/processed/weather_stations.csv")
  
  cat("Saved processed weather data:\n")
  cat("  - data/weather_historical.rds\n")
  cat("  - data/weather_stations.rds\n")
  cat("\nData summary:\n")
  cat("  Stations:", length(unique(processed$station_id)), "\n")
  cat("  Date range:", min(processed$date), "to", max(processed$date), "\n")
  cat("  Total records:", nrow(processed), "\n")
  
  return(processed)
}

# Main execution
cat("=== BOM Weather Data Download ===\n\n")

# Get Perth stations
cat("Fetching Perth metro weather stations...\n")
perth_stations <- get_perth_stations()

if (!is.null(perth_stations) && nrow(perth_stations) > 0) {
  cat("Found", nrow(perth_stations), "stations\n\n")
  
  # Save station list
  write_csv(perth_stations, "data-raw/downloads/bom_perth_stations.csv")
  
  # Display key stations
  cat("Key stations:\n")
  perth_stations %>%
    filter(station_priority == "Key") %>%
    select(site, name, latitude, longitude, start_year, end_year) %>%
    print()
  
  # Download weather data (limit to key stations to avoid rate limits)
  key_stations <- perth_stations %>%
    filter(station_priority == "Key")
  
  cat("\nDownloading weather data for key stations...\n")
  weather_data <- download_weather_data(key_stations, 2020, 2023)
  
  # Process data
  if (!is.null(weather_data)) {
    processed <- process_weather_data(weather_data)
  } else {
    cat("\nFailed to download weather data. Creating placeholder...\n")
    
    # Create placeholder with station metadata only
    placeholder <- key_stations %>%
      select(station_id = site,
             station_name = name,
             latitude,
             longitude,
             start_year,
             end_year)
    
    saveRDS(placeholder, "data/weather_stations.rds")
    cat("Saved station metadata only\n")
  }
  
} else {
  cat("Failed to fetch station list. Using manual station list...\n")
  
  # Fallback to manual station list
  manual_stations <- data.frame(
    station_id = c("009021", "009034", "009151", "009170", "009193"),
    station_name = c("Perth", "Perth Airport", "Jandakot", "Swanbourne", "Rottnest Island"),
    latitude = c(-31.9554, -31.9275, -32.1011, -31.9558, -32.0069),
    longitude = c(115.8583, 115.9764, 115.8794, 115.7614, 115.5025),
    stringsAsFactors = FALSE
  )
  
  saveRDS(manual_stations, "data/weather_stations.rds")
  cat("Saved manual station list\n")
}

cat("\n=== Download Complete ===\n")
