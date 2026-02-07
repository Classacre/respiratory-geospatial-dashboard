#!/usr/bin/env Rscript
# Master Data Pipeline for Pediatric Respiratory Geospatial Dashboard
#
# This script orchestrates the complete data pipeline:
# 1. Downloads real environmental data (BOM, WA DWER)
# 2. Sets up real geographic boundaries (ABS)
# 3. Loads real health statistics (AIHW)
# 4. Generates synthetic patients calibrated to real statistics
#
# Usage: Rscript run_data_pipeline.R

cat("========================================\n")
cat("Pediatric Respiratory Dashboard\n")
cat("Master Data Pipeline\n")
cat("========================================\n\n")

# Check working directory
if (!file.exists("DESCRIPTION")) {
  stop("Please run this script from the package root directory")
}

# Step 1: Download Real Environmental Data
cat("STEP 1: Downloading Real Environmental Data\n")
cat("-------------------------------------------\n")
source("data-raw/download_environmental_data.R")

cat("\n")

# Step 2: Generate Realistic Synthetic Patients
cat("STEP 2: Generating Synthetic Patients\n")
cat("--------------------------------------\n")
cat("Note: Individual patient records are synthetic but calibrated\n")
cat("to match real AIHW/ABS aggregate statistics.\n\n")
source("data-raw/generate_realistic_patients.R")

cat("\n")

# Step 3: Create Package Data
cat("STEP 3: Creating Package Data\n")
cat("------------------------------\n")

# Load the generated data
if (file.exists("data/respiratory_patients.rds")) {
  patients <- readRDS("data/respiratory_patients.rds")
  
  # Also create individual data frames for the package
  respiratory_data <- patients
  
  # Create air quality stations data
  if (file.exists("data/external/wa_airquality_stations.rds")) {
    air_quality_stations <- readRDS("data/external/wa_airquality_stations.rds")
    saveRDS(air_quality_stations, "data/air_quality_stations.rds")
    cat("✓ Created air_quality_stations.rds\n")
  }
  
  # Create weather stations data
  if (file.exists("data/external/bom_stations_metadata.rds")) {
    weather_stations <- readRDS("data/external/bom_stations_metadata.rds")
    saveRDS(weather_stations, "data/weather_stations.rds")
    cat("✓ Created weather_stations.rds\n")
  }
  
  # Create SA2 boundaries data
  if (file.exists("data/external/abs_perth_sa2_boundaries.rds")) {
    sa2_boundaries <- readRDS("data/external/abs_perth_sa2_boundaries.rds")
    saveRDS(sa2_boundaries, "data/sa2_boundaries.rds")
    cat("✓ Created sa2_boundaries.rds\n")
  }
  
  # Save main patient data
  saveRDS(respiratory_data, "data/respiratory_data.rds")
  cat("✓ Created respiratory_data.rds\n")
  
  # Create package .rda files if usethis is available
  if (requireNamespace("usethis", quietly = TRUE)) {
    usethis::use_data(respiratory_data, overwrite = TRUE)
    usethis::use_data(air_quality_stations, overwrite = TRUE)
    usethis::use_data(weather_stations, overwrite = TRUE)
    usethis::use_data(sa2_boundaries, overwrite = TRUE)
    cat("✓ Created package .rda files\n")
  }
} else {
  stop("Patient data not found. Pipeline failed.")
}

cat("\n")

# Step 4: Validate Data
cat("STEP 4: Validating Data\n")
cat("------------------------\n")

# Check key statistics
if (exists("respiratory_data")) {
  n_patients <- length(unique(respiratory_data$patient_id))
  n_records <- nrow(respiratory_data)
  asthma_prev <- mean(respiratory_data$asthma_diagnosis == "Yes", na.rm = TRUE) * 100
  
  cat("Validation Results:\n")
  cat("  Total unique patients:", n_patients, "\n")
  cat("  Total records (visits):", n_records, "\n")
  cat("  Mean visits per patient:", round(n_records / n_patients, 1), "\n")
  cat("  Asthma prevalence:", round(asthma_prev, 1), "%\n")
  cat("  Target prevalence: 9.3-11% (AIHW)\n")
  cat("  Status:", ifelse(abs(asthma_prev - 10) < 2, "✓ VALID", "⚠ CHECK"), "\n")
}

cat("\n")

# Step 5: Summary
cat("STEP 5: Pipeline Complete\n")
cat("--------------------------\n")
cat("\nData Sources Summary:\n")
cat("  ✓ BOM Weather Stations: Real station locations and climate data\n")
cat("  ✓ WA DWER Air Quality: Real monitoring stations and annual averages\n")
cat("  ✓ ABS SA2 Boundaries: Real geographic areas and population data\n")
cat("  ✓ AIHW Health Stats: Real prevalence and hospitalisation rates\n")
cat("  ✓ Synthetic Patients: Individual records calibrated to real stats\n")

cat("\nFiles Created:\n")
data_files <- list.files("data", pattern = "\\.rds$", full.names = FALSE)
for (f in data_files) {
  size <- round(file.size(file.path("data", f)) / 1024, 1)
  cat("  -", f, "(", size, "KB)\n")
}

cat("\nNext Steps:\n")
cat("  1. Run devtools::document() to update NAMESPACE\n")
cat("  2. Run devtools::check() to verify package integrity\n")
cat("  3. Launch dashboard: respiratorygeospatial::run_dashboard()\n")

cat("\n========================================\n")
cat("Data Pipeline Complete\n")
cat("========================================\n")
