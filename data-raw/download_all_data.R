#!/usr/bin/env Rscript
# Master Script to Download All Real Data
# 
# This script orchestrates the download of all real data sources:
# 1. WA Air Quality Data (DWER)
# 2. BOM Weather Data
# 3. ABS Geographic Boundaries and Health Data

# Set working directory to project root
setwd(dirname(dirname(sys.frame(1)$ofile)))

cat("========================================\n")
cat("Respiratory Geospatial Dashboard\n")
cat("Real Data Download Script\n")
cat("========================================\n\n")

# Create all necessary directories
cat("Creating directories...\n")
dirs <- c("data-raw/downloads", "data-raw/processed", "data", "data/external")
for (d in dirs) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    cat("  Created:", d, "\n")
  }
}

# Track download status
download_status <- list()

# 1. Download Air Quality Data
cat("\n----------------------------------------\n")
cat("STEP 1: Air Quality Data (DWER)\n")
cat("----------------------------------------\n")
tryCatch({
  source("data-raw/download_air_quality.R")
  download_status$air_quality <- "SUCCESS"
  cat("\n✓ Air quality data processed\n")
}, error = function(e) {
  download_status$air_quality <- paste("FAILED:", conditionMessage(e))
  cat("\n✗ Air quality download failed:", conditionMessage(e), "\n")
})

# 2. Download Weather Data
cat("\n----------------------------------------\n")
cat("STEP 2: Weather Data (BOM)\n")
cat("----------------------------------------\n")
tryCatch({
  source("data-raw/download_weather.R")
  download_status$weather <- "SUCCESS"
  cat("\n✓ Weather data processed\n")
}, error = function(e) {
  download_status$weather <- paste("FAILED:", conditionMessage(e))
  cat("\n✗ Weather download failed:", conditionMessage(e), "\n")
})

# 3. Download ABS Data
cat("\n----------------------------------------\n")
cat("STEP 3: ABS Boundaries and Health Data\n")
cat("----------------------------------------\n")
tryCatch({
  source("data-raw/download_abs_data.R")
  download_status$abs <- "SUCCESS"
  cat("\n✓ ABS data processed\n")
}, error = function(e) {
  download_status$abs <- paste("FAILED:", conditionMessage(e))
  cat("\n✗ ABS download failed:", conditionMessage(e), "\n")
})

# Create data manifest
cat("\n----------------------------------------\n")
cat("Creating Data Manifest\n")
cat("----------------------------------------\n")

manifest <- list(
  project = "Pediatric Respiratory Geospatial Dashboard",
  download_date = Sys.Date(),
  data_sources = list(
    air_quality = list(
      source = "WA Department of Water and Environmental Regulation (DWER)",
      url = "https://dwer.wa.gov.au/air-quality",
      status = download_status$air_quality,
      files = list.files("data", pattern = "air_quality", full.names = TRUE)
    ),
    weather = list(
      source = "Bureau of Meteorology (BOM)",
      url = "http://www.bom.gov.au/climate/data/",
      status = download_status$weather,
      files = list.files("data", pattern = "weather", full.names = TRUE)
    ),
    abs_boundaries = list(
      source = "Australian Bureau of Statistics (ABS)",
      url = "https://www.abs.gov.au/",
      status = download_status$abs,
      files = list.files("data", pattern = "sa2|abs", full.names = TRUE)
    )
  ),
  instructions = list(
    manual_download = list.files("data-raw/downloads", pattern = "INSTRUCTIONS", full.names = TRUE)
  )
)

# Save manifest
saveRDS(manifest, "data/data_manifest.rds")
jsonlite::write_json(manifest, "data/data_manifest.json", pretty = TRUE, auto_unbox = TRUE)

cat("Saved data manifest:\n")
cat("  - data/data_manifest.rds\n")
cat("  - data/data_manifest.json\n")

# Summary
cat("\n========================================\n")
cat("Download Summary\n")
cat("========================================\n")

for (source in names(manifest$data_sources)) {
  status <- manifest$data_sources[[source]]$status
  icon <- ifelse(grepl("SUCCESS", status), "✓", "✗")
  cat(icon, toupper(source), ":", status, "\n")
}

cat("\nData files created:\n")
data_files <- list.files("data", recursive = TRUE, full.names = TRUE)
if (length(data_files) > 0) {
  for (f in data_files) {
    size <- round(file.size(f) / 1024, 1)
    cat("  -", f, "(", size, "KB)\n")
  }
} else {
  cat("  No data files created yet\n")
}

cat("\nManual download instructions available:\n")
instruction_files <- list.files("data-raw/downloads", pattern = "INSTRUCTIONS", full.names = TRUE)
if (length(instruction_files) > 0) {
  for (f in instruction_files) {
    cat("  -", f, "\n")
  }
}

cat("\n========================================\n")
cat("Next Steps:\n")
cat("========================================\n")
cat("1. Review downloaded data in data/ folder\n")
cat("2. Follow manual download instructions if needed\n")
cat("3. Run data-raw/generate_synthetic_patients.R to create\n")
cat("   synthetic patients calibrated to real data\n")
cat("4. Build the dashboard with real data sources\n")
cat("\n========================================\n")
