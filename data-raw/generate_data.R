# Generate package data
library(devtools)

# Source functions
source("R/data_processing.R")

# Generate synthetic data
set.seed(42)
respiratory_data <- generate_synthetic_data(n_patients = 5000)
air_quality_stations <- generate_air_quality_stations(n_stations = 15)

# Save to data directory
usethis::use_data(respiratory_data, overwrite = TRUE)
usethis::use_data(air_quality_stations, overwrite = TRUE)

cat("Data files generated successfully!\n")
cat("Total patients:", length(unique(respiratory_data$patient_id)), "\n")
cat("Total visits:", nrow(respiratory_data), "\n")
cat("Air quality stations:", nrow(air_quality_stations), "\n")
