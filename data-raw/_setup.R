# Data Download Scripts
#
# This folder contains scripts to download and process real data from:
# - WA Department of Water and Environmental Regulation (DWER) - Air Quality
# - Bureau of Meteorology (BOM) - Weather data
# - Australian Bureau of Statistics (ABS) - Geographic boundaries
# - Australian Institute of Health and Welfare (AIHW) - Health statistics

# Create directories if they don't exist
dirs <- c("data-raw/downloads", "data-raw/processed", "data")
for (d in dirs) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

cat("Data directories created successfully!\n")
cat("Run individual download scripts to fetch real data.\n")
