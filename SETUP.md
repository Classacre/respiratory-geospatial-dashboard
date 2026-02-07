# Pediatric Respiratory Geospatial Dashboard - Setup Guide

## Quick Start

### 1. Install the Package

```r
# Install from GitHub
remotes::install_github("Classacre/respiratory-geospatial-dashboard")

# Or install locally
devtools::install("path/to/respiratory-geospatial-dashboard")
```

### 2. Launch the Dashboard

```r
library(respiratorygeospatial)
run_dashboard()
```

The dashboard will open in your default web browser at `http://localhost:XXXX`

## Development Setup

### Prerequisites

- R (>= 4.0.0)
- RStudio (recommended)
- Git

### Install Dependencies

```r
# Install required packages
install.packages(c(
  "shiny", "shinydashboard", "leaflet", "leaflet.extras",
  "plotly", "DT", "sf", "lme4", "ggplot2", "dplyr",
  "tidyr", "lubridate", "viridis", "geosphere"
))

# Install development packages
install.packages(c("devtools", "testthat", "knitr", "rmarkdown"))
```

### Clone and Build

```bash
git clone https://github.com/Classacre/respiratory-geospatial-dashboard.git
cd respiratory-geospatial-dashboard
```

```r
# In R
devtools::load_all()  # Load package
devtools::test()      # Run tests
devtools::check()     # Check package
```

### Generate Sample Data

```r
# Generate synthetic data
source("data-raw/generate_data.R")

# Or in R:
library(respiratorygeospatial)
data <- generate_synthetic_data(n_patients = 5000)
```

## Dashboard Features

### Tab 1: Overview
- Summary statistics cards
- Demographic distributions
- Environmental exposure summaries

### Tab 2: Geospatial Map
- Interactive Leaflet map
- Multiple layer options (patients, heatmap, PM2.5)
- Air quality station overlays
- Time filter slider

### Tab 3: Lung Function Trajectories
- Individual patient plots
- Population reference curves
- Growth percentiles
- Risk factor analysis

### Tab 4: Risk Prediction
- Logistic regression models
- Odds ratios visualization
- Model performance metrics
- Forest plots

### Tab 5: Data Explorer
- Interactive data table
- Column selection
- Download options (CSV, Excel, RDS)

## Troubleshooting

### Common Issues

**Issue: Package installation fails**
- Solution: Ensure R (>= 4.0.0) is installed
- Install RTools on Windows for compiling packages

**Issue: Shiny app won't start**
- Check that all dependencies are installed
- Try: `shiny::runApp("inst/shiny")`

**Issue: Map doesn't display**
- Check internet connection (tiles load from CDN)
- Try different base map layer

### Contact

For issues or questions, please open a GitHub issue.
