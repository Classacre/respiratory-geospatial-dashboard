# Pediatric Respiratory Geospatial Dashboard

[![R-CMD-check](https://github.com/Classacre/respiratory-geospatial-dashboard/workflows/R-CMD-check/badge.svg)](https://github.com/Classacre/respiratory-geospatial-dashboard/actions)

A comprehensive R Shiny dashboard for pediatric respiratory health analysis with geospatial components. This package provides tools for analyzing lung function trajectories, environmental risk factors, and spatial patterns of respiratory disease in pediatric populations.

## Overview

This dashboard was developed to learn and manipulate geospatial data using R. It demonstrates techniques for:

- **Geospatial Analysis**: Spatial autocorrelation, hotspot detection, and interpolation
- **Statistical Modeling**: Mixed-effects models, logistic regression, risk prediction
- **Interactive Visualization**: Leaflet maps, Plotly charts, and Shiny dashboards
- **R Package Development**: Proper package structure, documentation, and testing

## Features

### 1. Overview Dashboard
- Summary statistics and cohort description
- Demographic distributions (age, sex, SES)
- Environmental exposure summaries
- Lung function distributions

### 2. Geospatial Map
- Interactive Leaflet map of patient locations
- Heatmap layers for asthma prevalence and exacerbations
- Environmental overlay (air quality stations)
- Time slider for temporal analysis
- Spatial statistics and hotspot detection

### 3. Lung Function Trajectories
- Individual patient trajectory plots
- Population reference curves by age and sex
- Growth-adjusted percentiles
- Analysis by environmental risk factors

### 4. Risk Prediction
- Logistic regression for asthma risk
- Linear models for lung function
- Odds ratios visualization with forest plots
- Model performance metrics
- Predicted vs observed plots

### 5. Data Explorer
- Interactive data table with filtering
- Column selection and customization
- Download options (CSV, Excel, RDS)

## Installation

```r
# Install from GitHub
remotes::install_github("Classacre/respiratory-geospatial-dashboard")

# Or clone and install locally
git clone https://github.com/Classacre/respiratory-geospatial-dashboard.git
devtools::install("respiratory-geospatial-dashboard")
```

## Quick Start

```r
library(respiratorygeospatial)

# Launch the dashboard
run_dashboard()

# Access sample data
data(respiratory_data)
data(air_quality_stations)

# Generate new synthetic data
new_data <- generate_synthetic_data(n_patients = 1000)
```

## Data Structure

The package includes a synthetic dataset of 5,000 pediatric patients with:

| Variable | Description |
|----------|-------------|
| `patient_id` | Unique patient identifier |
| `sex` | Male/Female |
| `age_years` | Age in years (2-18) |
| `latitude` | Geographic latitude (Perth metro) |
| `longitude` | Geographic longitude (Perth metro) |
| `ses_score` | Socioeconomic status (0-100) |
| `pm25` | PM2.5 exposure (μg/m³) |
| `pollen_index` | Pollen index (0-10) |
| `fev1` | Forced Expiratory Volume (L) |
| `fvc` | Forced Vital Capacity (L) |
| `asthma_diagnosis` | Yes/No |
| `exacerbation_count` | Number of exacerbations |
| `visit_number` | Visit number (1-5) |

## Analysis Functions

### Spatial Analysis

```r
# Calculate spatial weights matrix
W <- calculate_spatial_weights(data, bandwidth = 10)

# Moran's I for spatial autocorrelation
moran_result <- calculate_morans_i(asthma_prevalence, W)

# Detect hotspots
hotspots <- detect_hotspots(data, outcome_col = "exacerbation_count")

# Spatial interpolation
grid <- create_interpolation_grid(bbox, resolution = 1)
pm25_surface <- idw_interpolate(data, "pm25", grid)
```

### Risk Modeling

```r
# Fit mixed-effects model for lung function
model <- fit_lung_function_model(
  data = respiratory_data,
  outcome = "fev1",
  fixed_effects = c("sex", "pm25", "ics_use"),
  random_effects = c("age_years")
)

# Logistic regression for asthma risk
asthma_model <- fit_asthma_risk_model(
  data,
  predictors = c("pm25", "pollen_index", "ses_score")
)

# Calculate odds ratios
or_table <- calculate_odds_ratios(asthma_model)
```

## Screenshots

*Dashboard Overview Tab*
![Overview](inst/screenshots/overview.png)

*Geospatial Map with Patient Locations*
![Map](inst/screenshots/map.png)

*Lung Function Trajectories*
![Trajectories](inst/screenshots/trajectories.png)

*Risk Prediction Models*
![Risk](inst/screenshots/risk.png)

## Project Structure

```
respiratory-geospatial-dashboard/
├── R/                          # Core analysis functions
│   ├── data_processing.R         # Synthetic data generation
│   ├── spatial_analysis.R        # Spatial statistics
│   ├── risk_modeling.R           # Statistical models
│   ├── geospatial_utils.R        # Mapping utilities
│   └── shiny_app.R               # App launcher
├── data/                       # Sample datasets
├── inst/shiny/                 # Shiny application
│   ├── app.R                     # Main app file
│   └── modules/                  # Shiny modules
├── tests/                      # Unit tests
├── vignettes/                  # Documentation
├── DESCRIPTION                 # Package metadata
└── README.md                   # This file
```

## Dependencies

### Core
- shiny, shinydashboard
- leaflet, leaflet.extras
- plotly, ggplot2
- DT

### Analysis
- sf, sp, spdep
- lme4
- gstat
- geosphere

### Data
- dplyr, tidyr
- lubridate

## Testing

```r
# Run all tests
devtools::test()

# Check package
devtools::check()
```

## Related Work

This project draws on established methodologies from the geospatial health literature:

### Books and Educational Resources

**Moraga, Paula. (2019).** *Geospatial Health Data: Modeling and Visualization with R-INLA and Shiny.* Chapman & Hall/CRC Biostatistics Series. ISBN 9780367357955

- Comprehensive guide to geospatial health analysis using R
- Covers R-INLA for Bayesian spatial modeling and Shiny for interactive visualization
- Available online at: https://www.paulamoraga.com/book-geospatial/

**Moraga, Paula. (2023).** *Spatial Statistics for Data Science: Theory and Practice with R.* Chapman & Hall/CRC Data Science Series. ISBN 9781032633510

- Advanced spatial statistics with practical R implementations
- Available online at: https://www.paulamoraga.com/book-spatial/

### Research Papers

**Roberts EM, English PB, Wong M, et al. (2006).** "Progress in pediatric asthma surveillance II: geospatial patterns of asthma in Alameda County, California." *Preventing Chronic Disease*, 3(3):A92. PMCID: PMC1637800.

- Demonstrates high-resolution geospatial analysis of pediatric asthma using density estimation mapping
- Uses overlapping spatial buffers and Monte Carlo simulation for significance testing
- Shows how geospatial visualization can reveal disparities in asthma burden across communities
- **Key techniques implemented in this dashboard:** Density estimation, hotspot detection, raster surface generation

**Lewinter KE, Hudson SM, Kysh L, et al. (2022).** "Geospatial data in pediatric asthma in the United States: a scoping review protocol." *JBI Evidence Synthesis*, 20(11):2790–2798. DOI: 10.11124/JBIES-21-00284. PMCID: PMC9669090.

- Comprehensive review of GIS applications in pediatric asthma research
- Identifies data types, outcomes studied, and analytic approaches in the literature
- Covers spatial analysis of disease, risk stratification, and predictive modeling
- **Relevant to this dashboard:** Validates the use of geospatial clustering, environmental exposure analysis, and risk prediction models

**Samuels-Kalow ME, Camargo CA. (2019).** "The use of geographic data to improve asthma care delivery and population health." *Clinics in Chest Medicine*, 40(1):209–225. DOI: 10.1016/j.ccm.2018.10.012.

- Reviews applications of geographic data in asthma care
- Discusses spatial clustering for risk stratification and targeted interventions

**Beck AF, Huang B, Wheeler K, et al. (2017).** "The Child Opportunity Index and Disparities in pediatric asthma hospitalizations across one Ohio metropolitan area, 2011-2013." *The Journal of Pediatrics*, 190:200–206. DOI: 10.1016/j.jpeds.2017.08.007. PMCID: PMC5708858.

- Links neighborhood-level opportunity indices to asthma hospitalization rates
- Demonstrates use of composite geospatial indices in health research

### Key Techniques from Literature

The following established methods from the literature are implemented in this dashboard:

1. **Density Estimation Mapping**: Following Rushton and Lolonis (1996), using overlapping circular buffers to create continuous raster surfaces of health event rates

2. **Spatial Autocorrelation**: Moran's I statistic for assessing clustering of respiratory outcomes

3. **Hotspot Detection**: Local spatial statistics (Getis-Ord Gi*) for identifying statistically significant clusters

4. **Inverse Distance Weighting (IDW)**: Spatial interpolation method for creating continuous exposure surfaces from point measurements

5. **Mixed-Effects Models**: Longitudinal analysis of lung function trajectories with random effects for individual patients

6. **Logistic Regression with Spatial Covariates**: Risk prediction modeling incorporating environmental and socioeconomic factors

## Contributing

For questions or suggestions, please open an issue on GitHub.

## License

MIT License - see LICENSE file for details.

## Acknowledgments

- Synthetic data generated for demonstration purposes
- Perth geographic boundaries used for spatial context
- Methodologies based on established geospatial health research practices
