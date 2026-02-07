# Data Documentation

This folder contains scripts to download and process real data for the Pediatric Respiratory Geospatial Dashboard.

## Folder Structure

```
data-raw/
├── _setup.R                          # Setup script (creates directories)
├── download_all_data.R               # Master script to download all data
├── download_air_quality.R            # WA DWER air quality data
├── download_weather.R                # BOM weather data
├── download_abs_data.R               # ABS boundaries and health data
├── generate_synthetic_patients.R     # Generate synthetic patients
├── downloads/                        # Downloaded raw data
│   ├── wa_air_quality_stations.csv
│   ├── bom_perth_stations.csv
│   ├── DWER_DOWNLOAD_INSTRUCTIONS.txt
│   ├── ABS_BOUNDARIES_INSTRUCTIONS.txt
│   └── ABS_HEALTH_INSTRUCTIONS.txt
└── processed/                        # Processed intermediate files
    ├── air_quality_historical.csv
    ├── weather_historical.csv
    ├── sa2_perth_2021.csv
    └── respiratory_patients.csv
```

## Quick Start

### 1. Download All Real Data

```bash
cd data-raw
Rscript download_all_data.R
```

Or run individual scripts:

```bash
Rscript download_air_quality.R    # WA air quality stations
Rscript download_weather.R        # BOM weather data
Rscript download_abs_data.R       # ABS boundaries
```

### 2. Generate Synthetic Patients

After downloading real data:

```bash
Rscript generate_synthetic_patients.R
```

This creates synthetic individual-level patients calibrated to real ABS/AIHW statistics.

## Data Sources

### 1. Air Quality Data (DWER)

**Source**: WA Department of Water and Environmental Regulation
**URL**: https://dwer.wa.gov.au/air-quality

**What's Downloaded**:
- Station locations and metadata
- Typical annual average values
- Instructions for historical data download

**Manual Download**: Some historical data requires manual download from DWER website.

### 2. Weather Data (BOM)

**Source**: Bureau of Meteorology
**URL**: http://www.bom.gov.au/climate/data/

**What's Downloaded**:
- Perth metro weather station metadata
- Historical daily observations (temperature, rainfall)
- Station coordinates

**R Package**: Uses `bomrang` package for automated download.

### 3. ABS Data

**Source**: Australian Bureau of Statistics
**URL**: https://www.abs.gov.au/

**What's Downloaded**:
- SA2 (Statistical Area Level 2) boundaries for Greater Perth
- Health statistics summary
- SEIFA socioeconomic indices

**Manual Download**: Full boundary files may require manual download or `absmapsdata` package.

## Synthetic Data Generation

The `generate_synthetic_patients.R` script creates:

- **5,000 synthetic pediatric patients** (ages 2-18)
- **Multiple visits per patient** (longitudinal data)
- **Realistic characteristics**:
  - Demographics matching ABS Census 2021
  - Asthma prevalence matching AIHW statistics (11%)
  - Lung function using Global Lung Initiative equations
  - Environmental exposures based on real air quality patterns
  - Geographic distribution within Perth SA2 boundaries

### Calibration to Real Data

| Variable | Source | Calibration |
|----------|--------|-------------|
| Age/sex distribution | ABS Census 2021 | Real population pyramid |
| Asthma prevalence | AIHW 2022 | 11% overall, age-sex specific |
| Hospitalisation rates | AIHW | 99/100k overall, 225/100k children |
| PM2.5 exposure | DWER | Real station measurements |
| Lung function | GLI 2012 | Reference equations by age/sex/height |
| SA2 boundaries | ABS | Real 2021 boundaries |

## Output Files

All processed data is saved to `data/` folder:

- `air_quality_historical.rds` - Daily air quality measurements
- `weather_historical.rds` - Daily weather observations
- `weather_stations.rds` - Weather station metadata
- `sa2_perth_2021.rds` - SA2 boundary data
- `abs_health_statistics.rds` - Health statistics summary
- `respiratory_patients.rds` - Synthetic patient data
- `data_manifest.json` - Data source documentation

## Notes

- **Real aggregate data**: Used for validation and calibration
- **Synthetic individual data**: Generated to match real statistics
- **Privacy**: No real patient data is used
- **Reproducibility**: Set seed (42) ensures reproducible synthetic data
- **Documentation**: All data sources documented in `DATA_SOURCES.md`

## Troubleshooting

### bomrang package not available
```r
install.packages("bomrang")
```

### absmapsdata package not available
```r
install.packages("absmapsdata", repos = "https://hughparsonage.github.io/drat")
```

### Manual data download required
Follow instructions in `data-raw/downloads/*_INSTRUCTIONS.txt` files.
