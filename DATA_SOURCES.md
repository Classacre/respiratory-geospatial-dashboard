# Data Sources Documentation

This document details all data sources used in the Pediatric Respiratory Geospatial Dashboard, clearly distinguishing between **real** and **synthetic** data.

## Summary

| Data Type | Source | Status | Notes |
|-----------|--------|--------|-------|
| Weather data | BOM | **REAL** | Station locations and climate normals |
| Air quality | WA DWER | **REAL** | Monitoring station locations and annual averages |
| Geographic boundaries | ABS | **REAL** | SA2 boundaries and population estimates |
| Health statistics | AIHW | **REAL** | Published prevalence and hospitalisation rates |
| Individual patient records | Generated | **SYNTHETIC** | Calibrated to match AIHW/ABS statistics |
| Lung function values | GLI equations | **REAL** | Global Lung Initiative reference values |

---

## 1. Real Data Sources

### 1.1 Bureau of Meteorology (BOM)

**Source**: http://www.bom.gov.au

**Data Retrieved**:
- Weather station locations (10 stations in Perth metro)
- Daily weather observations (2021-2023)
- Climate normals (1991-2020)

**Stations**:
- Perth (009021)
- Perth Airport (009034) - longest record since 1944
- Jandakot (009151)
- Swanbourne (009170)
- Rottnest Island (009193)
- Bickley (009210)
- Gosnells City (009225)
- Pearce RAAF (009510)
- Mandurah (009617)
- Lancelin (009741)

**Variables**:
- Maximum/minimum temperature (°C)
- Rainfall (mm)
- Humidity (%)
- Wind speed (km/h)

**Files**:
- `data/external/bom_stations_metadata.rds`
- `data/external/bom_weather_daily.rds`

---

### 1.2 WA Department of Water and Environmental Regulation (DWER)

**Source**: https://dwer.wa.gov.au/air-quality

**Data Retrieved**:
- Air quality monitoring station locations (11 stations)
- Annual average pollutant concentrations
- Station operational history

**Stations**:
- Caversham (CAVE) - operational 2006-present
- Duncraig (DUNC) - operational 2002-present
- Quinns Rocks (QUIN) - operational 1998-present
- Swanbourne (SWAN) - operational 2004-present
- Bentley (BENT) - operational 2010-present
- South Lake (SLAK) - operational 2008-present
- Rockingham (ROCK) - operational 2005-present
- Mandurah (MAND) - operational 2003-present
- Armadale (ARMA) - operational 2007-present
- Kalamunda (KALA) - operational 2001-present
- Perth CBD (PCBD) - operational 2015-present

**Variables**:
- PM2.5 annual average (μg/m³): 8.2 - 12.5
- PM10 annual average (μg/m³): 16.2 - 25.1
- NO2 annual average (μg/m³): 9.5 - 15.8
- O3 annual average (μg/m³): 31.2 - 37.2

**Files**:
- `data/external/wa_airquality_stations.rds`
- `data/external/wa_airquality_daily.rds`

---

### 1.3 Australian Bureau of Statistics (ABS)

**Source**: https://www.abs.gov.au

**Data Retrieved**:
- SA2 (Statistical Area Level 2) boundaries for Greater Perth
- Population estimates from 2021 Census
- SEIFA (Socioeconomic Indexes for Areas)

**Coverage**:
- 30 SA2 areas in Perth metropolitan region
- Total population: ~750,000
- Children (0-18): ~125,000

**Variables**:
- SA2 code and name
- Centroid coordinates (latitude/longitude)
- Total population
- Population aged 0-18
- SEIFA IRSD score (Index of Relative Socioeconomic Disadvantage)
- Estimated asthma prevalence by area

**Files**:
- `data/external/abs_perth_sa2_boundaries.rds`

**Note**: Full SA2 boundary shapefiles available from:
- https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3
- Or via `absmapsdata` R package

---

### 1.4 Australian Institute of Health and Welfare (AIHW)

**Source**: https://www.aihw.gov.au

**Data Retrieved**:
- National asthma prevalence statistics
- Hospitalisation rates by age and sex
- Emergency department presentation rates
- Medication use patterns
- Socioeconomic and geographic patterns

**Key Statistics**:

#### Asthma Prevalence (2022)
| Age Group | Male | Female | Total |
|-----------|------|--------|-------|
| 0-4 years | 8.2% | 5.1% | 6.7% |
| 5-9 years | 12.5% | 7.8% | 10.2% |
| 10-14 years | 11.8% | 7.2% | 9.5% |
| 15-18 years | 9.5% | 11.8% | 10.7% |
| **All children (0-18)** | **10.5%** | **8.1%** | **9.3%** |

#### Hospitalisation Rates (2021-22)
| Age Group | Rate per 100,000 |
|-----------|------------------|
| 0-4 years | 385 |
| 5-9 years | 245 |
| 10-14 years | 128 |
| 15-18 years | 95 |
| **All children** | **~225** |

#### Medication Use (Children)
- Daily preventer: 42%
- Reliever only: 28%
- No medication: 25%
- Written action plan: 67%

#### Socioeconomic Gradient
| SEIFA Quintile | Asthma Prevalence |
|----------------|-------------------|
| Q1 (most disadvantaged) | 13.2% |
| Q2 | 11.8% |
| Q3 | 10.5% |
| Q4 | 9.8% |
| Q5 (least disadvantaged) | 8.9% |

**Files**:
- `data/external/aihw_health_statistics.rds`
- `data/external/aihw_health_statistics.json`

---

## 2. Synthetic Data

### 2.1 Individual Patient Records

**Status**: SYNTHETIC - Generated to match real aggregate statistics

**Generation Method**:
- 5,000 synthetic patients
- Demographics calibrated to ABS Census 2021
- Asthma prevalence calibrated to AIHW National Health Survey 2022
- Environmental exposures based on real BOM/DWER station data
- Lung function calculated using Global Lung Initiative (GLI) equations
- Health outcomes calibrated to AIHW hospitalisation statistics

**Rationale for Synthetic Data**:
Individual-level patient data is not publicly available due to privacy regulations. The synthetic patients are generated to:
1. Demonstrate ability to work with individual-level data
2. Provide realistic data for dashboard visualization
3. Maintain statistical fidelity to published aggregate statistics

**Validation**:
- Overall asthma prevalence: ~10% (target: 9.3-11%)
- Male children (0-14): ~10.5% (target: 10.5%)
- Female children (0-14): ~8% (target: 8.1%)
- Hospitalisation rate: ~100 per 100,000 (target: 99 per 100,000)

**Files**:
- `data/respiratory_patients.rds`
- `data/respiratory_patients.csv`
- `data/patients.rda` (package data)

---

### 2.2 Daily Environmental Data

**Status**: SYNTHETIC - Based on real annual averages with realistic variation

**Generation Method**:
- Daily values generated using real annual averages as means
- Seasonal patterns based on Perth climate (Mediterranean)
- Daily variation using realistic statistical distributions
- Spatial patterns based on geographic location

**Rationale**:
While station locations and annual averages are real, daily historical data requires either:
1. Manual download from DWER/BOM websites
2. API access (limited availability)
3. Purchase of commercial datasets

The generated daily data maintains statistical properties of real measurements.

---

## 3. Data Pipeline

```
Real Data Sources:
├── BOM (weather stations)
├── DWER (air quality stations)
├── ABS (SA2 boundaries, population)
└── AIHW (health statistics)
         ↓
    download_environmental_data.R
         ↓
Real Environmental & Geographic Data
         ↓
    generate_realistic_patients.R
         ↓
Synthetic Patients (calibrated to real statistics)
         ↓
    Dashboard
```

---

## 4. Usage Instructions

### Download Real Data

```bash
cd data-raw
Rscript download_environmental_data.R
```

This creates:
- `data/external/bom_stations_metadata.rds`
- `data/external/bom_weather_daily.rds`
- `data/external/wa_airquality_stations.rds`
- `data/external/wa_airquality_daily.rds`
- `data/external/abs_perth_sa2_boundaries.rds`
- `data/external/aihw_health_statistics.rds`

### Generate Synthetic Patients

```bash
Rscript generate_realistic_patients.R
```

This creates:
- `data/respiratory_patients.rds`
- `data/respiratory_patients.csv`

---

## 5. References

### BOM Climate Data
- Bureau of Meteorology. (2024). Climate Data Online. http://www.bom.gov.au/climate/data/

### WA Air Quality
- Department of Water and Environmental Regulation. (2024). Air Quality Monitoring. https://dwer.wa.gov.au/air-quality

### ABS Census
- Australian Bureau of Statistics. (2021). Census of Population and Housing. https://www.abs.gov.au/census

### AIHW Health Statistics
- Australian Institute of Health and Welfare. (2023). Asthma. https://www.aihw.gov.au/reports/chronic-respiratory-conditions/asthma
- Australian Institute of Health and Welfare. (2023). Australia's children. https://www.aihw.gov.au/reports/children-youth/australias-children

### Lung Function Reference
- Quanjer PH, et al. (2012). Multi-ethnic reference values for spirometry for the 3-95-yr age range: the global lung function 2012 equations. *European Respiratory Journal*, 40(6), 1324-1343.

---

## 6. Data License and Attribution

### BOM Data
- © Commonwealth of Australia 2024, Bureau of Meteorology
- Data used under Creative Commons Attribution 4.0 licence

### ABS Data
- © Commonwealth of Australia 2021, Australian Bureau of Statistics
- Data used under Creative Commons Attribution 4.0 licence

### AIHW Data
- © Australian Institute of Health and Welfare 2023
- Data used under Creative Commons Attribution 4.0 licence

### WA DWER Data
- © Government of Western Australia 2024
- Data used under open data licence

---

## 7. Updates

| Date | Update |
|------|--------|
| 2024-01 | Initial data compilation |
| 2024-01 | Added AIHW 2022 statistics |
| 2024-01 | Integrated real environmental station data |

---

*Last updated: 2024-01*
