# Real Data Sources for Pediatric Respiratory Geospatial Dashboard

This document catalogs the real data sources used in this dashboard, distinguishing between real aggregate data and synthetic individual-level data.

## Data Source Classification

### REAL DATA (Aggregate Level)

#### 1. Australian Bureau of Statistics (ABS) - Health Data
**Source**: https://www.abs.gov.au/statistics/health/health-conditions-and-risks/asthma/

**Data Available**:
- Asthma prevalence by SA2 (Statistical Area Level 2) - 2017-18 NHS
- Small area estimates for chronic health conditions by age
- State/territory breakdowns for Western Australia
- Demographics: age, sex, socioeconomic status quintiles

**Usage in Dashboard**:
- SA2-level asthma prevalence rates for Perth metropolitan area
- Age-sex distribution patterns for synthetic patient generation
- Socioeconomic gradients in respiratory health

**Citation**: 
- Australian Bureau of Statistics (2018). National Health Survey: First Results, 2017-18. ABS Cat. No. 4364.0.55.001
- ABS (2022). National Health Survey 2022

#### 2. Australian Institute of Health and Welfare (AIHW) - Respiratory Health
**Source**: https://www.aihw.gov.au/reports/chronic-respiratory-conditions/asthma

**Key Statistics Used**:
- 2.8 million Australians (11%) living with asthma (2022)
- Hospitalisation rates: 99 per 100,000 population (2021-22)
- Children 0-14: 225 hospitalisations per 100,000
- Adults 15+: 70 hospitalisations per 100,000
- Asthma accounts for 2.5% of total disease burden
- Leading cause of burden in children aged 1-9 years

**Usage in Dashboard**:
- Validation of synthetic asthma prevalence rates
- Hospitalisation rate benchmarks
- Age-specific exacerbation patterns

**Citation**:
- Australian Institute of Health and Welfare (2023). Asthma. AIHW.

#### 3. Bureau of Meteorology - Climate Data
**Source**: https://www.bom.gov.au/climate/data/

**Data Available**:
- Historical weather station data for Perth metro area
- Temperature, rainfall, humidity records
- Station locations and metadata

**Usage in Dashboard**:
- Real temperature patterns for Perth
- Seasonal variation in environmental exposures
- Weather station locations for map overlay

#### 4. WA Department of Health - Air Quality
**Source**: https://ww2.health.wa.gov.au/Articles/A_E/Air-quality

**Data Available**:
- Air quality monitoring station locations in Perth
- PM2.5, PM10, NO2, O3 measurements
- Historical air quality index data

**Usage in Dashboard**:
- Real air quality station coordinates
- Typical PM2.5 ranges for Perth metro (annual average ~8-15 μg/m³)
- Spatial distribution of monitoring network

### SYNTHETIC DATA (Individual Level)

**Rationale**: Individual-level patient data with exact locations, medical histories, and lung function measurements is not publicly available due to privacy regulations (Privacy Act 1988, Health Records Act 2001 WA).

**Approach**: Synthetic individual-level data is generated to match the statistical properties of real aggregate data.

**Synthetic Data Generation Method**:

1. **Population Structure** (based on ABS Census 2021):
   - Age distribution: 2-18 years pediatric population
   - Sex ratio: Matches ABS reported patterns
   - Geographic distribution: Based on Perth SA2 population densities

2. **Asthma Prevalence** (calibrated to AIHW/ABS):
   - Overall: 11% prevalence (AIHW 2022)
   - Age pattern: Higher in 0-14 (10% boys, 6% girls)
   - Sex crossover: After age 15, females > males

3. **Environmental Exposures** (based on real monitoring):
   - PM2.5: Mean ~12 μg/m³ (based on WA air quality reports)
   - Spatial gradient: Higher in eastern/industrial areas
   - Seasonal patterns: From BoM climate data

4. **Lung Function** (based on Global Lung Initiative reference):
   - FEV1, FVC predicted values by age, sex, height
   - Z-scores calibrated to represent asthmatic vs non-asthmatic
   - Environmental effect sizes from published literature

5. **Health Outcomes** (calibrated to AIHW statistics):
   - Exacerbation rates matching hospitalisation data
   - Medication use patterns from NHS
   - Comorbidity rates from AIHW reports

## Data Integration Approach

### Real Data Layers:
1. **SA2 Boundaries**: ABS digital boundary files
2. **Air Quality Stations**: Real station locations and typical values
3. **Weather Stations**: Real BoM station network
4. **Population Density**: ABS mesh block data

### Synthetic Data Layers:
1. **Individual Patients**: Simulated to match aggregate patterns
2. **Visit Trajectories**: Simulated longitudinal follow-up
3. **Exact Coordinates**: Random within SA2 boundaries, weighted by population

## Validation

The synthetic dataset has been validated against:
- ABS small area estimates for asthma prevalence by SA2
- AIHW hospitalisation rates by age group
- Published literature on environmental health effects
- Global Lung Initiative reference equations for spirometry

## Ethical Considerations

- No real patient data is used
- All individual-level data is synthetic
- Aggregate statistics are from publicly available sources
- Geographic boundaries are public ABS data
- Research methodology follows AIHW and ABS guidelines

## Data Downloads

### For Users Wanting Real Data:

**ABS Health Data**:
```r
# ABS asthma small area estimates
# Download from: https://www.abs.gov.au/statistics/health/health-conditions-and-risks/asthma/2017-18
# File: Table 33 - Small area estimates (SA2 level)
```

**AIHW Data**:
```r
# AIHW asthma statistics
# https://www.aihw.gov.au/reports/chronic-respiratory-conditions/asthma
# Supplementary data tables available for download
```

**BoM Climate Data**:
```r
# Using bomrang package (if available) or direct download
# Station list: http://www.bom.gov.au/climate/data/stations/
```

**ABS Spatial Data**:
```r
# Using absmapsdata package
# install.packages("absmapsdata")
library(absmapsdata)
sa2_perth <- sa22021 %>% filter(gcc_name_2021 == "Greater Perth")
```

## References

1. Australian Bureau of Statistics (2023). National Health Survey 2022. ABS Cat. No. 4364.0.55.001.

2. Australian Institute of Health and Welfare (2023). Asthma. AIHW.

3. Australian Institute of Health and Welfare (2022). Australia's Children. AIHW.

4. Bureau of Meteorology. Climate Data Online. http://www.bom.gov.au/climate/data/

5. WA Department of Health. Air Quality. https://ww2.health.wa.gov.au/Articles/A_E/Air-quality

6. Global Lung Initiative (2012). Multi-ethnic reference values for spirometry. European Respiratory Journal.

7. Dharmage et al. (2019). Epidemiology of asthma in children and adults. Frontiers in Pediatrics.

## Updates

This data documentation will be updated as new real data sources are integrated.
Last updated: 2026-02-08
