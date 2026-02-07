# Data Pipeline Status

## Current Status: ✅ REAL DATA INTEGRATED

This dashboard now uses **real datasets** from Australian government sources wherever possible.

### Real Data Sources (Implemented)

| Source | Data | Status |
|--------|------|--------|
| **BOM** | Weather station locations, climate normals | ✅ Real |
| **WA DWER** | Air quality monitoring stations, annual averages | ✅ Real |
| **ABS** | SA2 boundaries, population estimates, SEIFA | ✅ Real |
| **AIHW** | Asthma prevalence, hospitalisation rates | ✅ Real |

### Synthetic Data (Calibrated to Real Statistics)

| Data | Status | Justification |
|------|--------|---------------|
| Individual patient records | Synthetic | Privacy - individual health data not public |
| Daily weather values | Synthetic | Based on real climate normals |
| Daily air quality values | Synthetic | Based on real annual averages |

### Key Statistics (Real)

- **Asthma prevalence (children 0-18)**: 9.3% (AIHW 2022)
- **Hospitalisation rate (children 0-14)**: 225 per 100,000 (AIHW 2021-22)
- **Perth children population**: ~125,000 across 30 SA2 areas (ABS 2021)
- **Air quality stations**: 11 real DWER monitoring stations
- **Weather stations**: 10 real BOM stations

## Data Pipeline Scripts

```bash
# Run complete pipeline
cd data-raw
Rscript run_data_pipeline.R

# Or run individually:
Rscript download_environmental_data.R  # Real BOM, DWER, ABS, AIHW data
Rscript generate_realistic_patients.R   # Synthetic patients calibrated to real stats
```

## Documentation

- `DATA_SOURCES.md` - Complete documentation of all data sources
- `data-raw/README.md` - Instructions for data downloads
- Each script includes inline documentation

## Attribution

All real data sources are properly attributed with:
- Source URLs
- Data collection dates
- License information
- Citation requirements
