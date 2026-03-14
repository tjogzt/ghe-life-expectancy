# Data Documentation

## Overview

This folder contains all data required to replicate the analysis in the study "How institutional capacity shapes the government health expenditure–life expectancy association".

---

## Directory Structure

```
data/
├── raw/                    # Original downloaded data
│   ├── *.csv               # Main indicator files
│   ├── wdi/                # World Development Indicators
│   ├── who/                # World Health Organization data
│   ├── gbd/                # Global Burden of Disease data
│   ├── imf/                # International Monetary Fund data
│   └── comtrade/           # UN Comtrade export data
├── processed/              # Cleaned and integrated data
│   ├── 01_*.rds           # Cleaned data from Step 1
│   ├── 02_*.rds           # Integrated panel data from Step 2
│   └── *.csv              # Summary statistics
└── DATA_README.md          # This file
```

---

## Data Sources

### Primary Data Sources

| Source | Description | Variables | Coverage |
|--------|-------------|-----------|----------|
| **World Bank WDI** | Development indicators | Life expectancy, GDP per capita, population | 190 countries, 2000–2022 |
| **WHO GHED** | Health expenditure | GHE (% of GDP), public health spending | 190 countries, 2000–2022 |
| **World Bank WGI** | Governance indicators | Government effectiveness, regulatory quality, rule of law, control of corruption | 190 countries, 1996–2022 |

### Instrumental Variables

| Source | Description | Use |
|--------|-------------|-----|
| **UN Comtrade** | Export commodity data | Export price shock IV |
| **IMF** | Fiscal indicators | Fiscal capacity IV |
| **WGI Historical** | Historical governance | Historical governance IV |

### Supplementary Data

| Source | Description |
|--------|-------------|
| **GBD** | Alternative health outcomes (HALE, disease burden) |
| **OWID** | COVID-19 pandemic indicators |

---

## File Descriptions

### Raw Data Files (`data/raw/`)

| File | Description |
|------|-------------|
| `world_governance_indicators.csv` | WGI data: 6 governance dimensions |
| `fiscal_capacity_indicators.csv` | Fiscal capacity measures |
| `geographic_variables.csv` | Geographic controls |
| `historical_variables.csv` | Historical institutional proxies |
| `institutional_quality_indicators.csv` | Additional institutional measures |
| `mechanism_variables.csv` | Service access, quality, accountability |
| `population_density.csv` | Population density data |
| `covid_owid_annual.csv` | COVID-19 indicators (annual) |

### Processed Data Files (`data/processed/`)

| File | Description |
|------|-------------|
| `01_cleaned_data_wb.rds` | Cleaned World Bank data |
| `02_integrated_panel_data.rds` | **Main analysis dataset** (4,310 obs × 190 countries) |
| `02_variable_dictionary.csv` | Variable definitions |
| `export_price_shocks.rds` | Computed export price shock IV |

---

## Key Variables

### Outcome Variable
- `life_expectancy`: Life expectancy at birth (years)

### Main Exposure
- `ghe_gdp`: Government health expenditure (% of GDP)

### Institutional Capacity (Threshold Variable)
- `government_effectiveness`: WGI Government Effectiveness index (standardised)

### Control Variables
- `gdp_per_capita`: GDP per capita (log)
- `population`: Total population (log)
- `urbanisation`: Urban population (%)
- `education_expenditure`: Public education spending (% of GDP)

### Instrumental Variables
- `export_price_shock`: Commodity price × export share interaction
- `fiscal_capacity`: Tax revenue and budget indicators
- `historical_governance`: Colonial/historical institutional proxies

---

## Data Reproducibility

### Automatic Download

Running `R/02_data_loading_and_cleaning.R` will automatically:
1. Check for missing data files
2. Download from World Bank API (WDI package) if needed
3. Clean and standardise all variables
4. Save processed data to `data/processed/`

### Manual Download (if API fails)

If automatic download fails, data can be manually obtained from:

1. **World Bank Open Data**: https://data.worldbank.org
2. **WHO GHED**: https://apps.who.int/nha/database
3. **WGI**: https://info.worldbank.org/governance/wgi/
4. **GBD**: https://vizhub.healthdata.org/gbd-results/
5. **UN Comtrade**: https://comtradeplus.un.org/

---

## Data Quality Notes

1. **Missing data**: ~15% of observations have missing values; handled via multiple imputation (see Methods)
2. **Country coverage**: 190 countries with ≥10 years of data each
3. **Time period**: 2000–2022 (23 years)
4. **Balanced panel**: Not strictly balanced; country-year observations vary

---

## Citation

If using this data, please cite:
- Original data sources (World Bank, WHO, etc.)
- This study: [Citation pending]

---

*Last updated: 2026-03-12*
