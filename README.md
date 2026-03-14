# How Institutional Capacity Shapes the Government Health Expenditure–Life Expectancy Association

## Replication Code Repository

This repository contains the complete replication code for the study:

**"How institutional capacity shapes the government health expenditure–life expectancy association: a threshold and multi-method analysis across 190 countries"**

---

## Overview

This study examines how **government health expenditure (GHE)** relates to **life expectancy at birth** across 190 countries (2000–2022), with a focus on the moderating role of **institutional capacity**. We employ a multi-method approach combining:

- Fixed-effects panel regression
- Instrumental variables (IV) estimation
- Threshold regression analysis
- Machine learning validation and interpretability (SHAP)

**Key finding**: The association between GHE and life expectancy is non-linear and strongly conditioned by institutional capacity. We identify a governance threshold below which the GHE–life expectancy association is small or near zero, and above which additional GHE is associated with negative returns.

---

## Repository Structure

```
github_code/
├── R/                              # Main analysis scripts (24 files)
│   ├── 00_run_all_analyses.R       # Master script to run all analyses
│   ├── 01_setup_and_configuration.R
│   ├── 02_data_loading_and_cleaning.R
│   ├── 03_data_integration.R
│   ├── 04_core_econometric_analysis.R
│   ├── 05_threshold_regression.R
│   ├── 05_threshold_regression_manuscript.R
│   ├── 06_ml_validation.R
│   ├── 07–09_*.R                   # Heterogeneity, robustness, IV
│   ├── 10–13_*.R                   # Figure and table generation
│   └── 14–21_*.R                   # Extended analyses
├── data/                           # Data files (~140 MB)
│   ├── raw/                        # Original data (20 files)
│   │   ├── *.csv                   # Main indicator files
│   │   ├── wdi/                    # World Development Indicators
│   │   ├── who/                    # WHO health data
│   │   ├── gbd/                    # Global Burden of Disease
│   │   ├── imf/                    # IMF fiscal data
│   │   └── comtrade/               # UN Comtrade exports
│   ├── processed/                  # Cleaned data (14 files)
│   │   ├── 01_cleaned_data_*.rds
│   │   ├── 02_integrated_panel_data.rds  # Main analysis dataset
│   │   └── export_price_shocks.rds
│   └── DATA_README.md              # Data documentation
├── results/                        # Analysis outputs
│   └── analysis/                   # Model results (83 files)
├── tools/                          # Utility scripts
│   ├── threshold_cpp.cpp           # C++ threshold algorithm
│   ├── threshold_cpp_enhanced.cpp
│   ├── setup_fonts.R
│   └── backup_and_restore.R
├── figures/                        # Output figures (empty, generated)
├── tables/                         # Output tables (empty, generated)
├── logs/                           # Log files (empty)
├── README.md                       # This file
├── FILE_LIST.md                    # Complete file list
├── .gitignore
└── LICENSE
```

---

## Quick Start

### Prerequisites

- **R** >= 4.0.0
- **LaTeX** (for PDF table generation; install via `tinytex::install_tinytex()`)

### Required R Packages

```r
# Data manipulation
install.packages(c("tidyverse", "data.table", "haven", "readxl"))

# Econometric analysis
install.packages(c("fixest", "plm", "AER", "ivreg", "sandwich", "lmtest"))

# Threshold regression
install.packages(c("pdR", "strucchange"))

# Machine learning
install.packages(c("randomForest", "xgboost", "caret", "gbm", "glmnet"))

# Visualisation
install.packages(c("ggplot2", "cowplot", "gridExtra", "scales", "RColorBrewer", "ggrepel"))

# Tables
install.packages(c("kableExtra", "flextable", "officer", "gt"))

# SHAP values
install.packages("shapr")
# or
devtools::install_github("ModelOriented/shapper")
```

### Running the Analysis

**Option 1: Run all analyses (recommended for full replication)**

```r
setwd("path/to/github_code")
source("R/00_run_all_analyses.R")
```

Estimated runtime: 30–60 minutes depending on hardware.

**Option 2: Run individual scripts**

```r
# Step 1: Setup and data preparation
source("R/01_setup_and_configuration.R")
source("R/02_data_loading_and_cleaning.R")
source("R/03_data_integration.R")

# Step 2: Core analyses
source("R/04_core_econometric_analysis.R")
source("R/05_threshold_regression_manuscript.R")
source("R/06_ml_validation.R")

# Step 3: Generate figures and tables
source("R/10_generate_main_figures.R")
source("R/11_generate_main_tables.R")
source("R/12_generate_supplementary_figures.R")
source("R/13_generate_supplementary_tables.R")
```

---

## Data Sources

The analysis integrates data from seven international sources:

| Source | Variables | Coverage |
|--------|-----------|----------|
| **World Bank WDI** | Life expectancy, GDP per capita, population | 190 countries, 2000–2022 |
| **WHO GHED** | Government health expenditure (% of GDP) | 190 countries, 2000–2022 |
| **World Bank WGI** | Government effectiveness, regulatory quality, rule of law, control of corruption | 190 countries, 2000–2022 |
| **UN Comtrade** | Export price shocks (IV) | 180+ countries |
| **IMF GFS** | Fiscal capacity indicators (IV) | 150+ countries |
| **V-Dem** | Historical governance indicators (IV) | 180 countries |
| **GBD** | Alternative health outcomes (HALE, disease burden) | 190 countries |

**Note**: Raw data files are not included in this repository due to size and licensing constraints. Scripts in `02_data_loading_and_cleaning.R` include code to download data from original sources.

---

## Script Descriptions

### Core Analysis Pipeline (00–09)

| Script | Description |
|--------|-------------|
| `00_run_all_analyses.R` | Master script; runs all analyses sequentially |
| `01_setup_and_configuration.R` | Package loading, path configuration, global settings |
| `02_data_loading_and_cleaning.R` | Data download, cleaning, and validation |
| `03_data_integration.R` | Merging datasets, creating analysis panel |
| `04_core_econometric_analysis.R` | OLS, fixed effects, and IV estimation |
| `05_threshold_regression.R` | Threshold regression with bootstrap CI |
| `05_threshold_regression_manuscript.R` | Manuscript-specific threshold analysis |
| `06_ml_validation.R` | Random Forest, XGBoost, SHAP analysis |
| `07_heterogeneity_analysis.R` | Subgroup analyses by income, region |
| `08_robustness_checks.R` | Alternative specifications, sample restrictions |
| `09_export_price_shock_iv.R` | Commodity export price shock IV |
| `09_extended_analysis.R` | Additional robustness and sensitivity |

### Figure and Table Generation (10–13)

| Script | Description |
|--------|-------------|
| `10_generate_main_figures.R` | Figures 1–3 for main text |
| `11_generate_main_tables.R` | Tables 1–3 for main text |
| `12_generate_supplementary_figures.R` | Figures S1–S13 |
| `13_generate_supplementary_tables.R` | Tables S1–S11 |

### Extended Analyses (14–21)

| Script | Description |
|--------|-------------|
| `14_fine_grained_heterogeneity.R` | Detailed subgroup heterogeneity |
| `15_dynamic_effects_analysis.R` | Distributed lag models |
| `16_nonlinear_threshold_analysis.R` | Alternative threshold specifications |
| `17_alternative_health_outcomes.R` | HALE, infant mortality outcomes |
| `18_spatial_spillover_analysis.R` | Spatial econometric models |
| `19_cost_effectiveness_analysis.R` | Cost-effectiveness calculations |
| `20_environmental_health_interaction.R` | Environment–health interactions |
| `21_generate_extended_analysis_figures.R` | Extended analysis visualisations |

---

## Output Structure

After running all analyses, outputs are saved to:

```
results/                    # RDS files with model objects
figures_final/
├── main/                   # Main text figures (PDF)
└── supplementary/          # Supplementary figures (PDF)
tables_final/
├── main/                   # Main text tables (PDF, DOCX)
└── supplementary/          # Supplementary tables (PDF)
```

---

## Reproducibility

- **Random seed**: All stochastic procedures use `set.seed(42)` for reproducibility.
- **Software versions**: Developed with R 4.3.x; tested on macOS and Linux.
- **Bootstrap**: 1,000 replications for confidence intervals; country-level resampling.

---

## Citation

If you use this code, please cite:

```
[Author names]. (2026). How institutional capacity shapes the government health 
expenditure–life expectancy association: a threshold and multi-method analysis 
across 190 countries. The Lancet Global Health. [DOI pending]
```

---
## License

This project is licensed under the MIT License. See [LICENSE](LICENSE) for details.

---

## Contact

For questions or issues, please open a GitHub issue or contact:

- **Corresponding author**: tjogzt@gmail。com
- **Repository maintainer**: tjogzt


---

## Acknowledgements

We thank [funding sources] for financial support and [data providers] for making their data publicly available.
