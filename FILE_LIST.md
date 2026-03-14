# File List for GitHub Repository

## Summary

- **Total files**: ~170
- **Total size**: ~152 MB
- **R scripts**: 24
- **C++ files**: 2
- **Data files**: ~120 (raw + processed + results)
- **Documentation**: 6

---

## Core Analysis Scripts (R/)

### Data Pipeline
| File | Lines | Description |
|------|-------|-------------|
| `00_run_all_analyses.R` | ~300 | Master script |
| `01_setup_and_configuration.R` | ~280 | Environment setup |
| `02_data_loading_and_cleaning.R` | ~900 | Data download and cleaning |
| `03_data_integration.R` | ~850 | Data merging |

### Econometric Analysis
| File | Lines | Description |
|------|-------|-------------|
| `04_core_econometric_analysis.R` | ~600 | OLS, FE, IV models |
| `05_threshold_regression.R` | ~700 | Threshold regression |
| `05_threshold_regression_manuscript.R` | ~450 | Manuscript version |
| `06_ml_validation.R` | ~800 | ML models + SHAP |
| `07_heterogeneity_analysis.R` | ~350 | Subgroup analysis |
| `08_robustness_checks.R` | ~370 | Robustness tests |
| `09_export_price_shock_iv.R` | ~340 | Export price IV |
| `09_extended_analysis.R` | ~720 | Extended robustness |

### Figure & Table Generation
| File | Lines | Description |
|------|-------|-------------|
| `10_generate_main_figures.R` | ~2,000 | Main figures |
| `11_generate_main_tables.R` | ~1,200 | Main tables |
| `12_generate_supplementary_figures.R` | ~3,200 | Supplementary figures |
| `13_generate_supplementary_tables.R` | ~1,400 | Supplementary tables |

### Extended Analyses
| File | Lines | Description |
|------|-------|-------------|
| `14_fine_grained_heterogeneity.R` | ~730 | Detailed heterogeneity |
| `15_dynamic_effects_analysis.R` | ~700 | Dynamic effects |
| `16_nonlinear_threshold_analysis.R` | ~530 | Nonlinear models |
| `17_alternative_health_outcomes.R` | ~380 | Alternative outcomes |
| `18_spatial_spillover_analysis.R` | ~410 | Spatial models |
| `19_cost_effectiveness_analysis.R` | ~360 | Cost-effectiveness |
| `20_environmental_health_interaction.R` | ~460 | Environment interaction |
| `21_generate_extended_analysis_figures.R` | ~510 | Extended figures |

---

## Tools (tools/)

| File | Description |
|------|-------------|
| `threshold_cpp.cpp` | C++ threshold search algorithm |
| `threshold_cpp_enhanced.cpp` | Enhanced C++ implementation |
| `setup_fonts.R` | Font configuration |
| `backup_and_restore.R` | Backup utilities |

---

## Documentation

| File | Description |
|------|-------------|
| `README.md` | Main documentation |
| `LICENSE` | MIT License |
| `.gitignore` | Git ignore rules |
| `FILE_LIST.md` | This file |

---

## Before Uploading to GitHub

### Checklist

- [ ] Update `README.md` with actual author names and email
- [ ] Update `LICENSE` with actual author name
- [ ] Review all scripts for any hardcoded local paths
- [ ] Ensure no sensitive data or API keys are included
- [ ] Test that `00_run_all_analyses.R` runs without errors
- [ ] Add data download instructions if data files are not included

### Optional Additions

- [ ] Add sample data or data dictionary
- [ ] Add expected output examples
- [ ] Add troubleshooting guide
- [ ] Set up GitHub Actions for automated testing

---

*Last updated: 2026-03-12*
