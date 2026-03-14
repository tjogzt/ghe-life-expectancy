# =============================================================================
# 03_core_econometric_analysis.R
# Core Econometric Analysis | 核心计量经济分析
# =============================================================================
#
# 【功能概述 | Function Overview】
# 执行核心计量经济分析
# Perform core econometric analysis:
# - Fixed Effects (FE) regression | 固定效应回归
# - Instrumental Variables (IV) estimation | 工具变量估计
# - Dynamic Panel GMM | 动态面板GMM
# - Robustness checks | 稳健性检验
#
# 【输入文件 | Input Files】
# - data/processed/02_integrated_panel_data.rds
#
# 【输出文件 | Output Files】
# - results/analysis/03_fe_results.rds
# - results/analysis/03_iv_results.rds
# - results/analysis/03_gmm_results.rds
# - results/analysis/03_analysis_summary.csv
# - tables/Table_2_Main_Results.csv
#
# 【运行时间 | Runtime】
# ~ 10-15 minutes | ~10-15分钟
#
# 【最后更新 | Last Updated】
# 2025-11-06 - Added three new IV strategies: fiscal, governance, geographic
# =============================================================================

# 运行配置脚本 | Run configuration script
if (!exists("CONFIG")) {
    source("01_setup_and_configuration.R")
}

cat("\n")
cat("=============================================================================\n")
cat("  03 - Core Econometric Analysis\n")
cat("=============================================================================\n\n")

log_message("Starting core econometric analysis")

# =============================================================================
# 1. 加载整合数据 | Load Integrated Data
# =============================================================================

cat("\n[STEP 1/6] Loading integrated panel data...\n")

panel_data <- readRDS(file.path("..", "data", "processed", "02_integrated_panel_data.rds"))
cat(sprintf(
    "[OK] Loaded panel data: %d observations, %d countries\n",
    nrow(panel_data), n_distinct(panel_data$iso3c)
))

# 创建面板数据对象 | Create panel data object
panel_data_clean <- panel_data %>%
    # 移除有缺失值的观测（用于回归分析）| Remove observations with missing values (for regression)
    filter(!is.na(che_gdp), !is.na(ghe_gdp), !is.na(life_exp), !is.na(gdp_pc)) %>%
    # 创建时间趋势变量 | Create time trend variable
    mutate(time_trend = year - min(year) + 1) %>%
    # 按国家和年份排序 | Sort by country and year
    arrange(iso3c, year)

cat(sprintf("[OK] Analysis-ready data: %d observations\n", nrow(panel_data_clean)))

# 转换为pdata.frame（plm包需要）| Convert to pdata.frame (required by plm package)
pdata <- pdata.frame(panel_data_clean, index = c("iso3c", "year"))

# =============================================================================
# 2. 固定效应回归分析 | Fixed Effects Regression Analysis
# =============================================================================

cat("\n[STEP 2/6] Running Fixed Effects regressions...\n")

# 模型1: 基准FE模型 | Model 1: Baseline FE model
cat("[INFO] Estimating Model 1: Baseline FE...\n")

fe_model1 <- plm(
    life_exp ~ ghe_gdp + log_gdp_pc + time_trend,
    data = pdata,
    model = "within", # Fixed effects
    effect = "twoways" # Two-way fixed effects (country + year)
)

cat("[OK] Model 1 estimated\n")

# 模型2: 添加控制变量 | Model 2: With additional controls
cat("[INFO] Estimating Model 2: FE with controls...\n")

fe_model2 <- plm(
    life_exp ~ ghe_gdp + che_gdp + log_gdp_pc + log_population + time_trend,
    data = pdata,
    model = "within",
    effect = "twoways"
)

cat("[OK] Model 2 estimated\n")

# 计算稳健标准误 | Calculate robust standard errors
fe_model1_robust <- coeftest(fe_model1, vcov = vcovHC(fe_model1, type = "HC1"))
fe_model2_robust <- coeftest(fe_model2, vcov = vcovHC(fe_model2, type = "HC1"))

cat("[OK] Robust standard errors calculated\n")

# 保存FE结果 | Save FE results
fe_results <- list(
    model1 = fe_model1,
    model2 = fe_model2,
    model1_robust = fe_model1_robust,
    model2_robust = fe_model2_robust
)

saveRDS(fe_results, file.path("..", "results", "analysis", "03_fe_results.rds"))
cat("[OK] FE results saved\n")

# =============================================================================
# 3. 工具变量估计 | Instrumental Variables Estimation
# =============================================================================
# 实现三套IV策略，按照手稿描述：
# 1. 财政能力IV (Fiscal Capacity)
# 2. 治理/制度IV (Governance/Institutional)
# 3. 地理/历史IV (Geographic/Historical)
# 4. 传统滞后值IV (Lagged)

cat("\n[STEP 3/6] Running IV regressions...\n")

# 初始化结果列表
iv_results <- list()

# =========================================================================
# 3.1 财政能力工具变量 | Fiscal Capacity Instruments
# =========================================================================
cat("\n[3.1] Fiscal Capacity IV Strategy...\n")

iv_fiscal_data <- panel_data_clean %>%
    filter(!is.na(tax_revenue_gdp) | !is.na(government_revenue_gdp))

cat(sprintf("[INFO] Fiscal capacity IV sample: %d observations\n", nrow(iv_fiscal_data)))

if (nrow(iv_fiscal_data) >= 100) {
    tryCatch({
        iv_fiscal <- ivreg(
            life_exp ~ ghe_gdp + log_gdp_pc + log_population + time_trend |
                log_gdp_pc + log_population + time_trend + tax_revenue_gdp + government_revenue_gdp,
            data = iv_fiscal_data
        )
        
        iv_fiscal_robust <- coeftest(iv_fiscal, vcov = vcovHC(iv_fiscal, type = "HC1"))
        iv_fiscal_summary <- summary(iv_fiscal, diagnostics = TRUE)
        
        cat("[OK] Fiscal capacity IV estimated\n")
        cat(sprintf("    GHE coefficient: %.4f\n", coef(iv_fiscal)["ghe_gdp"]))
        cat(sprintf("    First-stage F: %.2f\n", 
            iv_fiscal_summary$diagnostics["Weak instruments", "statistic"]))
        
        iv_results$fiscal <- list(
            model = iv_fiscal,
            robust = iv_fiscal_robust,
            summary = iv_fiscal_summary,
            first_stage_F = iv_fiscal_summary$diagnostics["Weak instruments", "statistic"],
            n_obs = nrow(iv_fiscal_data)
        )
    }, error = function(e) {
        cat(sprintf("[WARN] Fiscal capacity IV failed: %s\n", e$message))
        iv_results$fiscal <<- NULL
    })
} else {
    cat("[WARN] Insufficient observations for fiscal capacity IV\n")
    iv_results$fiscal <- NULL
}

# =========================================================================
# 3.2 治理/制度工具变量 | Governance/Institutional Instruments
# =========================================================================
cat("\n[3.2] Governance/Institutional IV Strategy...\n")

iv_gov_data <- panel_data_clean %>%
    filter(!is.na(government_effectiveness) & !is.na(rule_of_law))

cat(sprintf("[INFO] Governance IV sample: %d observations\n", nrow(iv_gov_data)))

if (nrow(iv_gov_data) >= 100) {
    tryCatch({
        iv_governance <- ivreg(
            life_exp ~ ghe_gdp + log_gdp_pc + log_population + time_trend |
                log_gdp_pc + log_population + time_trend + government_effectiveness + 
                rule_of_law + regulatory_quality,
            data = iv_gov_data
        )
        
        iv_gov_robust <- coeftest(iv_governance, vcov = vcovHC(iv_governance, type = "HC1"))
        iv_gov_summary <- summary(iv_governance, diagnostics = TRUE)
        
        cat("[OK] Governance IV estimated\n")
        cat(sprintf("    GHE coefficient: %.4f\n", coef(iv_governance)["ghe_gdp"]))
        cat(sprintf("    First-stage F: %.2f\n",
            iv_gov_summary$diagnostics["Weak instruments", "statistic"]))
        
        iv_results$governance <- list(
            model = iv_governance,
            robust = iv_gov_robust,
            summary = iv_gov_summary,
            first_stage_F = iv_gov_summary$diagnostics["Weak instruments", "statistic"],
            n_obs = nrow(iv_gov_data)
        )
    }, error = function(e) {
        cat(sprintf("[WARN] Governance IV failed: %s\n", e$message))
        iv_results$governance <<- NULL
    })
} else {
    cat("[WARN] Insufficient observations for governance IV\n")
    iv_results$governance <- NULL
}

# =========================================================================
# 3.3 地理/历史工具变量 | Geographic/Historical Instruments
# =========================================================================
# 使用：landlocked + cv_elevation (地形崎岖度变异系数)
# cv_elevation是地形崎岖度的良好代理，来自geodata包计算的海拔变异系数
cat("\n[3.3] Geographic IV Strategy (with Terrain Ruggedness)...\n")

iv_geo_data <- panel_data_clean %>%
    filter(!is.na(landlocked) & !is.na(cv_elevation))

cat(sprintf("[INFO] Geographic IV sample: %d observations\n", nrow(iv_geo_data)))
cat(sprintf("[INFO] Countries with terrain data: %d\n", n_distinct(iv_geo_data$iso3c)))

if (nrow(iv_geo_data) >= 100) {
    tryCatch({
        iv_geographic <- ivreg(
            life_exp ~ ghe_gdp + log_gdp_pc + log_population + time_trend |
                log_gdp_pc + log_population + time_trend + landlocked + cv_elevation,
            data = iv_geo_data
        )
        
        iv_geo_robust <- coeftest(iv_geographic, vcov = vcovHC(iv_geographic, type = "HC1"))
        iv_geo_summary <- summary(iv_geographic, diagnostics = TRUE)
        
        cat("[OK] Geographic IV estimated\n")
        cat(sprintf("    GHE coefficient: %.4f\n", coef(iv_geographic)["ghe_gdp"]))
        cat(sprintf("    First-stage F: %.2f\n",
            iv_geo_summary$diagnostics["Weak instruments", "statistic"]))
        
        iv_results$geographic <- list(
            model = iv_geographic,
            robust = iv_geo_robust,
            summary = iv_geo_summary,
            first_stage_F = iv_geo_summary$diagnostics["Weak instruments", "statistic"],
            n_obs = nrow(iv_geo_data)
        )
    }, error = function(e) {
        cat(sprintf("[WARN] Geographic IV failed: %s\n", e$message))
        iv_results$geographic <<- NULL
    })
} else {
    cat("[WARN] Insufficient observations for geographic IV\n")
    iv_results$geographic <- NULL
}

# =========================================================================
# 3.4 传统滞后值IV (用于比较) | Traditional Lagged IV (for comparison)
# =========================================================================
cat("\n[3.4] Traditional Lagged IV (baseline)...\n")

iv_lag_data <- panel_data_clean %>%
    group_by(iso3c) %>%
    arrange(year) %>%
    mutate(
        ghe_gdp_lag2 = lag(ghe_gdp, 2),
        ghe_gdp_lag3 = lag(ghe_gdp, 3)
    ) %>%
    ungroup() %>%
    filter(!is.na(ghe_gdp_lag2) & !is.na(ghe_gdp_lag3))

cat(sprintf("[INFO] Lagged IV sample: %d observations\n", nrow(iv_lag_data)))

if (nrow(iv_lag_data) >= 100) {
    tryCatch({
        iv_lagged <- ivreg(
            life_exp ~ ghe_gdp + log_gdp_pc + time_trend |
                log_gdp_pc + time_trend + ghe_gdp_lag2 + ghe_gdp_lag3,
            data = iv_lag_data
        )
        
        iv_lag_robust <- coeftest(iv_lagged, vcov = vcovHC(iv_lagged, type = "HC1"))
        iv_lag_summary <- summary(iv_lagged, diagnostics = TRUE)
        
        cat("[OK] Lagged IV estimated\n")
        cat(sprintf("    GHE coefficient: %.4f\n", coef(iv_lagged)["ghe_gdp"]))
        cat(sprintf("    First-stage F: %.2f\n",
            iv_lag_summary$diagnostics["Weak instruments", "statistic"]))
        
        iv_results$lagged <- list(
            model = iv_lagged,
            robust = iv_lag_robust,
            summary = iv_lag_summary,
            first_stage_F = iv_lag_summary$diagnostics["Weak instruments", "statistic"],
            n_obs = nrow(iv_lag_data)
        )
    }, error = function(e) {
        cat(sprintf("[WARN] Lagged IV failed: %s\n", e$message))
        iv_results$lagged <<- NULL
    })
} else {
    cat("[WARN] Insufficient observations for lagged IV\n")
    iv_results$lagged <- NULL
}

# =========================================================================
# 保存所有IV结果 | Save all IV results
# =========================================================================
cat("\n[INFO] Summary of IV estimations:\n")
cat("---------------------------------------\n")
if (!is.null(iv_results$fiscal)) {
    cat(sprintf("  Fiscal capacity IV: β = %.4f (N = %d)\n", 
        coef(iv_results$fiscal$model)["ghe_gdp"], 
        iv_results$fiscal$n_obs))
}
if (!is.null(iv_results$governance)) {
    cat(sprintf("  Governance IV: β = %.4f (N = %d)\n",
        coef(iv_results$governance$model)["ghe_gdp"],
        iv_results$governance$n_obs))
}
if (!is.null(iv_results$geographic)) {
    cat(sprintf("  Geographic IV: β = %.4f (N = %d)\n",
        coef(iv_results$geographic$model)["ghe_gdp"],
        iv_results$geographic$n_obs))
}
if (!is.null(iv_results$lagged)) {
    cat(sprintf("  Lagged IV: β = %.4f (N = %d)\n",
        coef(iv_results$lagged$model)["ghe_gdp"],
        iv_results$lagged$n_obs))
}
cat("---------------------------------------\n")

saveRDS(iv_results, file.path("..", "results", "analysis", "03_iv_results.rds"))
cat("[OK] All IV results saved\n")

# =============================================================================
# 4. 动态面板GMM估计 | Dynamic Panel GMM Estimation
# =============================================================================

cat("\n[STEP 4/6] Running Dynamic Panel GMM...\n")

# 创建动态面板数据 | Create dynamic panel data
gmm_data <- panel_data_clean %>%
    group_by(iso3c) %>%
    arrange(year) %>%
    mutate(
        life_exp_lag1 = lag(life_exp, 1)
    ) %>%
    ungroup() %>%
    filter(!is.na(life_exp_lag1))

cat(sprintf("[INFO] GMM analysis sample: %d observations\n", nrow(gmm_data)))

if (nrow(gmm_data) > 100) {
    # 转换为pdata.frame | Convert to pdata.frame
    pdata_gmm <- pdata.frame(gmm_data, index = c("iso3c", "year"))

    cat("[INFO] Estimating System GMM model...\n")

    tryCatch(
        {
            # 系统GMM估计 | System GMM estimation
            gmm_model <- pgmm(
                life_exp ~ lag(life_exp, 1) + ghe_gdp + log_gdp_pc |
                    lag(life_exp, 2:3) + lag(ghe_gdp, 2:3),
                data = pdata_gmm,
                effect = "twoways",
                model = "twosteps",
                transformation = "ld"
            )

            cat("[OK] GMM model estimated\n")

            # Sargan/Hansen检验（过度识别检验）| Sargan/Hansen test (overidentification test)
            gmm_summary <- summary(gmm_model)

            # 保存GMM结果 | Save GMM results
            gmm_results <- list(
                model = gmm_model,
                summary = gmm_summary
            )

            saveRDS(gmm_results, file.path("..", "results", "analysis", "03_gmm_results.rds"))
            cat("[OK] GMM results saved\n")
        },
        error = function(e) {
            cat(sprintf("[WARN] GMM estimation failed: %s\n", e$message))
            gmm_results <<- NULL
        }
    )
} else {
    cat("[WARN] Insufficient observations for GMM analysis\n")
    gmm_results <- NULL
}

# =============================================================================
# 5. 创建结果汇总表 | Create Results Summary Table
# =============================================================================

cat("\n[STEP 5/6] Creating results summary table...\n")

# 提取系数和标准误 | Extract coefficients and standard errors
extract_coef <- function(model, var_name) {
    if (is.null(model)) {
        return(c(NA, NA, NA))
    }

    tryCatch(
        {
            coef_val <- coef(model)[var_name]
            se_val <- sqrt(diag(vcov(model)))[var_name]
            t_val <- coef_val / se_val
            p_val <- 2 * pt(abs(t_val), df = model$df.residual, lower.tail = FALSE)

            return(c(coef_val, se_val, p_val))
        },
        error = function(e) {
            return(c(NA, NA, NA))
        }
    )
}

# 创建主结果表（类似Table 2）| Create main results table (similar to Table 2)
main_results_table <- tibble(
    Variable = c(
        "Government Health Expenditure (% GDP)",
        "Total Health Expenditure (% GDP)",
        "Log GDP per capita",
        "Log Population",
        "Time Trend"
    ),

    # FE Model 1
    FE_Model1_Coef = c(
        extract_coef(fe_model1, "ghe_gdp")[1],
        NA,
        extract_coef(fe_model1, "log_gdp_pc")[1],
        NA,
        extract_coef(fe_model1, "time_trend")[1]
    ),
    FE_Model1_SE = c(
        extract_coef(fe_model1, "ghe_gdp")[2],
        NA,
        extract_coef(fe_model1, "log_gdp_pc")[2],
        NA,
        extract_coef(fe_model1, "time_trend")[2]
    ),

    # FE Model 2
    FE_Model2_Coef = c(
        extract_coef(fe_model2, "ghe_gdp")[1],
        extract_coef(fe_model2, "che_gdp")[1],
        extract_coef(fe_model2, "log_gdp_pc")[1],
        extract_coef(fe_model2, "log_population")[1],
        extract_coef(fe_model2, "time_trend")[1]
    ),
    FE_Model2_SE = c(
        extract_coef(fe_model2, "ghe_gdp")[2],
        extract_coef(fe_model2, "che_gdp")[2],
        extract_coef(fe_model2, "log_gdp_pc")[2],
        extract_coef(fe_model2, "log_population")[2],
        extract_coef(fe_model2, "time_trend")[2]
    ),

    # IV Model (if available) - using fiscal capacity IV as main IV
    IV_Coef = if (!is.null(iv_results$fiscal)) {
        c(
            extract_coef(iv_results$fiscal$model, "ghe_gdp")[1],
            NA,
            extract_coef(iv_results$fiscal$model, "log_gdp_pc")[1],
            NA,
            extract_coef(iv_results$fiscal$model, "time_trend")[1]
        )
    } else {
        rep(NA, 5)
    },
    IV_SE = if (!is.null(iv_results$fiscal)) {
        c(
            extract_coef(iv_results$fiscal$model, "ghe_gdp")[2],
            NA,
            extract_coef(iv_results$fiscal$model, "log_gdp_pc")[2],
            NA,
            extract_coef(iv_results$fiscal$model, "time_trend")[2]
        )
    } else {
        rep(NA, 5)
    }
)

# 添加模型统计信息 | Add model statistics
model_stats <- tibble(
    Variable = c("Observations", "R-squared", "Countries", "F-statistic"),
    FE_Model1_Coef = c(
        nobs(fe_model1),
        summary(fe_model1)$r.squared[1],
        n_distinct(panel_data_clean$iso3c),
        summary(fe_model1)$fstatistic$statistic
    ),
    FE_Model1_SE = rep(NA, 4),
    FE_Model2_Coef = c(
        nobs(fe_model2),
        summary(fe_model2)$r.squared[1],
        n_distinct(panel_data_clean$iso3c),
        summary(fe_model2)$fstatistic$statistic
    ),
    FE_Model2_SE = rep(NA, 4),
    IV_Coef = if (!is.null(iv_results$fiscal)) {
        c(iv_results$fiscal$n_obs, NA, n_distinct(panel_data_clean$iso3c), NA)
    } else {
        rep(NA, 4)
    },
    IV_SE = rep(NA, 4)
)

# 合并结果 | Combine results
full_results_table <- bind_rows(main_results_table, model_stats)

# 保存结果表 | Save results table
write_csv(full_results_table, file.path("..", "tables", "Table_2_Main_Results.csv"))
cat("[OK] Main results table saved\n")

# =============================================================================
# 6. 诊断检验 | Diagnostic Tests
# =============================================================================

cat("\n[STEP 6/6] Running diagnostic tests...\n")

# Hausman检验（FE vs RE）| Hausman test (FE vs RE)
cat("[INFO] Performing Hausman test...\n")

re_model <- plm(
    life_exp ~ ghe_gdp + log_gdp_pc + time_trend,
    data = pdata,
    model = "random",
    effect = "twoways"
)

hausman_test <- phtest(fe_model1, re_model)
cat(sprintf("[OK] Hausman test p-value: %.4f\n", hausman_test$p.value))

# 序列相关检验 | Serial correlation test
cat("[INFO] Testing for serial correlation...\n")

serial_test <- pbgtest(fe_model1)
cat(sprintf("[OK] Serial correlation test p-value: %.4f\n", serial_test$p.value))

# 异方差检验 | Heteroskedasticity test
cat("[INFO] Testing for heteroskedasticity...\n")

# 使用Breusch-Pagan检验 | Use Breusch-Pagan test
bp_test <- bptest(fe_model1)
cat(sprintf("[OK] Breusch-Pagan test p-value: %.4f\n", bp_test$p.value))

# 保存诊断结果 | Save diagnostic results
diagnostic_results <- tibble(
    Test = c(
        "Hausman Test (FE vs RE)",
        "Serial Correlation (Breusch-Godfrey)",
        "Heteroskedasticity (Breusch-Pagan)"
    ),
    Statistic = c(
        hausman_test$statistic,
        serial_test$statistic,
        bp_test$statistic
    ),
    P_Value = c(
        hausman_test$p.value,
        serial_test$p.value,
        bp_test$p.value
    ),
    Interpretation = c(
        ifelse(hausman_test$p.value < 0.05, "Use Fixed Effects", "Use Random Effects"),
        ifelse(serial_test$p.value < 0.05, "Serial correlation present", "No serial correlation"),
        ifelse(bp_test$p.value < 0.05, "Heteroskedasticity present", "Homoskedastic")
    )
)

write_csv(diagnostic_results, file.path("..", "results", "analysis", "03_diagnostic_tests.csv"))
cat("[OK] Diagnostic results saved\n")

# =============================================================================
# 7. 打印结果摘要 | Print Results Summary
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("  Core Analysis Results Summary\n")
cat("=============================================================================\n\n")

cat("[FE Model 1 - Baseline]\n")
cat(sprintf(
    "  GHE coefficient: %.4f (SE: %.4f)\n",
    coef(fe_model1)["ghe_gdp"],
    sqrt(diag(vcov(fe_model1)))["ghe_gdp"]
))
cat(sprintf("  R-squared: %.4f\n", summary(fe_model1)$r.squared[1]))
cat(sprintf("  Observations: %d\n\n", nobs(fe_model1)))

cat("[FE Model 2 - With Controls]\n")
cat(sprintf(
    "  GHE coefficient: %.4f (SE: %.4f)\n",
    coef(fe_model2)["ghe_gdp"],
    sqrt(diag(vcov(fe_model2)))["ghe_gdp"]
))
cat(sprintf("  R-squared: %.4f\n", summary(fe_model2)$r.squared[1]))
cat(sprintf("  Observations: %d\n\n", nobs(fe_model2)))

if (!is.null(iv_results$fiscal)) {
    cat("[IV Model - Fiscal Capacity]\n")
    cat(sprintf(
        "  GHE coefficient: %.4f (SE: %.4f)\n",
        coef(iv_results$fiscal$model)["ghe_gdp"],
        sqrt(diag(vcov(iv_results$fiscal$model)))["ghe_gdp"]
    ))
    cat(sprintf("  First-stage F: %.2f\n", iv_results$fiscal$first_stage_F))
    cat(sprintf("  Observations: %d\n\n", iv_results$fiscal$n_obs))
}

cat("[Diagnostic Tests]\n")
print(diagnostic_results, n = 10)
cat("\n")

# =============================================================================
# 8. 完成 | Complete
# =============================================================================

log_message("Core econometric analysis completed successfully")

cat("\n")
cat("=============================================================================\n")
cat("  [OK] Script 03 completed successfully!\n")
cat("=============================================================================\n")
cat("  - FE models estimated: 2\n")
cat(sprintf("  - IV model estimated: %s\n", ifelse(!is.null(iv_results), "Yes", "No")))
cat(sprintf("  - GMM model estimated: %s\n", ifelse(!is.null(gmm_results), "Yes", "No")))
cat("  - Results saved to: ../results/analysis/\n")
cat("  - Main table saved to: ../tables/Table_2_Main_Results.csv\n")
cat("\n")

log_message("Script 03 execution completed")
