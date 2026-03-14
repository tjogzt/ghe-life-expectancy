# =============================================================================
# 07_robustness_checks.R
# Robustness Checks | 稳健性检验
# =============================================================================
#
# 【功能概述 | Function Overview】
# 对核心结果进行全面的稳健性检验
# Comprehensive robustness checks for core results:
# - Alternative specifications | 替代模型设定
# - Sample restrictions | 样本限制
# - Outlier sensitivity | 异常值敏感性
# - Alternative dependent variables | 替代因变量
# - Lag structure sensitivity | 滞后结构敏感性
#
# 【输入文件 | Input Files】
# - data/processed/02_integrated_panel_data.rds
# - results/analysis/03_fe_results.rds
#
# 【输出文件 | Output Files】
# - results/analysis/07_robustness_results.rds
# - tables/Table_S9_Robustness_Checks.csv
#
# 【运行时间 | Runtime】
# ~ 5-8 minutes | ~5-8分钟
#
# 【最后更新 | Last Updated】
# 2025-11-05
# =============================================================================

# 运行配置脚本 | Run configuration script
if (!exists("CONFIG")) {
    source("01_setup_and_configuration.R")
}

cat("\n")
cat("=============================================================================\n")
cat("  07 - Robustness Checks\n")
cat("=============================================================================\n\n")

log_message("Starting robustness checks")

# =============================================================================
# 1. 数据加载与准备 | Data Loading and Preparation
# =============================================================================

cat("\n[STEP 1/6] Loading data...\n")

panel_data <- readRDS(file.path("..", "data", "processed", "02_integrated_panel_data.rds"))
baseline_results <- readRDS(file.path("..", "results", "analysis", "03_fe_results.rds"))

# 准备稳健性检验数据 | Prepare robustness check data
robust_data <- panel_data %>%
    filter(!is.na(life_exp), !is.na(ghe_gdp), !is.na(gdp_pc)) %>%
    mutate(
        log_gdp_pc = log(gdp_pc),
        log_population = log(population),
        time_trend = year - min(year) + 1
    ) %>%
    arrange(iso3c, year)

cat(sprintf("[OK] Robustness data prepared: %d observations\n", nrow(robust_data)))

# 转换为pdata.frame | Convert to pdata.frame
pdata_robust <- pdata.frame(robust_data, index = c("iso3c", "year"))

# =============================================================================
# 2. 替代模型设定 | Alternative Specifications
# =============================================================================

cat("\n[STEP 2/6] Testing alternative specifications...\n")

# 2.1 不同固定效应组合 | Different fixed effects combinations
cat("[INFO] Testing fixed effects combinations...\n")

# 仅国家固定效应 | Country FE only
fe_country_only <- plm(
    life_exp ~ ghe_gdp + log_gdp_pc + log_population + time_trend,
    data = pdata_robust,
    model = "within",
    effect = "individual"
)

# 仅时间固定效应 | Time FE only
fe_time_only <- plm(
    life_exp ~ ghe_gdp + log_gdp_pc + log_population,
    data = pdata_robust,
    model = "within",
    effect = "time"
)

# 双向固定效应（基准） | Two-way FE (baseline)
fe_twoway <- plm(
    life_exp ~ ghe_gdp + log_gdp_pc + log_population + time_trend,
    data = pdata_robust,
    model = "within",
    effect = "twoways"
)

cat("[OK] Fixed effects combinations estimated\n")

# 2.2 非线性设定 | Nonlinear specifications
cat("[INFO] Testing nonlinear specifications...\n")

# 二次项 | Quadratic term
fe_quadratic <- plm(
    life_exp ~ ghe_gdp + I(ghe_gdp^2) + log_gdp_pc + log_population + time_trend,
    data = pdata_robust,
    model = "within",
    effect = "twoways"
)

# 对数变换 | Log transformation
robust_data_log <- robust_data %>%
    mutate(log_ghe_gdp = log(ghe_gdp + 0.01)) # 避免log(0) | Avoid log(0)

pdata_log <- pdata.frame(robust_data_log, index = c("iso3c", "year"))

fe_log <- plm(
    life_exp ~ log_ghe_gdp + log_gdp_pc + log_population + time_trend,
    data = pdata_log,
    model = "within",
    effect = "twoways"
)

cat("[OK] Nonlinear specifications estimated\n")

# =============================================================================
# 3. 样本限制检验 | Sample Restriction Tests
# =============================================================================

cat("\n[STEP 3/6] Testing sample restrictions...\n")

# 3.1 排除小国 | Exclude small countries
cat("[INFO] Excluding small countries (population < 1 million)...\n")

data_large_only <- robust_data %>%
    filter(population >= 1000000)

pdata_large <- pdata.frame(data_large_only, index = c("iso3c", "year"))

fe_large_countries <- plm(
    life_exp ~ ghe_gdp + log_gdp_pc + log_population + time_trend,
    data = pdata_large,
    model = "within",
    effect = "twoways"
)

cat(sprintf("[OK] Large countries only: %d obs\n", nrow(data_large_only)))

# 3.2 平衡面板 | Balanced panel
cat("[INFO] Using balanced panel only...\n")

# 识别有完整时间序列的国家 | Identify countries with complete time series
complete_countries <- robust_data %>%
    group_by(iso3c) %>%
    summarise(n_years = n()) %>%
    filter(n_years == max(n_years)) %>%
    pull(iso3c)

data_balanced <- robust_data %>%
    filter(iso3c %in% complete_countries)

pdata_balanced <- pdata.frame(data_balanced, index = c("iso3c", "year"))

fe_balanced <- plm(
    life_exp ~ ghe_gdp + log_gdp_pc + log_population + time_trend,
    data = pdata_balanced,
    model = "within",
    effect = "twoways"
)

cat(sprintf(
    "[OK] Balanced panel: %d countries, %d obs\n",
    length(complete_countries), nrow(data_balanced)
))

# 3.3 排除异常值 | Exclude outliers
cat("[INFO] Excluding outliers...\n")

# 使用已标记的异常值 | Use pre-flagged outliers
data_no_outliers <- robust_data %>%
    filter(!outlier_che, !outlier_ghe, !outlier_gdp)

pdata_no_outliers <- pdata.frame(data_no_outliers, index = c("iso3c", "year"))

fe_no_outliers <- plm(
    life_exp ~ ghe_gdp + log_gdp_pc + log_population + time_trend,
    data = pdata_no_outliers,
    model = "within",
    effect = "twoways"
)

cat(sprintf(
    "[OK] No outliers: %d obs (removed %d)\n",
    nrow(data_no_outliers),
    nrow(robust_data) - nrow(data_no_outliers)
))

# =============================================================================
# 4. 替代因变量 | Alternative Dependent Variables
# =============================================================================

cat("\n[STEP 4/6] Testing alternative dependent variables...\n")

# 4.1 婴儿死亡率 | Infant mortality rate
if ("infant_mort" %in% names(robust_data) && sum(!is.na(robust_data$infant_mort)) > 100) {
    cat("[INFO] Using infant mortality as dependent variable...\n")

    fe_infant_mort <- plm(
        infant_mort ~ ghe_gdp + log_gdp_pc + log_population + time_trend,
        data = pdata_robust,
        model = "within",
        effect = "twoways"
    )

    cat("[OK] Infant mortality model estimated\n")
} else {
    fe_infant_mort <- NULL
    cat("[SKIP] Infant mortality data not available\n")
}

# =============================================================================
# 5. 滞后结构敏感性 | Lag Structure Sensitivity
# =============================================================================

cat("\n[STEP 5/6] Testing lag structure sensitivity...\n")

# 创建不同滞后变量 | Create different lag variables
lag_data <- robust_data %>%
    group_by(iso3c) %>%
    arrange(year) %>%
    mutate(
        ghe_gdp_lag1 = lag(ghe_gdp, 1),
        ghe_gdp_lag2 = lag(ghe_gdp, 2),
        life_exp_lag1 = lag(life_exp, 1)
    ) %>%
    ungroup()

# 5.1 当期和滞后1期 | Contemporary and 1-period lag
data_lag1 <- lag_data %>% filter(!is.na(ghe_gdp_lag1))
pdata_lag1 <- pdata.frame(data_lag1, index = c("iso3c", "year"))

fe_with_lag1 <- plm(
    life_exp ~ ghe_gdp + ghe_gdp_lag1 + log_gdp_pc + log_population + time_trend,
    data = pdata_lag1,
    model = "within",
    effect = "twoways"
)

cat("[OK] Model with 1-period lag estimated\n")

# 5.2 动态面板（包含因变量滞后项）| Dynamic panel with lagged DV
data_dynamic <- lag_data %>% filter(!is.na(life_exp_lag1))
pdata_dynamic <- pdata.frame(data_dynamic, index = c("iso3c", "year"))

fe_dynamic <- plm(
    life_exp ~ life_exp_lag1 + ghe_gdp + log_gdp_pc + log_population + time_trend,
    data = pdata_dynamic,
    model = "within",
    effect = "twoways"
)

cat("[OK] Dynamic panel model estimated\n")

# =============================================================================
# 6. 汇总稳健性检验结果 | Compile Robustness Check Results
# =============================================================================

cat("\n[STEP 6/6] Compiling robustness check results...\n")

# 提取GHE系数和标准误 | Extract GHE coefficients and standard errors
extract_robust_coef <- function(model, model_name, var_name = "ghe_gdp") {
    if (is.null(model)) {
        return(tibble(
            Specification = model_name,
            Coefficient = NA,
            Std_Error = NA,
            T_Statistic = NA,
            P_Value = NA,
            N_Obs = NA
        ))
    }

    tryCatch(
        {
            robust_se <- coeftest(model, vcov = vcovHC(model, type = "HC1"))

            tibble(
                Specification = model_name,
                Coefficient = coef(model)[var_name],
                Std_Error = robust_se[var_name, "Std. Error"],
                T_Statistic = robust_se[var_name, "t value"],
                P_Value = robust_se[var_name, "Pr(>|t|)"],
                N_Obs = nobs(model)
            )
        },
        error = function(e) {
            tibble(
                Specification = model_name,
                Coefficient = NA,
                Std_Error = NA,
                T_Statistic = NA,
                P_Value = NA,
                N_Obs = if (!is.null(model)) nobs(model) else NA
            )
        }
    )
}

# 创建稳健性检验汇总表 | Create robustness summary table
robustness_table <- bind_rows(
    # 基准模型 | Baseline
    extract_robust_coef(fe_twoway, "Baseline (Two-way FE)"),

    # 固定效应组合 | FE combinations
    extract_robust_coef(fe_country_only, "Country FE only"),
    extract_robust_coef(fe_time_only, "Time FE only"),

    # 非线性设定 | Nonlinear specifications
    extract_robust_coef(fe_quadratic, "Quadratic term"),
    extract_robust_coef(fe_log, "Log specification", "log_ghe_gdp"),

    # 样本限制 | Sample restrictions
    extract_robust_coef(fe_large_countries, "Large countries only"),
    extract_robust_coef(fe_balanced, "Balanced panel"),
    extract_robust_coef(fe_no_outliers, "Outliers excluded"),

    # 替代因变量 | Alternative DV
    extract_robust_coef(fe_infant_mort, "DV: Infant mortality"),

    # 滞后结构 | Lag structure
    extract_robust_coef(fe_with_lag1, "With 1-period lag"),
    extract_robust_coef(fe_dynamic, "Dynamic panel")
) %>%
    mutate(
        Significance = case_when(
            is.na(P_Value) ~ "",
            P_Value < 0.01 ~ "***",
            P_Value < 0.05 ~ "**",
            P_Value < 0.1 ~ "*",
            TRUE ~ ""
        )
    )

# 保存稳健性检验表 | Save robustness table
write_csv(
    robustness_table,
    file.path("..", "tables", "Table_S9_Robustness_Checks.csv")
)
cat("[OK] Robustness checks table saved\n")

# 保存完整结果 | Save complete results
robustness_results_full <- list(
    models = list(
        fe_country_only = fe_country_only,
        fe_time_only = fe_time_only,
        fe_twoway = fe_twoway,
        fe_quadratic = fe_quadratic,
        fe_log = fe_log,
        fe_large_countries = fe_large_countries,
        fe_balanced = fe_balanced,
        fe_no_outliers = fe_no_outliers,
        fe_infant_mort = fe_infant_mort,
        fe_with_lag1 = fe_with_lag1,
        fe_dynamic = fe_dynamic
    ),
    summary_table = robustness_table,
    sample_info = list(
        full_sample = nrow(robust_data),
        large_countries = nrow(data_large_only),
        balanced_panel = nrow(data_balanced),
        no_outliers = nrow(data_no_outliers)
    )
)

saveRDS(
    robustness_results_full,
    file.path("..", "results", "analysis", "07_robustness_results.rds")
)
cat("[OK] Complete robustness results saved\n")

# =============================================================================
# 7. 打印结果摘要 | Print Results Summary
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("  Robustness Checks Summary\n")
cat("=============================================================================\n\n")

cat("[Robustness Check Results]\n")
print(robustness_table[, c("Specification", "Coefficient", "Std_Error", "Significance", "N_Obs")],
    n = 20
)
cat("\n")

# 计算系数范围 | Calculate coefficient range
valid_coefs <- robustness_table$Coefficient[!is.na(robustness_table$Coefficient)]
if (length(valid_coefs) > 0) {
    cat(sprintf(
        "[Coefficient Range]: %.4f to %.4f\n",
        min(valid_coefs), max(valid_coefs)
    ))
    cat(sprintf("[Median Coefficient]: %.4f\n", median(valid_coefs)))
    cat(sprintf(
        "[Number of significant results (p<0.05)]: %d / %d\n\n",
        sum(robustness_table$P_Value < 0.05, na.rm = TRUE),
        sum(!is.na(robustness_table$P_Value))
    ))
}

# =============================================================================
# 8. 完成 | Complete
# =============================================================================

log_message("Robustness checks completed successfully")

cat("\n")
cat("=============================================================================\n")
cat("  [OK] Script 07 completed successfully!\n")
cat("=============================================================================\n")
cat(sprintf("  - Total specifications tested: %d\n", nrow(robustness_table)))
cat(sprintf("  - Results saved to: ../results/analysis/\n"))
cat(sprintf("  - Table saved to: ../tables/Table_S9_Robustness_Checks.csv\n"))
cat("\n")

log_message("Script 07 execution completed")
