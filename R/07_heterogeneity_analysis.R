# =============================================================================
# 06_heterogeneity_analysis.R
# Heterogeneity Analysis | 异质性分析
# =============================================================================
#
# 【功能概述 | Function Overview】
# 分析政府卫生支出效应的异质性
# Analyze heterogeneity in government health expenditure effects:
# - Income level heterogeneity | 收入水平异质性
# - Regional heterogeneity | 地区异质性
# - Temporal heterogeneity | 时间异质性
# - Policy regime heterogeneity | 政策区制异质性
#
# 【输入文件 | Input Files】
# - data/processed/02_integrated_panel_data.rds
# - results/analysis/04_threshold_results.rds
#
# 【输出文件 | Output Files】
# - results/analysis/06_heterogeneity_results.rds
# - tables/Table_3_Heterogeneity_Income.csv
# - tables/Table_S7_Heterogeneity_Region.csv
#
# 【运行时间 | Runtime】
# ~ 5-10 minutes | ~5-10分钟
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
cat("  06 - Heterogeneity Analysis\n")
cat("=============================================================================\n\n")

log_message("Starting heterogeneity analysis")

# =============================================================================
# 1. 数据加载与准备 | Data Loading and Preparation
# =============================================================================

cat("\n[STEP 1/5] Loading and preparing data...\n")

panel_data <- readRDS(file.path("..", "data", "processed", "02_integrated_panel_data.rds"))

# 准备异质性分析数据 | Prepare heterogeneity analysis data
hetero_data <- panel_data %>%
    filter(!is.na(life_exp), !is.na(ghe_gdp), !is.na(gdp_pc)) %>%
    mutate(
        # 创建收入分组 | Create income groups
        # 基于GDP per capita | Based on GDP per capita
        income_group = case_when(
            gdp_pc < quantile(gdp_pc, 0.33, na.rm = TRUE) ~ "Low Income",
            gdp_pc < quantile(gdp_pc, 0.67, na.rm = TRUE) ~ "Middle Income",
            TRUE ~ "High Income"
        ),
        income_group = factor(income_group,
            levels = c("Low Income", "Middle Income", "High Income")
        ),

        # 创建时期分组 | Create time periods
        period = case_when(
            year < 2008 ~ "Pre-Crisis (2000-2007)",
            year < 2015 ~ "Post-Crisis (2008-2014)",
            TRUE ~ "Recent (2015-2022)"
        ),
        period = factor(period),

        # 对数变换 | Log transformations
        log_gdp_pc = log(gdp_pc),
        log_population = log(population),

        # 创建时间趋势 | Create time trend
        time_trend = year - min(year) + 1
    ) %>%
    # 移除缺失值 | Remove missing values
    filter(!is.na(income_group), !is.na(period))

cat(sprintf("[OK] Heterogeneity data prepared: %d observations\n", nrow(hetero_data)))

# 打印分组统计 | Print group statistics
cat("\n[INFO] Sample distribution by income group:\n")
print(hetero_data %>%
    group_by(income_group) %>%
    summarise(
        n_obs = n(),
        n_countries = n_distinct(iso3c),
        avg_gdp_pc = mean(gdp_pc, na.rm = TRUE),
        avg_ghe = mean(ghe_gdp, na.rm = TRUE)
    ))

# =============================================================================
# 2. 收入水平异质性分析 | Income Level Heterogeneity
# =============================================================================

cat("\n[STEP 2/5] Analyzing income level heterogeneity...\n")

# 转换为pdata.frame | Convert to pdata.frame
pdata_hetero <- pdata.frame(hetero_data, index = c("iso3c", "year"))

# 分收入组估计模型 | Estimate models by income group
income_results <- list()

for (income in levels(hetero_data$income_group)) {
    cat(sprintf("[INFO] Estimating model for %s countries...\n", income))

    # 筛选数据 | Filter data
    data_subset <- hetero_data %>% filter(income_group == income)

    if (nrow(data_subset) < 50) {
        cat(sprintf("[WARN] Insufficient observations for %s, skipping...\n", income))
        next
    }

    # 转换为pdata.frame | Convert to pdata.frame
    pdata_subset <- pdata.frame(data_subset, index = c("iso3c", "year"))

    # 估计FE模型 | Estimate FE model
    fe_model <- plm(
        life_exp ~ ghe_gdp + log_gdp_pc + log_population + time_trend,
        data = pdata_subset,
        model = "within",
        effect = "twoways"
    )

    # 稳健标准误 | Robust standard errors
    fe_robust <- coeftest(fe_model, vcov = vcovHC(fe_model, type = "HC1"))

    income_results[[income]] <- list(
        model = fe_model,
        robust = fe_robust,
        n_obs = nrow(data_subset),
        n_countries = n_distinct(data_subset$iso3c)
    )

    cat(sprintf(
        "[OK] %s: GHE coef = %.4f (SE: %.4f)\n",
        income,
        coef(fe_model)["ghe_gdp"],
        sqrt(diag(vcovHC(fe_model, type = "HC1")))["ghe_gdp"]
    ))
}

# 交互项模型 | Interaction model
cat("[INFO] Estimating interaction model...\n")

interaction_model <- plm(
    life_exp ~ ghe_gdp * income_group + log_gdp_pc + log_population + time_trend,
    data = pdata_hetero,
    model = "within",
    effect = "twoways"
)

interaction_robust <- coeftest(interaction_model,
    vcov = vcovHC(interaction_model, type = "HC1")
)

cat("[OK] Interaction model estimated\n")

# =============================================================================
# 3. 地区异质性分析 | Regional Heterogeneity
# =============================================================================

cat("\n[STEP 3/5] Analyzing regional heterogeneity...\n")

# 添加地区信息 | Add regional information
# 使用World Bank的region变量 | Use World Bank region variable
if ("region" %in% names(hetero_data)) {
    regional_results <- list()

    # 获取主要地区 | Get major regions
    major_regions <- hetero_data %>%
        group_by(region) %>%
        summarise(n = n()) %>%
        filter(n >= 100) %>%
        pull(region)

    for (reg in major_regions) {
        cat(sprintf("[INFO] Estimating model for %s...\n", reg))

        # 筛选数据 | Filter data
        data_region <- hetero_data %>% filter(region == reg)

        if (nrow(data_region) < 50) next

        # 转换为pdata.frame | Convert to pdata.frame
        pdata_region <- pdata.frame(data_region, index = c("iso3c", "year"))

        # 估计FE模型 | Estimate FE model
        fe_region <- plm(
            life_exp ~ ghe_gdp + log_gdp_pc + log_population + time_trend,
            data = pdata_region,
            model = "within",
            effect = "twoways"
        )

        regional_results[[reg]] <- list(
            model = fe_region,
            robust = coeftest(fe_region, vcov = vcovHC(fe_region, type = "HC1")),
            n_obs = nrow(data_region),
            n_countries = n_distinct(data_region$iso3c)
        )

        cat(sprintf(
            "[OK] %s: GHE coef = %.4f\n",
            reg, coef(fe_region)["ghe_gdp"]
        ))
    }
} else {
    cat("[WARN] Regional variable not available, skipping regional analysis\n")
    regional_results <- NULL
}

# =============================================================================
# 4. 时间异质性分析 | Temporal Heterogeneity
# =============================================================================

cat("\n[STEP 4/5] Analyzing temporal heterogeneity...\n")

temporal_results <- list()

for (prd in levels(hetero_data$period)) {
    cat(sprintf("[INFO] Estimating model for %s...\n", prd))

    # 筛选数据 | Filter data
    data_period <- hetero_data %>% filter(period == prd)

    if (nrow(data_period) < 100) {
        cat(sprintf("[WARN] Insufficient observations for %s, skipping...\n", prd))
        next
    }

    # 转换为pdata.frame | Convert to pdata.frame
    pdata_period <- pdata.frame(data_period, index = c("iso3c", "year"))

    # 估计FE模型 | Estimate FE model
    fe_period <- plm(
        life_exp ~ ghe_gdp + log_gdp_pc + log_population + time_trend,
        data = pdata_period,
        model = "within",
        effect = "twoways"
    )

    temporal_results[[prd]] <- list(
        model = fe_period,
        robust = coeftest(fe_period, vcov = vcovHC(fe_period, type = "HC1")),
        n_obs = nrow(data_period),
        n_years = n_distinct(data_period$year)
    )

    cat(sprintf(
        "[OK] %s: GHE coef = %.4f\n",
        prd, coef(fe_period)["ghe_gdp"]
    ))
}

# =============================================================================
# 5. 创建结果表格 | Create Results Tables
# =============================================================================

cat("\n[STEP 5/5] Creating results tables...\n")

# 提取系数函数 | Function to extract coefficients
extract_hetero_results <- function(model_list, var_name = "ghe_gdp") {
    map_dfr(names(model_list), function(group_name) {
        model <- model_list[[group_name]]$model
        robust <- model_list[[group_name]]$robust

        coef_val <- coef(model)[var_name]
        se_val <- robust[var_name, "Std. Error"]
        t_val <- robust[var_name, "t value"]
        p_val <- robust[var_name, "Pr(>|t|)"]

        tibble(
            Group = group_name,
            Coefficient = coef_val,
            Std_Error = se_val,
            T_Statistic = t_val,
            P_Value = p_val,
            Significance = case_when(
                p_val < 0.01 ~ "***",
                p_val < 0.05 ~ "**",
                p_val < 0.1 ~ "*",
                TRUE ~ ""
            ),
            N_Obs = model_list[[group_name]]$n_obs
        )
    })
}

# 收入异质性表 | Income heterogeneity table
income_table <- extract_hetero_results(income_results)
write_csv(
    income_table,
    file.path("..", "tables", "Table_3_Heterogeneity_Income.csv")
)
cat("[OK] Income heterogeneity table saved\n")

# 地区异质性表 | Regional heterogeneity table
if (!is.null(regional_results) && length(regional_results) > 0) {
    regional_table <- extract_hetero_results(regional_results)
    write_csv(
        regional_table,
        file.path("..", "tables", "Table_S7_Heterogeneity_Region.csv")
    )
    cat("[OK] Regional heterogeneity table saved\n")
}

# 时间异质性表 | Temporal heterogeneity table
temporal_table <- extract_hetero_results(temporal_results)
write_csv(
    temporal_table,
    file.path("..", "tables", "Table_S8_Heterogeneity_Temporal.csv")
)
cat("[OK] Temporal heterogeneity table saved\n")

# =============================================================================
# 6. 保存完整结果 | Save Complete Results
# =============================================================================

cat("\n[INFO] Saving complete heterogeneity results...\n")

heterogeneity_results_full <- list(
    income_results = income_results,
    regional_results = regional_results,
    temporal_results = temporal_results,
    interaction_model = interaction_model,
    tables = list(
        income = income_table,
        regional = if (!is.null(regional_results)) regional_table else NULL,
        temporal = temporal_table
    ),
    data_summary = list(
        total_obs = nrow(hetero_data),
        income_groups = table(hetero_data$income_group),
        time_periods = table(hetero_data$period)
    )
)

saveRDS(
    heterogeneity_results_full,
    file.path("..", "results", "analysis", "06_heterogeneity_results.rds")
)
cat("[OK] Complete results saved\n")

# =============================================================================
# 7. 打印结果摘要 | Print Results Summary
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("  Heterogeneity Analysis Summary\n")
cat("=============================================================================\n\n")

cat("[Income Level Heterogeneity]\n")
if (!is.null(income_table) && nrow(income_table) > 0) {
    print(as.data.frame(income_table[, c("Group", "Coefficient", "Std_Error", "Significance")]), row.names = FALSE)
}
cat("\n")

cat("[Temporal Heterogeneity]\n")
if (!is.null(temporal_table) && nrow(temporal_table) > 0) {
    print(as.data.frame(temporal_table[, c("Group", "Coefficient", "Std_Error", "Significance")]), row.names = FALSE)
}
cat("\n")

if (!is.null(regional_results) && length(regional_results) > 0) {
    cat("[Regional Heterogeneity]\n")
    print(head(regional_table[, c("Group", "Coefficient", "Std_Error", "Significance")], 10))
    cat("\n")
}

# =============================================================================
# 8. 完成 | Complete
# =============================================================================

log_message("Heterogeneity analysis completed successfully")

cat("\n")
cat("=============================================================================\n")
cat("  [OK] Script 06 completed successfully!\n")
cat("=============================================================================\n")
cat(sprintf("  - Income groups analyzed: %d\n", nrow(income_table)))
cat(sprintf("  - Time periods analyzed: %d\n", nrow(temporal_table)))
if (!is.null(regional_results)) {
    cat(sprintf("  - Regions analyzed: %d\n", length(regional_results)))
}
cat("  - Results saved to: ../results/analysis/\n")
cat("  - Tables saved to: ../tables/\n")
cat("\n")

log_message("Script 06 execution completed")
