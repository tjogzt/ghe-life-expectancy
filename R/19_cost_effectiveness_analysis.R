# =============================================================================
# 19_cost_effectiveness_analysis.R
# Cost-Effectiveness Analysis | 成本效益分析
# =============================================================================
#
# 【功能概述 | Function Overview】
# 分析GHE投资的成本效益比
# Analyze cost-effectiveness ratio of GHE investment:
# - Cost-effectiveness ratio (CER) | 成本效益比
# - Incremental cost-effectiveness ratio (ICER) | 增量成本效益比
# - Cost-effectiveness by institutional quality | 按制度质量分组的成本效益
#
# 【输入文件 | Input Files】
# - data/processed/02_integrated_panel_data.rds
# - results/analysis/03_fe_results.rds
#
# 【输出文件 | Output Files】
# - results/analysis/19_cost_effectiveness.rds
# - tables/Table_S23_Cost_Effectiveness_Ratio.csv
# - tables/Table_S24_Incremental_CER.csv
#
# 【运行时间 | Runtime】
# ~ 5-10 minutes | ~5-10分钟
#
# 【最后更新 | Last Updated】
# 2025-01-XX
# =============================================================================

# 运行配置脚本 | Run configuration script
if (!exists("CONFIG")) {
    source("01_setup_and_configuration.R")
}

cat("\n")
cat("=============================================================================\n")
cat("  19 - Cost-Effectiveness Analysis\n")
cat("=============================================================================\n\n")

log_message("Starting cost-effectiveness analysis")

# =============================================================================
# 0. 加载必要的包 | Load Required Packages
# =============================================================================

required_packages <- c("dplyr", "tidyr", "plm", "lmtest", "sandwich", 
                       "readr", "ggplot2")

for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
        install.packages(pkg)
        library(pkg, character.only = TRUE)
    }
}

cat("[OK] Required packages loaded\n")

# =============================================================================
# 1. 数据加载与准备 | Data Loading and Preparation
# =============================================================================

cat("\n[STEP 1/5] Loading and preparing data...\n")

panel_data <- readRDS(file.path("..", "data", "processed", "02_integrated_panel_data.rds"))

# 准备分析数据 | Prepare analysis data
analysis_data <- panel_data %>%
    filter(!is.na(life_exp), !is.na(ghe_gdp), !is.na(gdp_pc)) %>%
    mutate(
        log_gdp_pc = log(gdp_pc),
        log_population = log(population),
        time_trend = year - min(year) + 1
    ) %>%
    arrange(iso3c, year)

# 计算GHE的绝对成本（以GDP百分比表示，转换为美元）
# Calculate absolute GHE cost (as % of GDP, convert to USD)
# GHE成本 = GHE/GDP * GDP per capita * population
# GHE cost = GHE/GDP * GDP per capita * population
analysis_data <- analysis_data %>%
    mutate(
        ghe_cost_per_capita = (ghe_gdp / 100) * gdp_pc,  # 人均GHE成本（美元）| Per capita GHE cost (USD)
        ghe_cost_total = ghe_cost_per_capita * population  # 总GHE成本（美元）| Total GHE cost (USD)
    )

cat(sprintf("[OK] Analysis data prepared: %d observations\n", nrow(analysis_data)))

# =============================================================================
# 2. 估计GHE对预期寿命的边际效应 | Estimate Marginal Effect of GHE on Life Expectancy
# =============================================================================

cat("\n[STEP 2/5] Estimating marginal effect of GHE on life expectancy...\n")

# 使用固定效应模型估计边际效应 | Use fixed effects model to estimate marginal effect
pdata_ce <- pdata.frame(analysis_data, index = c("iso3c", "year"))

fe_ce <- plm(
    life_exp ~ ghe_gdp + log_gdp_pc + log_population + time_trend,
    data = pdata_ce,
    model = "within",
    effect = "twoways"
)

fe_ce_robust <- coeftest(fe_ce, vcov = vcovHC(fe_ce, type = "HC1"))

# 提取GHE的边际效应 | Extract marginal effect of GHE
marginal_effect <- coef(fe_ce)["ghe_gdp"]  # 每增加1% GDP的GHE，预期寿命增加的年数
# Years of life expectancy increase per 1% GDP increase in GHE

cat(sprintf("[OK] Marginal effect: %.4f years per 1%% GDP increase in GHE\n", marginal_effect))

# =============================================================================
# 3. 计算成本效益比（CER）| Calculate Cost-Effectiveness Ratio (CER)
# =============================================================================

cat("\n[STEP 3/5] Calculating cost-effectiveness ratios...\n")

# CER = 成本 / 健康收益
# CER = Cost / Health gain
# 成本：每增加1% GDP的GHE成本（以人均GDP的1%表示）
# Cost: Cost of increasing GHE by 1% of GDP (expressed as 1% of per capita GDP)
# 收益：预期寿命增加的年数
# Benefit: Years of life expectancy increase

# 人均GDP的1% | 1% of per capita GDP
gdp_1pct_per_capita <- analysis_data$gdp_pc / 100

# 成本效益比：每增加1年预期寿命的成本（以人均GDP的倍数表示）
# Cost-effectiveness ratio: Cost per year of life expectancy increase (as multiple of per capita GDP)
cer <- gdp_1pct_per_capita / marginal_effect

# 转换为美元 | Convert to USD
cer_usd <- cer  # 已经是美元 | Already in USD

analysis_data <- analysis_data %>%
    mutate(
        cer = cer_usd,
        cer_per_life_year = cer_usd  # 每增加1年预期寿命的成本 | Cost per year of life expectancy
    )

# 计算CER的统计量 | Calculate CER statistics
cer_summary <- analysis_data %>%
    filter(!is.na(cer), is.finite(cer), cer > 0) %>%
    summarise(
        mean_cer = mean(cer, na.rm = TRUE),
        median_cer = median(cer, na.rm = TRUE),
        sd_cer = sd(cer, na.rm = TRUE),
        q25_cer = quantile(cer, 0.25, na.rm = TRUE),
        q75_cer = quantile(cer, 0.75, na.rm = TRUE)
    )

cat(sprintf("[OK] Mean CER: $%.2f per life year\n", cer_summary$mean_cer))
cat(sprintf("[OK] Median CER: $%.2f per life year\n", cer_summary$median_cer))

# =============================================================================
# 4. 按制度质量分组的成本效益分析 | Cost-Effectiveness by Institutional Quality
# =============================================================================

cat("\n[STEP 4/5] Analyzing cost-effectiveness by institutional quality...\n")

ce_by_governance <- list()

# 检查制度质量变量 | Check institutional quality variable
if ("government_effectiveness" %in% names(analysis_data)) {
    analysis_data_ce <- analysis_data %>%
        filter(!is.na(government_effectiveness)) %>%
        mutate(
            inst_capacity = government_effectiveness,
            inst_group = ifelse(inst_capacity <= median(inst_capacity, na.rm = TRUE), 
                               "Low", "High")
        )
    
    # 按制度质量分组估计边际效应 | Estimate marginal effect by institutional quality
    for (group in c("Low", "High")) {
        data_subset <- analysis_data_ce %>% filter(inst_group == group)
        
        if (nrow(data_subset) > 50) {
            pdata_subset <- pdata.frame(data_subset, index = c("iso3c", "year"))
            
            fe_subset <- plm(
                life_exp ~ ghe_gdp + log_gdp_pc + log_population + time_trend,
                data = pdata_subset,
                model = "within",
                effect = "twoways"
            )
            
            marginal_effect_group <- coef(fe_subset)["ghe_gdp"]
            
            # 计算该组的CER | Calculate CER for this group
            gdp_1pct_group <- data_subset$gdp_pc / 100
            cer_group <- gdp_1pct_group / marginal_effect_group
            
            ce_by_governance[[group]] <- list(
                marginal_effect = marginal_effect_group,
                mean_cer = mean(cer_group[is.finite(cer_group) & cer_group > 0], na.rm = TRUE),
                median_cer = median(cer_group[is.finite(cer_group) & cer_group > 0], na.rm = TRUE),
                n_obs = nrow(data_subset),
                n_countries = n_distinct(data_subset$iso3c)
            )
            
            cat(sprintf("[OK] %s governance: Marginal effect = %.4f, Mean CER = $%.2f\n",
                        group, marginal_effect_group, ce_by_governance[[group]]$mean_cer))
        }
    }
} else {
    cat("[WARN] Institutional quality variable not available\n")
}

# =============================================================================
# 5. 增量成本效益比（ICER）| Incremental Cost-Effectiveness Ratio (ICER)
# =============================================================================

cat("\n[STEP 5/5] Calculating incremental cost-effectiveness ratio...\n")

# ICER = (成本差异) / (健康收益差异)
# ICER = (Cost difference) / (Health gain difference)
# 比较不同GHE水平的成本效益 | Compare cost-effectiveness at different GHE levels

# 定义GHE水平 | Define GHE levels
ghe_quantiles <- quantile(analysis_data$ghe_gdp, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

icer_results <- list()

# 比较低GHE vs 高GHE | Compare low GHE vs high GHE
low_ghe_threshold <- ghe_quantiles[1]
high_ghe_threshold <- ghe_quantiles[3]

low_ghe_data <- analysis_data %>% filter(ghe_gdp <= low_ghe_threshold)
high_ghe_data <- analysis_data %>% filter(ghe_gdp >= high_ghe_threshold)

if (nrow(low_ghe_data) > 50 && nrow(high_ghe_data) > 50) {
    # 计算平均成本和收益 | Calculate average cost and benefit
    avg_cost_low <- mean(low_ghe_data$ghe_cost_per_capita, na.rm = TRUE)
    avg_cost_high <- mean(high_ghe_data$ghe_cost_per_capita, na.rm = TRUE)
    avg_life_exp_low <- mean(low_ghe_data$life_exp, na.rm = TRUE)
    avg_life_exp_high <- mean(high_ghe_data$life_exp, na.rm = TRUE)
    
    # ICER | ICER
    cost_diff <- avg_cost_high - avg_cost_low
    benefit_diff <- avg_life_exp_high - avg_life_exp_low
    
    if (benefit_diff > 0) {
        icer <- cost_diff / benefit_diff
        
        icer_results$low_vs_high <- list(
            cost_difference = cost_diff,
            benefit_difference = benefit_diff,
            icer = icer,
            n_low = nrow(low_ghe_data),
            n_high = nrow(high_ghe_data)
        )
        
        cat(sprintf("[OK] ICER (Low vs High GHE): $%.2f per life year\n", icer))
    }
}

# =============================================================================
# 6. 保存结果 | Save Results
# =============================================================================

cat("\n[STEP 6/6] Saving results...\n")

all_results <- list(
    marginal_effect = marginal_effect,
    cer_summary = cer_summary,
    ce_by_governance = ce_by_governance,
    icer = icer_results,
    data_summary = list(
        total_obs = nrow(analysis_data),
        n_countries = n_distinct(analysis_data$iso3c),
        year_range = range(analysis_data$year, na.rm = TRUE)
    )
)

saveRDS(all_results, file.path("..", "results", "analysis", "19_cost_effectiveness.rds"))
cat("[OK] Results saved to RDS file\n")

# 生成汇总表格 | Generate summary tables
# 成本效益比表格 | Cost-effectiveness ratio table
cer_table <- data.frame(
    Statistic = c("Mean", "Median", "SD", "Q25", "Q75"),
    CER_USD = c(
        cer_summary$mean_cer,
        cer_summary$median_cer,
        cer_summary$sd_cer,
        cer_summary$q25_cer,
        cer_summary$q75_cer
    ),
    stringsAsFactors = FALSE
)

write_csv(cer_table, file.path("..", "tables", "Table_S23_Cost_Effectiveness_Ratio.csv"))
cat("[OK] Cost-effectiveness ratio table saved\n")

# 按制度质量分组的CER表格 | CER by institutional quality table
if (length(ce_by_governance) > 0) {
    ce_governance_table <- data.frame(
        Governance_Level = character(),
        Marginal_Effect = numeric(),
        Mean_CER = numeric(),
        Median_CER = numeric(),
        N_obs = integer(),
        N_countries = integer(),
        stringsAsFactors = FALSE
    )
    
    for (group in names(ce_by_governance)) {
        ce_governance_table <- rbind(ce_governance_table, data.frame(
            Governance_Level = group,
            Marginal_Effect = ce_by_governance[[group]]$marginal_effect,
            Mean_CER = ce_by_governance[[group]]$mean_cer,
            Median_CER = ce_by_governance[[group]]$median_cer,
            N_obs = ce_by_governance[[group]]$n_obs,
            N_countries = ce_by_governance[[group]]$n_countries
        ))
    }
    
    write_csv(ce_governance_table, file.path("..", "tables", "Table_S24_CER_by_Governance.csv"))
    cat("[OK] CER by governance table saved\n")
}

# ICER表格 | ICER table
if (length(icer_results) > 0) {
    icer_table <- data.frame(
        Comparison = character(),
        Cost_Difference = numeric(),
        Benefit_Difference = numeric(),
        ICER = numeric(),
        N_Low = integer(),
        N_High = integer(),
        stringsAsFactors = FALSE
    )
    
    for (comp in names(icer_results)) {
        icer_table <- rbind(icer_table, data.frame(
            Comparison = comp,
            Cost_Difference = icer_results[[comp]]$cost_difference,
            Benefit_Difference = icer_results[[comp]]$benefit_difference,
            ICER = icer_results[[comp]]$icer,
            N_Low = icer_results[[comp]]$n_low,
            N_High = icer_results[[comp]]$n_high
        ))
    }
    
    write_csv(icer_table, file.path("..", "tables", "Table_S25_Incremental_CER.csv"))
    cat("[OK] Incremental CER table saved\n")
}

cat("\n=============================================================================\n")
cat("  Cost-Effectiveness Analysis Complete\n")
cat("=============================================================================\n\n")

log_message("Cost-effectiveness analysis completed successfully")

