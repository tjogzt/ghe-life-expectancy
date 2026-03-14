# =============================================================================
# 20_environmental_health_interaction.R
# Environmental Health Interaction Analysis | 环境健康交互作用分析
# =============================================================================
#
# 【功能概述 | Function Overview】
# 分析环境污染如何调节GHE对健康的影响
# Analyze how environmental pollution moderates GHE effects on health:
# - GHE × PM2.5 interaction | GHE与PM2.5的交互效应
# - GHE × Water quality interaction | GHE与水质的交互效应
# - GHE effects by environmental quality | 按环境质量分组的GHE效应
#
# 【输入文件 | Input Files】
# - data/processed/02_integrated_panel_data.rds
#
# 【输出文件 | Output Files】
# - results/analysis/20_environmental_interaction.rds
# - tables/Table_S26_Environmental_Interaction.csv
#
# 【运行时间 | Runtime】
# ~ 10-15 minutes | ~10-15分钟
#
# 【注意事项 | Notes】
# - 需要从WDI下载环境数据 | Requires downloading environmental data from WDI
# - 环境数据可能不完整 | Environmental data may be incomplete
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
cat("  20 - Environmental Health Interaction Analysis\n")
cat("=============================================================================\n\n")

log_message("Starting environmental health interaction analysis")

# =============================================================================
# 0. 加载必要的包 | Load Required Packages
# =============================================================================

required_packages <- c(
    "dplyr", "tidyr", "plm", "lmtest", "sandwich",
    "readr", "WDI"
)

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

# 准备基础分析数据 | Prepare base analysis data
analysis_data <- panel_data %>%
    filter(!is.na(life_exp), !is.na(ghe_gdp), !is.na(gdp_pc)) %>%
    mutate(
        log_gdp_pc = log(gdp_pc),
        log_population = log(population),
        time_trend = year - min(year) + 1
    ) %>%
    arrange(iso3c, year)

cat(sprintf("[OK] Base analysis data prepared: %d observations\n", nrow(analysis_data)))

# =============================================================================
# 2. 下载环境数据 | Download Environmental Data
# =============================================================================

cat("\n[STEP 2/5] Downloading environmental data...\n")

# 检查环境数据文件是否存在 | Check if environmental data file exists
env_data_file <- file.path("..", "data", "raw", "environmental_indicators.csv")

# 初始化env_data为NULL | Initialize env_data as NULL
env_data <- NULL

if (!file.exists(env_data_file)) {
    cat("[INFO] Downloading environmental indicators from WDI...\n")

    tryCatch(
        {
            # WDI环境指标代码 | WDI environmental indicator codes
            # 说明：由于部分指标（如EN.ATM.CO2E.PC）在当前环境下下载不稳定，先使用核心指标\n            # Note: Some indicators (e.g., EN.ATM.CO2E.PC) are unstable to download in this environment; use core indicators first
            env_indicators <- c(
                "EN.ATM.PM25.MC.M3", # PM2.5空气污染（微克/立方米）| PM2.5 air pollution (μg/m³)
                "SH.H2O.SMDW.ZS", # 使用改善的饮用水源的人口比例 | Population using improved drinking water sources (%)
                "SH.STA.SMSS.ZS", # 使用改善的卫生设施的人口比例 | Population using improved sanitation facilities (%)
                "AG.LND.FRST.ZS" # 森林面积占比 | Forest area (% of land area)
            )

            env_data <- WDI(
                indicator = env_indicators,
                start = 2000,
                end = 2022,
                extra = TRUE
            ) %>%
                as_tibble() %>%
                filter(region != "Aggregates") %>%
                dplyr::select(
                    iso3c, country, year, region, income,
                    pm25 = EN.ATM.PM25.MC.M3,
                    improved_water = SH.H2O.SMDW.ZS,
                    improved_sanitation = SH.STA.SMSS.ZS,
                    forest_area = AG.LND.FRST.ZS
                ) %>%
                arrange(iso3c, year)

            write_csv(env_data, env_data_file)
            cat(sprintf("  ✓ Downloaded environmental data: %d observations\n", nrow(env_data)))
            env_data <<- env_data # 使用<<-确保赋值到外层作用域 | Use <<- to assign to outer scope
        },
        error = function(e) {
            cat(sprintf("  ✗ Failed to download environmental data: %s\n", e$message))
            env_data <<- NULL # 使用<<-确保赋值到外层作用域 | Use <<- to assign to outer scope
        }
    )
} else {
    cat("[OK] Environmental data file already exists\n")
    tryCatch(
        {
            env_data <- read_csv(env_data_file, show_col_types = FALSE)
        },
        error = function(e) {
            cat(sprintf("  ✗ Failed to read environmental data: %s\n", e$message))
            env_data <<- NULL # 使用<<-确保赋值到外层作用域 | Use <<- to assign to outer scope
        }
    )
}

# =============================================================================
# 3. 合并环境数据 | Merge Environmental Data
# =============================================================================

cat("\n[STEP 3/5] Merging environmental data...\n")

if (!is.null(env_data)) {
    analysis_data <- analysis_data %>%
        left_join(
            env_data %>% dplyr::select(
                iso3c, year, pm25, improved_water,
                improved_sanitation, forest_area
            ),
            by = c("iso3c", "year")
        )
    cat("[OK] Environmental data merged\n")
} else {
    cat("[WARN] Environmental data not available, creating placeholder variables\n")
    analysis_data <- analysis_data %>%
        mutate(
            pm25 = NA_real_,
            improved_water = NA_real_,
            improved_sanitation = NA_real_,
            forest_area = NA_real_
        )
}

# =============================================================================
# 4. GHE × 环境质量交互效应分析 | GHE × Environmental Quality Interaction Analysis
# =============================================================================

cat("\n[STEP 4/5] Analyzing GHE × environmental quality interactions...\n")

env_interaction_results <- list()

# 4.1 PM2.5交互效应 | PM2.5 interaction
if (sum(!is.na(analysis_data$pm25)) > 100) {
    cat("[INFO] Analyzing GHE × PM2.5 interaction...\n")

    analysis_data_pm25 <- analysis_data %>%
        filter(!is.na(pm25)) %>%
        mutate(
            pm25_centered = pm25 - mean(pm25, na.rm = TRUE),
            ghe_pm25_interaction = ghe_gdp * pm25_centered
        )

    pdata_pm25 <- pdata.frame(analysis_data_pm25, index = c("iso3c", "year"))

    fe_pm25_interaction <- plm(
        life_exp ~ ghe_gdp + pm25_centered + ghe_pm25_interaction +
            log_gdp_pc + log_population + time_trend,
        data = pdata_pm25,
        model = "within",
        effect = "twoways"
    )

    fe_pm25_robust <- coeftest(fe_pm25_interaction, vcov = vcovHC(fe_pm25_interaction, type = "HC1"))

    env_interaction_results$pm25 <- list(
        model = fe_pm25_interaction,
        robust = fe_pm25_robust,
        n_obs = nrow(analysis_data_pm25),
        n_countries = n_distinct(analysis_data_pm25$iso3c)
    )

    cat(sprintf(
        "[OK] PM2.5 interaction: GHE coef = %.4f, Interaction coef = %.4f\n",
        coef(fe_pm25_interaction)["ghe_gdp"],
        coef(fe_pm25_interaction)["ghe_pm25_interaction"]
    ))
}

# 4.2 按PM2.5水平分组 | By PM2.5 level
if (sum(!is.na(analysis_data$pm25)) > 100) {
    cat("[INFO] Analyzing GHE effects by PM2.5 level...\n")

    analysis_data_pm25_group <- analysis_data %>%
        filter(!is.na(pm25)) %>%
        mutate(
            pm25_group = case_when(
                pm25 < quantile(pm25, 0.33, na.rm = TRUE) ~ "Low PM2.5",
                pm25 < quantile(pm25, 0.67, na.rm = TRUE) ~ "Medium PM2.5",
                TRUE ~ "High PM2.5"
            ),
            pm25_group = factor(pm25_group, levels = c("Low PM2.5", "Medium PM2.5", "High PM2.5"))
        )

    pm25_group_results <- list()

    for (group in levels(analysis_data_pm25_group$pm25_group)) {
        data_subset <- analysis_data_pm25_group %>% filter(pm25_group == group)

        if (nrow(data_subset) > 50) {
            pdata_subset <- pdata.frame(data_subset, index = c("iso3c", "year"))

            fe_group <- plm(
                life_exp ~ ghe_gdp + log_gdp_pc + log_population + time_trend,
                data = pdata_subset,
                model = "within",
                effect = "twoways"
            )

            pm25_group_results[[group]] <- list(
                model = fe_group,
                robust = coeftest(fe_group, vcov = vcovHC(fe_group, type = "HC1")),
                n_obs = nrow(data_subset),
                n_countries = n_distinct(data_subset$iso3c)
            )

            cat(sprintf(
                "[OK] %s: GHE coef = %.4f\n",
                group, coef(fe_group)["ghe_gdp"]
            ))
        }
    }

    env_interaction_results$pm25_by_group <- pm25_group_results
}

# 4.3 改善的饮用水源交互效应 | Improved water source interaction
if (sum(!is.na(analysis_data$improved_water)) > 100) {
    cat("[INFO] Analyzing GHE × improved water interaction...\n")

    analysis_data_water <- analysis_data %>%
        filter(!is.na(improved_water)) %>%
        mutate(
            water_centered = improved_water - mean(improved_water, na.rm = TRUE),
            ghe_water_interaction = ghe_gdp * water_centered
        )

    pdata_water <- pdata.frame(analysis_data_water, index = c("iso3c", "year"))

    fe_water_interaction <- plm(
        life_exp ~ ghe_gdp + water_centered + ghe_water_interaction +
            log_gdp_pc + log_population + time_trend,
        data = pdata_water,
        model = "within",
        effect = "twoways"
    )

    fe_water_robust <- coeftest(fe_water_interaction, vcov = vcovHC(fe_water_interaction, type = "HC1"))

    env_interaction_results$water <- list(
        model = fe_water_interaction,
        robust = fe_water_robust,
        n_obs = nrow(analysis_data_water),
        n_countries = n_distinct(analysis_data_water$iso3c)
    )

    cat(sprintf(
        "[OK] Water interaction: GHE coef = %.4f, Interaction coef = %.4f\n",
        coef(fe_water_interaction)["ghe_gdp"],
        coef(fe_water_interaction)["ghe_water_interaction"]
    ))
}

# 4.4 综合环境质量指数 | Composite environmental quality index
cat("[INFO] Creating composite environmental quality index...\n")

analysis_data_env <- analysis_data %>%
    mutate(
        # 标准化环境指标（反向：高污染 = 低分数）| Standardize environmental indicators (inverse: high pollution = low score)
        pm25_std = ifelse(!is.na(pm25),
            -scale(pm25)[, 1], # 反向：PM2.5越高，分数越低 | Inverse: higher PM2.5 = lower score
            NA_real_
        ),
        water_std = ifelse(!is.na(improved_water),
            scale(improved_water)[, 1],
            NA_real_
        ),
        # 综合环境质量指数（如果有足够数据）| Composite environmental quality index (if sufficient data)
        env_quality = ifelse(!is.na(pm25_std) & !is.na(water_std),
            (pm25_std + water_std) / 2,
            ifelse(!is.na(pm25_std), pm25_std,
                ifelse(!is.na(water_std), water_std, NA_real_)
            )
        )
    )

if (sum(!is.na(analysis_data_env$env_quality)) > 100) {
    analysis_data_env_clean <- analysis_data_env %>%
        filter(!is.na(env_quality)) %>%
        mutate(
            env_quality_centered = env_quality - mean(env_quality, na.rm = TRUE),
            ghe_env_interaction = ghe_gdp * env_quality_centered
        )

    pdata_env <- pdata.frame(analysis_data_env_clean, index = c("iso3c", "year"))

    fe_env_interaction <- plm(
        life_exp ~ ghe_gdp + env_quality_centered + ghe_env_interaction +
            log_gdp_pc + log_population + time_trend,
        data = pdata_env,
        model = "within",
        effect = "twoways"
    )

    fe_env_robust <- coeftest(fe_env_interaction, vcov = vcovHC(fe_env_interaction, type = "HC1"))

    env_interaction_results$composite <- list(
        model = fe_env_interaction,
        robust = fe_env_robust,
        n_obs = nrow(analysis_data_env_clean),
        n_countries = n_distinct(analysis_data_env_clean$iso3c)
    )

    cat(sprintf(
        "[OK] Composite environmental quality interaction: GHE coef = %.4f, Interaction coef = %.4f\n",
        coef(fe_env_interaction)["ghe_gdp"],
        coef(fe_env_interaction)["ghe_env_interaction"]
    ))
}

# =============================================================================
# 5. 保存结果 | Save Results
# =============================================================================

cat("\n[STEP 5/5] Saving results...\n")

all_results <- list(
    interactions = env_interaction_results,
    data_summary = list(
        total_obs = nrow(analysis_data),
        n_countries = n_distinct(analysis_data$iso3c),
        year_range = range(analysis_data$year, na.rm = TRUE),
        pm25_obs = sum(!is.na(analysis_data$pm25)),
        water_obs = sum(!is.na(analysis_data$improved_water))
    )
)

saveRDS(all_results, file.path("..", "results", "analysis", "20_environmental_interaction.rds"))
cat("[OK] Results saved to RDS file\n")

# 生成汇总表格 | Generate summary tables
env_table <- data.frame(
    Environmental_Indicator = character(),
    GHE_Coefficient = numeric(),
    GHE_SE = numeric(),
    Interaction_Coefficient = numeric(),
    Interaction_SE = numeric(),
    N_obs = numeric(),
    N_countries = numeric(),
    stringsAsFactors = FALSE
)

if (!is.null(env_interaction_results$pm25)) {
    coef_ghe <- coef(env_interaction_results$pm25$model)["ghe_gdp"]
    se_ghe <- sqrt(diag(vcovHC(env_interaction_results$pm25$model, type = "HC1")))["ghe_gdp"]
    coef_inter <- coef(env_interaction_results$pm25$model)["ghe_pm25_interaction"]
    se_inter <- sqrt(diag(vcovHC(env_interaction_results$pm25$model, type = "HC1")))["ghe_pm25_interaction"]

    env_table <- rbind(env_table, data.frame(
        Environmental_Indicator = "PM2.5",
        GHE_Coefficient = coef_ghe,
        GHE_SE = se_ghe,
        Interaction_Coefficient = coef_inter,
        Interaction_SE = se_inter,
        N_obs = env_interaction_results$pm25$n_obs,
        N_countries = env_interaction_results$pm25$n_countries
    ))
}

if (!is.null(env_interaction_results$water)) {
    coef_ghe <- coef(env_interaction_results$water$model)["ghe_gdp"]
    se_ghe <- sqrt(diag(vcovHC(env_interaction_results$water$model, type = "HC1")))["ghe_gdp"]
    coef_inter <- coef(env_interaction_results$water$model)["ghe_water_interaction"]
    se_inter <- sqrt(diag(vcovHC(env_interaction_results$water$model, type = "HC1")))["ghe_water_interaction"]

    env_table <- rbind(env_table, data.frame(
        Environmental_Indicator = "Improved Water",
        GHE_Coefficient = coef_ghe,
        GHE_SE = se_ghe,
        Interaction_Coefficient = coef_inter,
        Interaction_SE = se_inter,
        N_obs = env_interaction_results$water$n_obs,
        N_countries = env_interaction_results$water$n_countries
    ))
}

if (!is.null(env_interaction_results$composite)) {
    coef_ghe <- coef(env_interaction_results$composite$model)["ghe_gdp"]
    se_ghe <- sqrt(diag(vcovHC(env_interaction_results$composite$model, type = "HC1")))["ghe_gdp"]
    coef_inter <- coef(env_interaction_results$composite$model)["ghe_env_interaction"]
    se_inter <- sqrt(diag(vcovHC(env_interaction_results$composite$model, type = "HC1")))["ghe_env_interaction"]

    env_table <- rbind(env_table, data.frame(
        Environmental_Indicator = "Composite Environmental Quality",
        GHE_Coefficient = coef_ghe,
        GHE_SE = se_ghe,
        Interaction_Coefficient = coef_inter,
        Interaction_SE = se_inter,
        N_obs = env_interaction_results$composite$n_obs,
        N_countries = env_interaction_results$composite$n_countries
    ))
}

if (nrow(env_table) > 0) {
    write_csv(env_table, file.path("..", "tables", "Table_S26_Environmental_Interaction.csv"))
    cat("[OK] Environmental interaction table saved\n")
} else {
    cat("[WARN] No environmental interaction models estimated, skipping table generation\n")
}

cat("\n=============================================================================\n")
cat("  Environmental Health Interaction Analysis Complete\n")
cat("=============================================================================\n\n")

log_message("Environmental health interaction analysis completed successfully")
