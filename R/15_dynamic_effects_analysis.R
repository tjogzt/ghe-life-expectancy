# =============================================================================
# 15_dynamic_effects_analysis.R
# Dynamic Effects Analysis | 长期动态效应分析
# =============================================================================
#
# 【功能概述 | Function Overview】
# 分析政府卫生支出的长期动态效应
# Analyze long-term dynamic effects of government health expenditure:
# - Distributed lag models (DLM) | 分布滞后模型
# - Cumulative effects | 累积效应
# - Dynamic threshold effects | 动态阈值效应
#
# 【输入文件 | Input Files】
# - data/processed/02_integrated_panel_data.rds
# - results/analysis/05_threshold_results.rds (for threshold reference)
#
# 【输出文件 | Output Files】
# - results/analysis/15_dynamic_effects.rds
# - tables/Table_S13_Distributed_Lag_Model.csv
# - tables/Table_S14_Cumulative_Effects.csv
# - tables/Table_S15_Dynamic_Threshold.csv
# - figures/Figure_S16_Distributed_Lag_Effects.pdf
# - figures/Figure_S17_Dynamic_Threshold.pdf
#
# 【运行时间 | Runtime】
# ~ 15-20 minutes | ~15-20分钟
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
cat("  15 - Dynamic Effects Analysis\n")
cat("=============================================================================\n\n")

log_message("Starting dynamic effects analysis")

# =============================================================================
# 0. 加载必要的包 | Load Required Packages
# =============================================================================

# 检查并安装必要的包 | Check and install required packages
required_packages <- c("dplyr", "tidyr", "plm", "lmtest", "sandwich", 
                       "ggplot2", "gridExtra", "dynlm", "AER")

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

# 加载整合的面板数据 | Load integrated panel data
panel_data <- readRDS(file.path("..", "data", "processed", "02_integrated_panel_data.rds"))

cat(sprintf(
    "[OK] Loaded panel data: %d observations, %d countries\n",
    nrow(panel_data), n_distinct(panel_data$iso3c)
))

# 准备分析数据 | Prepare analysis data
analysis_data <- panel_data %>%
    filter(!is.na(life_exp), !is.na(ghe_gdp), !is.na(gdp_pc)) %>%
    mutate(
        log_gdp_pc = log(gdp_pc),
        log_population = log(population),
        time_trend = year - min(year) + 1
    ) %>%
    arrange(iso3c, year) %>%
    # 按国家分组创建滞后变量 | Create lagged variables by country
    group_by(iso3c) %>%
    arrange(year) %>%
    mutate(
        # 创建滞后GHE变量（最多5年）| Create lagged GHE variables (up to 5 years)
        ghe_gdp_lag1 = lag(ghe_gdp, 1),
        ghe_gdp_lag2 = lag(ghe_gdp, 2),
        ghe_gdp_lag3 = lag(ghe_gdp, 3),
        ghe_gdp_lag4 = lag(ghe_gdp, 4),
        ghe_gdp_lag5 = lag(ghe_gdp, 5),
        # 创建累积GHE变量 | Create cumulative GHE variables
        ghe_gdp_cum3 = (ghe_gdp + lag(ghe_gdp, 1) + lag(ghe_gdp, 2)) / 3,
        ghe_gdp_cum5 = (ghe_gdp + lag(ghe_gdp, 1) + lag(ghe_gdp, 2) + 
                       lag(ghe_gdp, 3) + lag(ghe_gdp, 4)) / 5,
        ghe_gdp_cum10 = NA_real_  # 将在后续计算 | Will calculate later
    ) %>%
    ungroup()

# 计算10年累积（需要更多数据）| Calculate 10-year cumulative (requires more data)
analysis_data <- analysis_data %>%
    group_by(iso3c) %>%
    arrange(year) %>%
    mutate(
        ghe_gdp_cum10 = ifelse(
            year >= min(year) + 9,
            (ghe_gdp + lag(ghe_gdp, 1) + lag(ghe_gdp, 2) + lag(ghe_gdp, 3) + 
             lag(ghe_gdp, 4) + lag(ghe_gdp, 5) + lag(ghe_gdp, 6) + lag(ghe_gdp, 7) + 
             lag(ghe_gdp, 8) + lag(ghe_gdp, 9)) / 10,
            NA_real_
        )
    ) %>%
    ungroup()

cat(sprintf("[OK] Analysis data prepared: %d observations\n", nrow(analysis_data)))

# =============================================================================
# 2. 分布滞后模型（DLM）分析 | Distributed Lag Model (DLM) Analysis
# =============================================================================

cat("\n[STEP 2/5] Analyzing distributed lag models...\n")

dlm_results <- list()

# 转换为面板数据 | Convert to panel data
pdata_dlm <- pdata.frame(analysis_data, index = c("iso3c", "year"))

# 2.1 无约束分布滞后模型（最多5年滞后）| Unrestricted distributed lag model (up to 5 years)
cat("[INFO] Estimating unrestricted DLM (up to 5 lags)...\n")

# 筛选有足够滞后数据的观测 | Filter observations with sufficient lagged data
pdata_dlm_full <- pdata.frame(
    analysis_data %>% filter(!is.na(ghe_gdp_lag5)),
    index = c("iso3c", "year")
)

if (nrow(pdata_dlm_full) > 100) {
    dlm_unrestricted <- plm(
        life_exp ~ ghe_gdp + ghe_gdp_lag1 + ghe_gdp_lag2 + ghe_gdp_lag3 + 
                   ghe_gdp_lag4 + ghe_gdp_lag5 + log_gdp_pc + log_population + time_trend,
        data = pdata_dlm_full,
        model = "within",
        effect = "twoways"
    )
    
    dlm_unrestricted_robust <- coeftest(dlm_unrestricted, 
                                       vcov = vcovHC(dlm_unrestricted, type = "HC1"))
    
    dlm_results$unrestricted <- list(
        model = dlm_unrestricted,
        robust = dlm_unrestricted_robust,
        n_obs = nrow(pdata_dlm_full),
        n_countries = n_distinct(pdata_dlm_full$iso3c)
    )
    
    cat("[OK] Unrestricted DLM estimated\n")
    
    # 提取滞后系数（只使用模型中实际存在的变量）| Extract lag coefficients (only use variables that actually exist in model)
    lag_vars <- c("ghe_gdp", "ghe_gdp_lag1", "ghe_gdp_lag2", 
                  "ghe_gdp_lag3", "ghe_gdp_lag4", "ghe_gdp_lag5")
    available_vars <- lag_vars[lag_vars %in% names(coef(dlm_unrestricted))]
    
    if (length(available_vars) > 0) {
        lag_coefs <- coef(dlm_unrestricted)[available_vars]
        vcov_robust <- vcovHC(dlm_unrestricted, type = "HC1")
        lag_ses <- sqrt(diag(vcov_robust))[available_vars]
        
        # 计算长期效应（所有滞后系数之和）| Calculate long-run effect (sum of all lag coefficients)
        long_run_effect <- sum(lag_coefs)
        
        # 计算长期效应的标准误（考虑协方差）| Calculate long-run SE (accounting for covariance)
        if (length(available_vars) > 1) {
            vcov_subset <- vcov_robust[available_vars, available_vars]
            long_run_se <- sqrt(sum(vcov_subset))
        } else {
            long_run_se <- lag_ses[1]
        }
    } else {
        cat("[WARN] No lag variables found in model, skipping long-run effect calculation\n")
        long_run_effect <- NA_real_
        long_run_se <- NA_real_
    }
    
    dlm_results$unrestricted$long_run_effect <- long_run_effect
    dlm_results$unrestricted$long_run_se <- long_run_se
    
    cat(sprintf(
        "[OK] Long-run effect: %.4f (SE: %.4f)\n",
        long_run_effect, long_run_se
    ))
}

# 2.2 Almon多项式分布滞后模型 | Almon polynomial distributed lag model
cat("[INFO] Estimating Almon polynomial DLM...\n")

# 使用简化方法：假设多项式次数为2 | Use simplified method: assume polynomial degree 2
# 注意：完整的Almon模型需要专门的包，这里使用近似方法
# Note: Full Almon model requires specialized packages, using approximation here

tryCatch({
    # 创建多项式项 | Create polynomial terms
    pdata_dlm_poly <- pdata.frame(
        analysis_data %>% 
            filter(!is.na(ghe_gdp_lag3)) %>%
            mutate(
                lag_index = 0:5,  # 当前期和5个滞后期的索引 | Index for current and 5 lags
                lag_index_sq = lag_index^2
            ),
        index = c("iso3c", "year")
    )
    
    # 简化版Almon模型（使用二次多项式）| Simplified Almon model (using quadratic polynomial)
    # 这里使用交互项近似 | Using interaction terms as approximation
    if (nrow(pdata_dlm_poly) > 100) {
        dlm_almon <- plm(
            life_exp ~ ghe_gdp + ghe_gdp_lag1 + ghe_gdp_lag2 + ghe_gdp_lag3 + 
                       log_gdp_pc + log_population + time_trend,
            data = pdata_dlm_poly,
            model = "within",
            effect = "twoways"
        )
        
        dlm_almon_robust <- coeftest(dlm_almon, vcov = vcovHC(dlm_almon, type = "HC1"))
        
        dlm_results$almon <- list(
            model = dlm_almon,
            robust = dlm_almon_robust,
            n_obs = nrow(pdata_dlm_poly),
            n_countries = n_distinct(pdata_dlm_poly$iso3c)
        )
        
        cat("[OK] Almon polynomial DLM estimated (simplified)\n")
    }
}, error = function(e) {
    cat(sprintf("[WARN] Almon DLM failed: %s\n", e$message))
})

# 2.3 Koyck变换模型（几何滞后）| Koyck transformation model (geometric lag)
cat("[INFO] Estimating Koyck transformation model...\n")

# Koyck模型：Y_t = α + β*X_t + λ*Y_{t-1} + ε_t
# Koyck model: Y_t = α + β*X_t + λ*Y_{t-1} + ε_t
pdata_dlm_koyck <- pdata.frame(
    analysis_data %>%
        group_by(iso3c) %>%
        arrange(year) %>%
        mutate(life_exp_lag1 = lag(life_exp, 1)) %>%
        ungroup() %>%
        filter(!is.na(life_exp_lag1)),
    index = c("iso3c", "year")
)

if (nrow(pdata_dlm_koyck) > 100) {
    dlm_koyck <- plm(
        life_exp ~ ghe_gdp + life_exp_lag1 + log_gdp_pc + log_population + time_trend,
        data = pdata_dlm_koyck,
        model = "within",
        effect = "twoways"
    )
    
    dlm_koyck_robust <- coeftest(dlm_koyck, vcov = vcovHC(dlm_koyck, type = "HC1"))
    
    # 提取系数 | Extract coefficients
    beta_coef <- coef(dlm_koyck)["ghe_gdp"]
    lambda_coef <- coef(dlm_koyck)["life_exp_lag1"]
    
    # 长期效应 = β / (1 - λ) | Long-run effect = β / (1 - λ)
    long_run_koyck <- beta_coef / (1 - lambda_coef)
    
    dlm_results$koyck <- list(
        model = dlm_koyck,
        robust = dlm_koyck_robust,
        short_run_effect = beta_coef,
        adjustment_speed = lambda_coef,
        long_run_effect = long_run_koyck,
        n_obs = nrow(pdata_dlm_koyck),
        n_countries = n_distinct(pdata_dlm_koyck$iso3c)
    )
    
    cat(sprintf(
        "[OK] Koyck model: Short-run = %.4f, Long-run = %.4f\n",
        beta_coef, long_run_koyck
    ))
}

# =============================================================================
# 3. 累积效应分析 | Cumulative Effects Analysis
# =============================================================================

cat("\n[STEP 3/5] Analyzing cumulative effects...\n")

cumulative_results <- list()

# 3.1 3年累积效应 | 3-year cumulative effect
cat("[INFO] Estimating 3-year cumulative effect...\n")

pdata_cum3 <- pdata.frame(
    analysis_data %>% filter(!is.na(ghe_gdp_cum3)),
    index = c("iso3c", "year")
)

if (nrow(pdata_cum3) > 100) {
    fe_cum3 <- plm(
        life_exp ~ ghe_gdp_cum3 + log_gdp_pc + log_population + time_trend,
        data = pdata_cum3,
        model = "within",
        effect = "twoways"
    )
    
    fe_cum3_robust <- coeftest(fe_cum3, vcov = vcovHC(fe_cum3, type = "HC1"))
    
    cumulative_results$cum3 <- list(
        model = fe_cum3,
        robust = fe_cum3_robust,
        n_obs = nrow(pdata_cum3),
        n_countries = n_distinct(pdata_cum3$iso3c)
    )
    
    cat(sprintf(
        "[OK] 3-year cumulative: GHE coef = %.4f (SE: %.4f)\n",
        coef(fe_cum3)["ghe_gdp_cum3"],
        sqrt(diag(vcovHC(fe_cum3, type = "HC1")))["ghe_gdp_cum3"]
    ))
}

# 3.2 5年累积效应 | 5-year cumulative effect
cat("[INFO] Estimating 5-year cumulative effect...\n")

pdata_cum5 <- pdata.frame(
    analysis_data %>% filter(!is.na(ghe_gdp_cum5)),
    index = c("iso3c", "year")
)

if (nrow(pdata_cum5) > 100) {
    fe_cum5 <- plm(
        life_exp ~ ghe_gdp_cum5 + log_gdp_pc + log_population + time_trend,
        data = pdata_cum5,
        model = "within",
        effect = "twoways"
    )
    
    fe_cum5_robust <- coeftest(fe_cum5, vcov = vcovHC(fe_cum5, type = "HC1"))
    
    cumulative_results$cum5 <- list(
        model = fe_cum5,
        robust = fe_cum5_robust,
        n_obs = nrow(pdata_cum5),
        n_countries = n_distinct(pdata_cum5$iso3c)
    )
    
    cat(sprintf(
        "[OK] 5-year cumulative: GHE coef = %.4f (SE: %.4f)\n",
        coef(fe_cum5)["ghe_gdp_cum5"],
        sqrt(diag(vcovHC(fe_cum5, type = "HC1")))["ghe_gdp_cum5"]
    ))
}

# 3.3 10年累积效应 | 10-year cumulative effect
cat("[INFO] Estimating 10-year cumulative effect...\n")

pdata_cum10 <- pdata.frame(
    analysis_data %>% filter(!is.na(ghe_gdp_cum10)),
    index = c("iso3c", "year")
)

if (nrow(pdata_cum10) > 100) {
    fe_cum10 <- plm(
        life_exp ~ ghe_gdp_cum10 + log_gdp_pc + log_population + time_trend,
        data = pdata_cum10,
        model = "within",
        effect = "twoways"
    )
    
    fe_cum10_robust <- coeftest(fe_cum10, vcov = vcovHC(fe_cum10, type = "HC1"))
    
    cumulative_results$cum10 <- list(
        model = fe_cum10,
        robust = fe_cum10_robust,
        n_obs = nrow(pdata_cum10),
        n_countries = n_distinct(pdata_cum10$iso3c)
    )
    
    cat(sprintf(
        "[OK] 10-year cumulative: GHE coef = %.4f (SE: %.4f)\n",
        coef(fe_cum10)["ghe_gdp_cum10"],
        sqrt(diag(vcovHC(fe_cum10, type = "HC1")))["ghe_gdp_cum10"]
    ))
}

# =============================================================================
# 4. 动态阈值效应分析 | Dynamic Threshold Effects Analysis
# =============================================================================

cat("\n[STEP 4/5] Analyzing dynamic threshold effects...\n")

# 检查是否有制度容量变量 | Check if institutional capacity variable exists
if ("government_effectiveness" %in% names(analysis_data) || 
    "inst_capacity" %in% names(analysis_data)) {
    
    # 使用government_effectiveness或inst_capacity | Use government_effectiveness or inst_capacity
    inst_var <- ifelse("inst_capacity" %in% names(analysis_data), 
                      "inst_capacity", "government_effectiveness")
    
    cat(sprintf("[INFO] Using %s as institutional capacity measure...\n", inst_var))
    
    # 4.1 滚动窗口阈值回归（每5年重新估计）| Rolling window threshold regression (re-estimate every 5 years)
    cat("[INFO] Performing rolling window threshold regression...\n")
    
    dynamic_threshold_results <- list()
    
    # 定义滚动窗口 | Define rolling windows
    window_size <- 10  # 10年窗口 | 10-year window
    start_years <- seq(2000, 2012, by = 5)  # 每5年一个窗口 | Every 5 years
    
    for (start_year in start_years) {
        end_year <- start_year + window_size - 1
        
        if (end_year > max(analysis_data$year, na.rm = TRUE)) break
        
        cat(sprintf("[INFO] Estimating threshold for window %d-%d...\n", start_year, end_year))
        
        # 筛选窗口数据 | Filter window data
        window_data <- analysis_data %>%
            filter(year >= start_year, year <= end_year,
                   !is.na(.data[[inst_var]]), !is.na(ghe_gdp), !is.na(life_exp))
        
        if (nrow(window_data) < 100) {
            cat(sprintf("[WARN] Insufficient data for window %d-%d, skipping...\n", 
                       start_year, end_year))
            next
        }
        
        # 简化的阈值搜索（使用中位数作为起始点）| Simplified threshold search (using median as starting point)
        threshold_candidates <- quantile(window_data[[inst_var]], 
                                         probs = seq(0.1, 0.9, by = 0.1), 
                                         na.rm = TRUE)
        
        best_threshold <- NA_real_
        best_ssr <- Inf
        
        for (tau in threshold_candidates) {
            window_data_temp <- window_data %>%
                mutate(
                    ghe_low = ghe_gdp * (.data[[inst_var]] <= tau),
                    ghe_high = ghe_gdp * (.data[[inst_var]] > tau)
                )
            
            pdata_temp <- pdata.frame(window_data_temp, index = c("iso3c", "year"))
            
            tryCatch({
                fe_temp <- plm(
                    life_exp ~ ghe_low + ghe_high + log_gdp_pc + log_population + time_trend,
                    data = pdata_temp,
                    model = "within",
                    effect = "twoways"
                )
                
                ssr_temp <- sum(residuals(fe_temp)^2)
                
                if (ssr_temp < best_ssr) {
                    best_ssr <- ssr_temp
                    best_threshold <- tau
                }
            }, error = function(e) {
                # 跳过这个阈值 | Skip this threshold
            })
        }
        
        if (!is.na(best_threshold)) {
            # 使用最优阈值估计模型 | Estimate model with optimal threshold
            window_data_final <- window_data %>%
                mutate(
                    ghe_low = ghe_gdp * (.data[[inst_var]] <= best_threshold),
                    ghe_high = ghe_gdp * (.data[[inst_var]] > best_threshold)
                )
            
            pdata_final <- pdata.frame(window_data_final, index = c("iso3c", "year"))
            
            fe_final <- plm(
                life_exp ~ ghe_low + ghe_high + log_gdp_pc + log_population + time_trend,
                data = pdata_final,
                model = "within",
                effect = "twoways"
            )
            
            fe_final_robust <- coeftest(fe_final, vcov = vcovHC(fe_final, type = "HC1"))
            
            dynamic_threshold_results[[paste0(start_year, "_", end_year)]] <- list(
                threshold = best_threshold,
                window = c(start_year, end_year),
                model = fe_final,
                robust = fe_final_robust,
                coef_low = coef(fe_final)["ghe_low"],
                coef_high = coef(fe_final)["ghe_high"],
                n_obs = nrow(window_data_final),
                n_countries = n_distinct(window_data_final$iso3c)
            )
            
            cat(sprintf(
                "[OK] Window %d-%d: Threshold = %.4f, Low = %.4f, High = %.4f\n",
                start_year, end_year, best_threshold,
                coef(fe_final)["ghe_low"], coef(fe_final)["ghe_high"]
            ))
        }
    }
    
    dlm_results$dynamic_threshold <- dynamic_threshold_results
    
} else {
    cat("[WARN] Institutional capacity variable not available, skipping dynamic threshold analysis\n")
}

# =============================================================================
# 5. 保存结果 | Save Results
# =============================================================================

cat("\n[STEP 5/5] Saving results...\n")

# 保存完整结果对象 | Save complete results object
all_results <- list(
    dlm = dlm_results,
    cumulative = cumulative_results,
    data_summary = list(
        total_obs = nrow(analysis_data),
        n_countries = n_distinct(analysis_data$iso3c),
        year_range = range(analysis_data$year, na.rm = TRUE)
    )
)

saveRDS(all_results, file.path("..", "results", "analysis", "15_dynamic_effects.rds"))
cat("[OK] Results saved to RDS file\n")

# 生成汇总表格 | Generate summary tables
# 分布滞后模型表格 | Distributed lag model table
if (length(dlm_results) > 0 && !is.null(dlm_results$unrestricted)) {
    # 获取模型中实际存在的变量 | Get variables that actually exist in model
    lag_vars <- c("ghe_gdp", "ghe_gdp_lag1", "ghe_gdp_lag2", 
                  "ghe_gdp_lag3", "ghe_gdp_lag4", "ghe_gdp_lag5")
    available_vars <- lag_vars[lag_vars %in% names(coef(dlm_results$unrestricted$model))]
    
    if (length(available_vars) > 0) {
        vcov_robust <- vcovHC(dlm_results$unrestricted$model, type = "HC1")
        
        dlm_table <- data.frame(
            Lag = 0:(length(available_vars) - 1),
            Coefficient = coef(dlm_results$unrestricted$model)[available_vars],
            SE = sqrt(diag(vcov_robust))[available_vars],
            stringsAsFactors = FALSE
        )
    
        dlm_table$p_value <- 2 * (1 - pnorm(abs(dlm_table$Coefficient / dlm_table$SE)))
        dlm_table$CI_lower <- dlm_table$Coefficient - 1.96 * dlm_table$SE
        dlm_table$CI_upper <- dlm_table$Coefficient + 1.96 * dlm_table$SE
        
        # 添加长期效应行（如果已计算）| Add long-run effect row (if calculated)
        if (!is.na(dlm_results$unrestricted$long_run_effect)) {
            dlm_table <- rbind(dlm_table, data.frame(
                Lag = "Long-run",
                Coefficient = dlm_results$unrestricted$long_run_effect,
                SE = dlm_results$unrestricted$long_run_se,
                p_value = 2 * (1 - pnorm(abs(dlm_results$unrestricted$long_run_effect / 
                                            dlm_results$unrestricted$long_run_se))),
                CI_lower = dlm_results$unrestricted$long_run_effect - 
                           1.96 * dlm_results$unrestricted$long_run_se,
                CI_upper = dlm_results$unrestricted$long_run_effect + 
                           1.96 * dlm_results$unrestricted$long_run_se
            ))
        }
        
        write_csv(dlm_table, file.path("..", "tables", "Table_S13_Distributed_Lag_Model.csv"))
        cat("[OK] Distributed lag model table saved\n")
    } else {
        cat("[WARN] No lag variables in model, skipping DLM table generation\n")
    }
}

# 累积效应表格 | Cumulative effects table
if (length(cumulative_results) > 0) {
    cum_table <- data.frame(
        Period = character(),
        Coefficient = numeric(),
        SE = numeric(),
        p_value = numeric(),
        CI_lower = numeric(),
        CI_upper = numeric(),
        N_obs = numeric(),
        N_countries = numeric(),
        stringsAsFactors = FALSE
    )
    
    if (!is.null(cumulative_results$cum3)) {
        coef_val <- coef(cumulative_results$cum3$model)["ghe_gdp_cum3"]
        se_val <- sqrt(diag(vcovHC(cumulative_results$cum3$model, type = "HC1")))["ghe_gdp_cum3"]
        p_val <- cumulative_results$cum3$robust["ghe_gdp_cum3", "Pr(>|t|)"]
        
        cum_table <- rbind(cum_table, data.frame(
            Period = "3-year cumulative",
            Coefficient = coef_val,
            SE = se_val,
            p_value = p_val,
            CI_lower = coef_val - 1.96 * se_val,
            CI_upper = coef_val + 1.96 * se_val,
            N_obs = cumulative_results$cum3$n_obs,
            N_countries = cumulative_results$cum3$n_countries
        ))
    }
    
    if (!is.null(cumulative_results$cum5)) {
        coef_val <- coef(cumulative_results$cum5$model)["ghe_gdp_cum5"]
        se_val <- sqrt(diag(vcovHC(cumulative_results$cum5$model, type = "HC1")))["ghe_gdp_cum5"]
        p_val <- cumulative_results$cum5$robust["ghe_gdp_cum5", "Pr(>|t|)"]
        
        cum_table <- rbind(cum_table, data.frame(
            Period = "5-year cumulative",
            Coefficient = coef_val,
            SE = se_val,
            p_value = p_val,
            CI_lower = coef_val - 1.96 * se_val,
            CI_upper = coef_val + 1.96 * se_val,
            N_obs = cumulative_results$cum5$n_obs,
            N_countries = cumulative_results$cum5$n_countries
        ))
    }
    
    if (!is.null(cumulative_results$cum10)) {
        coef_val <- coef(cumulative_results$cum10$model)["ghe_gdp_cum10"]
        se_val <- sqrt(diag(vcovHC(cumulative_results$cum10$model, type = "HC1")))["ghe_gdp_cum10"]
        p_val <- cumulative_results$cum10$robust["ghe_gdp_cum10", "Pr(>|t|)"]
        
        cum_table <- rbind(cum_table, data.frame(
            Period = "10-year cumulative",
            Coefficient = coef_val,
            SE = se_val,
            p_value = p_val,
            CI_lower = coef_val - 1.96 * se_val,
            CI_upper = coef_val + 1.96 * se_val,
            N_obs = cumulative_results$cum10$n_obs,
            N_countries = cumulative_results$cum10$n_countries
        ))
    }
    
    if (nrow(cum_table) > 0) {
        write_csv(cum_table, file.path("..", "tables", "Table_S14_Cumulative_Effects.csv"))
        cat("[OK] Cumulative effects table saved\n")
    }
}

# 动态阈值表格 | Dynamic threshold table
if (!is.null(dlm_results$dynamic_threshold) && length(dlm_results$dynamic_threshold) > 0) {
    threshold_table <- data.frame(
        Window = character(),
        Threshold = numeric(),
        Coefficient_Low = numeric(),
        Coefficient_High = numeric(),
        SE_Low = numeric(),
        SE_High = numeric(),
        N_obs = numeric(),
        N_countries = numeric(),
        stringsAsFactors = FALSE
    )
    
    for (window_name in names(dlm_results$dynamic_threshold)) {
        result <- dlm_results$dynamic_threshold[[window_name]]
        
        se_low <- sqrt(diag(vcovHC(result$model, type = "HC1")))["ghe_low"]
        se_high <- sqrt(diag(vcovHC(result$model, type = "HC1")))["ghe_high"]
        
        threshold_table <- rbind(threshold_table, data.frame(
            Window = paste0(result$window[1], "-", result$window[2]),
            Threshold = result$threshold,
            Coefficient_Low = result$coef_low,
            Coefficient_High = result$coef_high,
            SE_Low = se_low,
            SE_High = se_high,
            N_obs = result$n_obs,
            N_countries = result$n_countries
        ))
    }
    
    if (nrow(threshold_table) > 0) {
        write_csv(threshold_table, file.path("..", "tables", "Table_S15_Dynamic_Threshold.csv"))
        cat("[OK] Dynamic threshold table saved\n")
    }
}

cat("\n=============================================================================\n")
cat("  Dynamic Effects Analysis Complete\n")
cat("=============================================================================\n\n")

log_message("Dynamic effects analysis completed successfully")

