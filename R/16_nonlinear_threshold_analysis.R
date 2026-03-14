# =============================================================================
# 16_nonlinear_threshold_analysis.R
# Nonlinear Threshold Effects Analysis | 非线性阈值效应深化分析
# =============================================================================
#
# 【功能概述 | Function Overview】
# 深化阈值回归分析，检验非线性阈值效应
# Deepen threshold regression analysis to test nonlinear threshold effects:
# - Multiple threshold testing | 多阈值检验
# - Smooth threshold regression (STR) | 平滑阈值回归
# - Threshold symmetry testing | 阈值效应对称性检验
#
# 【输入文件 | Input Files】
# - data/processed/02_integrated_panel_data.rds
# - results/analysis/05_threshold_results.rds
#
# 【输出文件 | Output Files】
# - results/analysis/16_nonlinear_threshold.rds
# - tables/Table_S16_Multiple_Threshold_Test.csv
# - tables/Table_S17_Smooth_Threshold_Regression.csv
# - tables/Table_S18_Threshold_Symmetry.csv
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
cat("  16 - Nonlinear Threshold Effects Analysis\n")
cat("=============================================================================\n\n")

log_message("Starting nonlinear threshold effects analysis")

# =============================================================================
# 0. 加载必要的包 | Load Required Packages
# =============================================================================

required_packages <- c("dplyr", "tidyr", "plm", "lmtest", "sandwich", 
                       "ggplot2", "gridExtra", "readr")

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

# 准备阈值分析数据 | Prepare threshold analysis data
threshold_data <- panel_data %>%
    filter(!is.na(life_exp), !is.na(ghe_gdp), !is.na(gdp_pc)) %>%
    mutate(
        log_gdp_pc = log(gdp_pc),
        log_population = log(population),
        time_trend = year - min(year) + 1
    )

# 检查制度容量变量 | Check institutional capacity variable
if ("government_effectiveness" %in% names(threshold_data)) {
    threshold_data <- threshold_data %>%
        filter(!is.na(government_effectiveness)) %>%
        mutate(inst_capacity = government_effectiveness)
} else if ("inst_capacity" %in% names(threshold_data)) {
    threshold_data <- threshold_data %>%
        filter(!is.na(inst_capacity))
} else {
    stop("Institutional capacity variable not found")
}

cat(sprintf("[OK] Threshold data prepared: %d observations\n", nrow(threshold_data)))

# =============================================================================
# 2. 多阈值检验 | Multiple Threshold Testing
# =============================================================================

cat("\n[STEP 2/5] Testing multiple thresholds...\n")

multiple_threshold_results <- list()

# 2.1 单阈值模型（基准）| Single threshold model (baseline)
cat("[INFO] Estimating single threshold model...\n")

# 使用中位数作为起始阈值 | Use median as starting threshold
tau1_candidate <- median(threshold_data$inst_capacity, na.rm = TRUE)

threshold_data_single <- threshold_data %>%
    mutate(
        ghe_low = ghe_gdp * (inst_capacity <= tau1_candidate),
        ghe_high = ghe_gdp * (inst_capacity > tau1_candidate)
    )

pdata_single <- pdata.frame(threshold_data_single, index = c("iso3c", "year"))

fe_single <- plm(
    life_exp ~ ghe_low + ghe_high + log_gdp_pc + log_population + time_trend,
    data = pdata_single,
    model = "within",
    effect = "twoways"
)

ssr_single <- sum(residuals(fe_single)^2)
# plm对象不支持AIC/BIC，使用SSR和自由度计算 | plm objects don't support AIC/BIC, use SSR and degrees of freedom
n_obs_single <- length(residuals(fe_single))
n_params_single <- length(coef(fe_single))
aic_single <- n_obs_single * log(ssr_single / n_obs_single) + 2 * n_params_single
bic_single <- n_obs_single * log(ssr_single / n_obs_single) + n_params_single * log(n_obs_single)

multiple_threshold_results$single <- list(
    model = fe_single,
    threshold = tau1_candidate,
    ssr = ssr_single,
    aic = aic_single,
    bic = bic_single
)

cat(sprintf("[OK] Single threshold: SSR = %.2f, AIC = %.2f, BIC = %.2f\n",
            ssr_single, aic_single, bic_single))

# 2.2 双阈值模型 | Double threshold model
cat("[INFO] Estimating double threshold model...\n")

# 搜索两个阈值 | Search for two thresholds
tau_candidates <- quantile(threshold_data$inst_capacity, 
                          probs = seq(0.2, 0.8, by = 0.1), 
                          na.rm = TRUE)

best_double <- list(ssr = Inf, tau1 = NA, tau2 = NA, aic = Inf, bic = Inf)

for (i in 1:(length(tau_candidates) - 1)) {
    for (j in (i + 1):length(tau_candidates)) {
        tau1 <- tau_candidates[i]
        tau2 <- tau_candidates[j]
        
        if (tau1 >= tau2) next
        
        threshold_data_double <- threshold_data %>%
            mutate(
                regime = case_when(
                    inst_capacity <= tau1 ~ "low",
                    inst_capacity <= tau2 ~ "medium",
                    TRUE ~ "high"
                ),
                ghe_low = ghe_gdp * (regime == "low"),
                ghe_medium = ghe_gdp * (regime == "medium"),
                ghe_high = ghe_gdp * (regime == "high")
            )
        
        pdata_double <- pdata.frame(threshold_data_double, index = c("iso3c", "year"))
        
        tryCatch({
            fe_double <- plm(
                life_exp ~ ghe_low + ghe_medium + ghe_high + log_gdp_pc + log_population + time_trend,
                data = pdata_double,
                model = "within",
                effect = "twoways"
            )
            
            ssr_double <- sum(residuals(fe_double)^2)
            n_obs_double <- length(residuals(fe_double))
            n_params_double <- length(coef(fe_double))
            aic_double <- n_obs_double * log(ssr_double / n_obs_double) + 2 * n_params_double
            bic_double <- n_obs_double * log(ssr_double / n_obs_double) + n_params_double * log(n_obs_double)
            
            if (ssr_double < best_double$ssr) {
                best_double$ssr <- ssr_double
                best_double$tau1 <- tau1
                best_double$tau2 <- tau2
                best_double$aic <- aic_double
                best_double$bic <- bic_double
                best_double$model <- fe_double
            }
        }, error = function(e) {
            # 跳过这个组合 | Skip this combination
        })
    }
}

if (!is.na(best_double$tau1)) {
    multiple_threshold_results$double <- best_double
    cat(sprintf("[OK] Double threshold: τ1 = %.4f, τ2 = %.4f, SSR = %.2f, AIC = %.2f, BIC = %.2f\n",
                best_double$tau1, best_double$tau2, best_double$ssr, 
                best_double$aic, best_double$bic))
} else {
    cat("[WARN] Double threshold model failed\n")
}

# 2.3 三阈值模型 | Triple threshold model
cat("[INFO] Estimating triple threshold model...\n")

best_triple <- list(ssr = Inf, tau1 = NA, tau2 = NA, tau3 = NA, aic = Inf, bic = Inf)

# 简化搜索（使用四分位数）| Simplified search (using quartiles)
tau_quartiles <- quantile(threshold_data$inst_capacity, 
                          probs = c(0.25, 0.5, 0.75), 
                          na.rm = TRUE)

for (i in 1:length(tau_quartiles)) {
    for (j in (i + 1):length(tau_quartiles)) {
        for (k in (j + 1):length(tau_quartiles)) {
            tau1 <- tau_quartiles[i]
            tau2 <- tau_quartiles[j]
            tau3 <- tau_quartiles[k]
            
            if (is.na(tau1) || is.na(tau2) || is.na(tau3)) next
            if (tau1 >= tau2 || tau2 >= tau3) next
            
            threshold_data_triple <- threshold_data %>%
                mutate(
                    regime = case_when(
                        inst_capacity <= tau1 ~ "low",
                        inst_capacity <= tau2 ~ "medium_low",
                        inst_capacity <= tau3 ~ "medium_high",
                        TRUE ~ "high"
                    ),
                    ghe_low = ghe_gdp * (regime == "low"),
                    ghe_med_low = ghe_gdp * (regime == "medium_low"),
                    ghe_med_high = ghe_gdp * (regime == "medium_high"),
                    ghe_high = ghe_gdp * (regime == "high")
                )
            
            pdata_triple <- pdata.frame(threshold_data_triple, index = c("iso3c", "year"))
            
            tryCatch({
                fe_triple <- plm(
                    life_exp ~ ghe_low + ghe_med_low + ghe_med_high + ghe_high + 
                              log_gdp_pc + log_population + time_trend,
                    data = pdata_triple,
                    model = "within",
                    effect = "twoways"
                )
                
                ssr_triple <- sum(residuals(fe_triple)^2)
                n_obs_triple <- length(residuals(fe_triple))
                n_params_triple <- length(coef(fe_triple))
                aic_triple <- n_obs_triple * log(ssr_triple / n_obs_triple) + 2 * n_params_triple
                bic_triple <- n_obs_triple * log(ssr_triple / n_obs_triple) + n_params_triple * log(n_obs_triple)
                
                if (ssr_triple < best_triple$ssr) {
                    best_triple$ssr <- ssr_triple
                    best_triple$tau1 <- tau1
                    best_triple$tau2 <- tau2
                    best_triple$tau3 <- tau3
                    best_triple$aic <- aic_triple
                    best_triple$bic <- bic_triple
                    best_triple$model <- fe_triple
                }
            }, error = function(e) {
                # 跳过这个组合 | Skip this combination
            })
        }
    }
}

if (!is.na(best_triple$tau1)) {
    multiple_threshold_results$triple <- best_triple
    cat(sprintf("[OK] Triple threshold: τ1 = %.4f, τ2 = %.4f, τ3 = %.4f, SSR = %.2f, AIC = %.2f, BIC = %.2f\n",
                best_triple$tau1, best_triple$tau2, best_triple$tau3, 
                best_triple$ssr, best_triple$aic, best_triple$bic))
} else {
    cat("[WARN] Triple threshold model failed\n")
}

# =============================================================================
# 3. 平滑阈值回归（STR）| Smooth Threshold Regression (STR)
# =============================================================================

cat("\n[STEP 3/5] Estimating smooth threshold regression...\n")

smooth_threshold_results <- list()

# STR模型：使用逻辑函数实现平滑过渡
# STR model: Use logistic function for smooth transition
# Y = β0 + β1*GHE + β2*GHE*F(inst_capacity - τ) + controls
# 其中 F(z) = 1 / (1 + exp(-γ*z)) 是逻辑函数
# Where F(z) = 1 / (1 + exp(-γ*z)) is logistic function

# 使用非线性最小二乘法估计 | Use nonlinear least squares
tryCatch({
    # 准备数据 | Prepare data
    str_data <- threshold_data %>%
        mutate(
            inst_centered = inst_capacity - median(inst_capacity, na.rm = TRUE)
        ) %>%
        filter(!is.na(inst_centered))
    
    # 初始值 | Initial values
    tau_init <- median(str_data$inst_capacity, na.rm = TRUE)
    gamma_init <- 1.0  # 平滑参数 | Smoothing parameter
    
    # 简化的STR模型（使用固定γ）| Simplified STR model (using fixed γ)
    # 通过网格搜索找到最优τ | Find optimal τ through grid search
    tau_candidates_str <- quantile(str_data$inst_capacity, 
                                   probs = seq(0.1, 0.9, by = 0.05), 
                                   na.rm = TRUE)
    
    best_str <- list(ssr = Inf, tau = NA, gamma = gamma_init)
    
    for (tau in tau_candidates_str) {
        str_data_temp <- str_data %>%
            mutate(
                inst_diff = inst_capacity - tau,
                transition = 1 / (1 + exp(-gamma_init * inst_diff)),
                ghe_transition = ghe_gdp * transition
            )
        
        pdata_str <- pdata.frame(str_data_temp, index = c("iso3c", "year"))
        
        tryCatch({
            fe_str <- plm(
                life_exp ~ ghe_gdp + ghe_transition + log_gdp_pc + log_population + time_trend,
                data = pdata_str,
                model = "within",
                effect = "twoways"
            )
            
            ssr_str <- sum(residuals(fe_str)^2)
            
            if (ssr_str < best_str$ssr) {
                best_str$ssr <- ssr_str
                best_str$tau <- tau
                best_str$model <- fe_str
            }
        }, error = function(e) {
            # 跳过 | Skip
        })
    }
    
    if (!is.na(best_str$tau)) {
        smooth_threshold_results$str <- best_str
        cat(sprintf("[OK] STR model: τ = %.4f, γ = %.2f, SSR = %.2f\n",
                    best_str$tau, gamma_init, best_str$ssr))
    }
}, error = function(e) {
    cat(sprintf("[WARN] STR model failed: %s\n", e$message))
})

# =============================================================================
# 4. 阈值效应对称性检验 | Threshold Effect Symmetry Testing
# =============================================================================

cat("\n[STEP 4/5] Testing threshold effect symmetry...\n")

symmetry_results <- list()

# 使用单阈值模型 | Use single threshold model
tau_symmetry <- multiple_threshold_results$single$threshold

threshold_data_symmetry <- threshold_data %>%
    mutate(
        inst_distance = inst_capacity - tau_symmetry,
        inst_distance_abs = abs(inst_distance),
        inst_distance_signed = ifelse(inst_distance >= 0, inst_distance_abs, -inst_distance_abs),
        # 创建对称和非对称项 | Create symmetric and asymmetric terms
        ghe_symmetric = ghe_gdp * inst_distance_abs,
        ghe_asymmetric = ghe_gdp * inst_distance_signed
    )

pdata_symmetry <- pdata.frame(threshold_data_symmetry, index = c("iso3c", "year"))

# 对称模型 | Symmetric model
fe_symmetric <- plm(
    life_exp ~ ghe_gdp + ghe_symmetric + log_gdp_pc + log_population + time_trend,
    data = pdata_symmetry,
    model = "within",
    effect = "twoways"
)

# 非对称模型 | Asymmetric model
fe_asymmetric <- plm(
    life_exp ~ ghe_gdp + ghe_asymmetric + log_gdp_pc + log_population + time_trend,
    data = pdata_symmetry,
    model = "within",
    effect = "twoways"
)

# 比较模型 | Compare models
# 计算AIC和BIC | Calculate AIC and BIC
ssr_symmetric <- sum(residuals(fe_symmetric)^2)
ssr_asymmetric <- sum(residuals(fe_asymmetric)^2)
n_obs_sym <- length(residuals(fe_symmetric))
n_params_sym <- length(coef(fe_symmetric))
n_params_asym <- length(coef(fe_asymmetric))

aic_symmetric <- n_obs_sym * log(ssr_symmetric / n_obs_sym) + 2 * n_params_sym
aic_asymmetric <- n_obs_sym * log(ssr_asymmetric / n_obs_sym) + 2 * n_params_asym
bic_symmetric <- n_obs_sym * log(ssr_symmetric / n_obs_sym) + n_params_sym * log(n_obs_sym)
bic_asymmetric <- n_obs_sym * log(ssr_asymmetric / n_obs_sym) + n_params_asym * log(n_obs_sym)

symmetry_results$symmetric <- list(
    model = fe_symmetric,
    aic = aic_symmetric,
    bic = bic_symmetric
)

symmetry_results$asymmetric <- list(
    model = fe_asymmetric,
    aic = aic_asymmetric,
    bic = bic_asymmetric
)

cat(sprintf("[OK] Symmetric model: AIC = %.2f, BIC = %.2f\n", aic_symmetric, bic_symmetric))
cat(sprintf("[OK] Asymmetric model: AIC = %.2f, BIC = %.2f\n", aic_asymmetric, bic_asymmetric))

if (aic_asymmetric < aic_symmetric) {
    cat("[INFO] Asymmetric model preferred (lower AIC)\n")
} else {
    cat("[INFO] Symmetric model preferred (lower AIC)\n")
}

# =============================================================================
# 5. 保存结果 | Save Results
# =============================================================================

cat("\n[STEP 5/5] Saving results...\n")

all_results <- list(
    multiple_threshold = multiple_threshold_results,
    smooth_threshold = smooth_threshold_results,
    symmetry = symmetry_results,
    data_summary = list(
        total_obs = nrow(threshold_data),
        n_countries = n_distinct(threshold_data$iso3c),
        year_range = range(threshold_data$year, na.rm = TRUE)
    )
)

saveRDS(all_results, file.path("..", "results", "analysis", "16_nonlinear_threshold.rds"))
cat("[OK] Results saved to RDS file\n")

# 生成汇总表格 | Generate summary tables
# 多阈值检验表格 | Multiple threshold test table
multiple_table <- data.frame(
    Model = character(),
    N_Thresholds = integer(),
    Threshold_1 = numeric(),
    Threshold_2 = numeric(),
    Threshold_3 = numeric(),
    SSR = numeric(),
    AIC = numeric(),
    BIC = numeric(),
    stringsAsFactors = FALSE
)

if (!is.null(multiple_threshold_results$single)) {
    multiple_table <- rbind(multiple_table, data.frame(
        Model = "Single",
        N_Thresholds = 1,
        Threshold_1 = multiple_threshold_results$single$threshold,
        Threshold_2 = NA_real_,
        Threshold_3 = NA_real_,
        SSR = multiple_threshold_results$single$ssr,
        AIC = multiple_threshold_results$single$aic,
        BIC = multiple_threshold_results$single$bic
    ))
}

if (!is.null(multiple_threshold_results$double)) {
    multiple_table <- rbind(multiple_table, data.frame(
        Model = "Double",
        N_Thresholds = 2,
        Threshold_1 = multiple_threshold_results$double$tau1,
        Threshold_2 = multiple_threshold_results$double$tau2,
        Threshold_3 = NA_real_,
        SSR = multiple_threshold_results$double$ssr,
        AIC = multiple_threshold_results$double$aic,
        BIC = multiple_threshold_results$double$bic
    ))
}

if (!is.null(multiple_threshold_results$triple)) {
    multiple_table <- rbind(multiple_table, data.frame(
        Model = "Triple",
        N_Thresholds = 3,
        Threshold_1 = multiple_threshold_results$triple$tau1,
        Threshold_2 = multiple_threshold_results$triple$tau2,
        Threshold_3 = multiple_threshold_results$triple$tau3,
        SSR = multiple_threshold_results$triple$ssr,
        AIC = multiple_threshold_results$triple$aic,
        BIC = multiple_threshold_results$triple$bic
    ))
}

if (nrow(multiple_table) > 0) {
    write_csv(multiple_table, file.path("..", "tables", "Table_S16_Multiple_Threshold_Test.csv"))
    cat("[OK] Multiple threshold test table saved\n")
}

# 平滑阈值回归表格 | Smooth threshold regression table
if (!is.null(smooth_threshold_results$str)) {
    str_table <- data.frame(
        Threshold = smooth_threshold_results$str$tau,
        Gamma = gamma_init,
        SSR = smooth_threshold_results$str$ssr,
        stringsAsFactors = FALSE
    )
    
    write_csv(str_table, file.path("..", "tables", "Table_S17_Smooth_Threshold_Regression.csv"))
    cat("[OK] Smooth threshold regression table saved\n")
}

# 对称性检验表格 | Symmetry test table
symmetry_table <- data.frame(
    Model = c("Symmetric", "Asymmetric"),
    AIC = c(symmetry_results$symmetric$aic, symmetry_results$asymmetric$aic),
    BIC = c(symmetry_results$symmetric$bic, symmetry_results$asymmetric$bic),
    Preferred = c(aic_symmetric < aic_asymmetric, aic_asymmetric < aic_symmetric),
    stringsAsFactors = FALSE
)

write_csv(symmetry_table, file.path("..", "tables", "Table_S18_Threshold_Symmetry.csv"))
cat("[OK] Threshold symmetry table saved\n")

cat("\n=============================================================================\n")
cat("  Nonlinear Threshold Effects Analysis Complete\n")
cat("=============================================================================\n\n")

log_message("Nonlinear threshold effects analysis completed successfully")

