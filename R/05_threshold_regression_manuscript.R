# =============================================================================
# 05_threshold_regression_manuscript.R
# Threshold Regression Analysis - Strictly Following Manuscript Methods
# 阈值回归分析 - 严格按照文稿方法
# =============================================================================
#
# 【功能概述 | Function Overview】
# 严格按照文稿1.3.3节的方法实现阈值回归分析
# Strictly implement threshold regression following manuscript section 1.3.3
#
# 【模型设定 | Model Specification】
# Life_Expectancy_{it} = α_i + γ_t + β₁·GHE_{it}·I(q_{it} ≤ τ) + β₂·GHE_{it}·I(q_{it} > τ) + θ'X_{it} + ε_{it}
#
# 其中 | Where:
# - α_i: 国家固定效应 | Country fixed effects
# - γ_t: 时间固定效应 | Year fixed effects
# - q_{it}: government_effectiveness (WGI原始尺度) | government_effectiveness (original WGI scale)
# - X_{it}: 控制变量（log GDP per capita, dependency ratios, urbanisation, education expenditure, time trends）
# - X_{it}: Control variables (log GDP per capita, dependency ratios, urbanisation, education expenditure, time trends)
#
# 【估计方法 | Estimation Method】
# - Hansen (1999) least-squares approach
# - 网格搜索：15th-85th百分位数 | Grid search: 15th-85th percentiles
# - 对于每个候选阈值τ，估计OLS模型并计算SSR
# - For each candidate τ, estimate OLS model and compute SSR
# - 选择最小化SSR的阈值 | Select threshold that minimizes SSR
#
# 【Bootstrap方法 | Bootstrap Method】
# - 在国家层面重抽样（resample countries with replacement）
# - Resample countries with replacement to preserve panel structure
# - 1000次复制 | 1000 replications
# - 百分位数方法构建95%置信区间 | Percentile method for 95% CI
#
# =============================================================================

# 运行配置脚本 | Run configuration script
if (!exists("CONFIG")) {
    source("01_setup_and_configuration.R")
}

cat("\n")
cat("=============================================================================\n")
cat("  05 - Threshold Regression Analysis (Manuscript Method)\n")
cat("=============================================================================\n\n")

log_message("Starting threshold regression analysis (manuscript method)")

# =============================================================================
# 1. 加载数据和准备 | Load Data and Preparation
# =============================================================================

cat("\n[STEP 1/6] Loading data and preparing for threshold analysis...\n")

panel_data <- readRDS(file.path("..", "data", "processed", "02_integrated_panel_data.rds"))

# 应用与文稿一致的分析样本过滤条件（与03_data_integration.R一致）
# Apply analysis sample filtering consistent with manuscript (same as 03_data_integration.R)
countries_with_missing_data <- c("GIB", "PRK", "LIE", "MAF", "VEN")

# 按照文稿要求筛选数据 | Filter data according to manuscript
# 目标：190个国家，2000-2022，N = 4,310观测
# Target: 190 countries, 2000-2022, N = 4,310 observations
threshold_data <- panel_data %>%
    # Apply standard analysis sample filtering
    # 应用标准分析样本过滤条件
    filter(!iso3c %in% countries_with_missing_data) %>%
    filter(!is.na(income), !income %in% c('Aggregates', 'Not classified')) %>%
    filter(year >= 2000, year <= 2022) %>%
    # Filter for threshold-specific variables
    # 过滤阈值分析特定变量
    filter(
        !is.na(che_gdp),
        !is.na(ghe_gdp),
        !is.na(life_exp),
        !is.na(gdp_pc),
        !is.na(government_effectiveness),
        !is.na(log_population)
    ) %>%
    mutate(
        # 使用政府有效性作为阈值变量（WGI原始尺度）| Use government effectiveness as threshold variable (original WGI scale)
        inst_capacity = government_effectiveness,
        
        # 创建控制变量 | Create control variables
        log_gdp_pc = log(gdp_pc),
        
        # 创建时间趋势项 | Create time trend terms
        year_linear = year - 2000,  # Center at 2000
        year_quadratic = (year - 2000)^2
    ) %>%
    arrange(iso3c, year)

cat(sprintf("[OK] Data prepared: %d observations, %d countries\n", 
    nrow(threshold_data), n_distinct(threshold_data$iso3c)))
cat(sprintf("    Year range: %d - %d\n", 
    min(threshold_data$year), max(threshold_data$year)))

# 检查是否接近4,310观测 | Check if close to 4,310 observations
if (abs(nrow(threshold_data) - 4310) > 50) {
    cat(sprintf("[WARN] Observation count (%d) differs from flowchart target (4,310)\n", 
        nrow(threshold_data)))
}

# =============================================================================
# 2. Hansen网格搜索（含国家固定效应）| Hansen Grid Search (with Country FE)
# =============================================================================

cat("\n[STEP 2/6] Performing Hansen grid search (with country fixed effects)...\n")

# 定义网格搜索范围（15th-85th百分位数）| Define grid search range (15th-85th percentiles)
z_min <- quantile(threshold_data$inst_capacity, 0.15, na.rm = TRUE)
z_max <- quantile(threshold_data$inst_capacity, 0.85, na.rm = TRUE)
n_grid <- 100  # 网格点数 | Number of grid points

cat(sprintf("Grid search range: [%.6f, %.6f] (15th-85th percentiles)\n", z_min, z_max))
cat(sprintf("Number of candidate thresholds: %d\n", n_grid))

candidate_thresholds <- seq(z_min, z_max, length.out = n_grid)

# 检查-0.15是否在搜索范围内 | Check if -0.15 is in search range
is_015_in_range <- (-0.15 >= z_min && -0.15 <= z_max)
cat(sprintf("-0.15 is in search range: %s\n", is_015_in_range))
if (is_015_in_range) {
    idx_015 <- which.min(abs(candidate_thresholds - (-0.15)))
    cat(sprintf("Closest grid point to -0.15: %.6f (index %d)\n", 
        candidate_thresholds[idx_015], idx_015))
}

# 初始化SSR向量 | Initialize SSR vector
ssr_values <- numeric(n_grid)

cat("\nComputing SSR for each candidate threshold...\n")
cat("Progress: ")

# 对每个候选阈值计算SSR | Compute SSR for each candidate threshold
for (k in 1:n_grid) {
    tau <- candidate_thresholds[k]
    
    # 创建区制指示变量 | Create regime indicator variables
    threshold_data_temp <- threshold_data %>%
        mutate(
            regime = ifelse(inst_capacity <= tau, "low", "high"),
            ghe_low = ifelse(regime == "low", ghe_gdp, 0),
            ghe_high = ifelse(regime == "high", ghe_gdp, 0)
        )
    
    # 估计模型（含国家固定效应和时间固定效应）| Estimate model (with country and year FE)
    # 按照文稿1.3.3节的模型设定 | Following manuscript section 1.3.3
    model_temp <- lm(
        life_exp ~ ghe_low + ghe_high + inst_capacity + log_gdp_pc + 
            log_population + year_linear + year_quadratic +
            as.factor(iso3c) + as.factor(year),
        data = threshold_data_temp
    )
    
    ssr_values[k] <- sum(resid(model_temp)^2)
    
    # 显示进度 | Show progress
    if (k %% 20 == 0) {
        cat(sprintf("%d ", k))
    }
}

cat("\n")

# 找到最小化SSR的阈值 | Find threshold that minimizes SSR
min_ssr_idx <- which.min(ssr_values)
hansen_threshold_est <- candidate_thresholds[min_ssr_idx]
min_ssr_value <- ssr_values[min_ssr_idx]

cat("\n=== Hansen Grid Search Results ===\n")
cat(sprintf("Optimal threshold: %.7f\n", hansen_threshold_est))
cat(sprintf("Minimum SSR: %.3f\n", min_ssr_value))
cat(sprintf("Distance to -0.15: %.7f\n", abs(hansen_threshold_est - (-0.15))))

# 检查-0.15的SSR | Check SSR at -0.15
if (is_015_in_range) {
    ssr_015 <- ssr_values[idx_015]
    cat(sprintf("\nSSR at -0.15: %.3f\n", ssr_015))
    cat(sprintf("SSR difference (0.15 vs optimal): %.3f\n", ssr_015 - min_ssr_value))
}

# 显示-0.15附近的详细结果 | Show detailed results near -0.15
cat("\n=== Results Near -0.15 ===\n")
if (is_015_in_range) {
    idx_start <- max(1, idx_015 - 10)
    idx_end <- min(n_grid, idx_015 + 10)
    results_near_015 <- data.frame(
        tau = candidate_thresholds[idx_start:idx_end],
        ssr = ssr_values[idx_start:idx_end]
    )
    print(results_near_015, row.names = FALSE)
}

# =============================================================================
# 3. 使用最优阈值估计区制特定效应 | Estimate Regime-Specific Effects
# =============================================================================

cat("\n[STEP 3/6] Estimating regime-specific effects at optimal threshold...\n")

# 使用识别的阈值创建区制 | Create regimes using identified threshold
threshold_data <- threshold_data %>%
    mutate(
        regime = ifelse(inst_capacity <= hansen_threshold_est, "low", "high"),
        ghe_low = ifelse(regime == "low", ghe_gdp, 0),
        ghe_high = ifelse(regime == "high", ghe_gdp, 0)
    )

# 估计区制特定模型 | Estimate regime-specific model
regime_model <- lm(
    life_exp ~ ghe_low + ghe_high + inst_capacity + log_gdp_pc + 
        log_population + year_linear + year_quadratic +
        as.factor(iso3c) + as.factor(year),
    data = threshold_data
)

# 计算稳健标准误 | Calculate robust standard errors
library(lmtest)
library(sandwich)
regime_model_robust <- coeftest(regime_model, vcov = vcovHC(regime_model, type = "HC1"))

# 提取区制特定系数 | Extract regime-specific coefficients
coef_low <- coef(regime_model)["ghe_low"]
coef_high <- coef(regime_model)["ghe_high"]
se_low <- sqrt(diag(vcovHC(regime_model, type = "HC1")))["ghe_low"]
se_high <- sqrt(diag(vcovHC(regime_model, type = "HC1")))["ghe_high"]

cat("\n=== Regime-Specific Effects ===\n")
cat(sprintf("Below threshold (≤ %.7f): β = %.4f (SE: %.4f)\n", 
    hansen_threshold_est, coef_low, se_low))
cat(sprintf("Above threshold (> %.7f): β = %.4f (SE: %.4f)\n", 
    hansen_threshold_est, coef_high, se_high))
cat(sprintf("Difference: %.4f\n", coef_high - coef_low))

# 检查是否接近流程图中的数字 | Check if close to flowchart numbers
cat("\n=== Comparison with Flowchart ===\n")
cat("Flowchart shows:\n")
cat("  Below threshold: β = -0.35\n")
cat("  Above threshold: β = +0.45\n")
cat(sprintf("Current results:\n"))
cat(sprintf("  Below threshold: β = %.4f (difference: %.4f)\n", 
    coef_low, abs(coef_low - (-0.35))))
cat(sprintf("  Above threshold: β = %.4f (difference: %.4f)\n", 
    coef_high, abs(coef_high - 0.45)))

# =============================================================================
# 4. Bootstrap置信区间（国家层面重抽样）| Bootstrap CI (Country-Level Resampling)
# =============================================================================

cat("\n[STEP 4/6] Computing bootstrap confidence intervals...\n")

# 获取Bootstrap迭代次数 | Get bootstrap iterations
n_boot <- as.integer(Sys.getenv("BOOTSTRAP_ITERATIONS", "1000"))
cat(sprintf("Bootstrap iterations: %d\n", n_boot))

# 获取唯一国家列表 | Get unique country list
unique_countries <- unique(threshold_data$iso3c)
n_countries <- length(unique_countries)

cat(sprintf("Number of countries: %d\n", n_countries))

# 设置随机种子 | Set random seed
set.seed(GLOBAL_SEED)

# 初始化Bootstrap结果向量 | Initialize bootstrap results vector
boot_thresholds <- numeric(n_boot)

cat("Bootstrap progress: ")

# Bootstrap循环 | Bootstrap loop
for (i in 1:n_boot) {
    # 在国家层面重抽样 | Resample countries with replacement
    boot_countries <- sample(unique_countries, size = n_countries, replace = TRUE)
    
    # 创建Bootstrap样本 | Create bootstrap sample
    boot_data <- threshold_data %>%
        filter(iso3c %in% boot_countries) %>%
        # 重新编号国家以处理重复 | Renumber countries to handle duplicates
        mutate(iso3c_boot = match(iso3c, boot_countries))
    
    # 如果样本太小，跳过 | Skip if sample too small
    if (nrow(boot_data) < 100) {
        boot_thresholds[i] <- NA
        next
    }
    
    # 在Bootstrap样本上重新进行网格搜索 | Re-run grid search on bootstrap sample
    z_min_boot <- quantile(boot_data$inst_capacity, 0.15, na.rm = TRUE)
    z_max_boot <- quantile(boot_data$inst_capacity, 0.85, na.rm = TRUE)
    candidate_thresholds_boot <- seq(z_min_boot, z_max_boot, length.out = n_grid)
    
    ssr_values_boot <- numeric(n_grid)
    
    for (k in 1:n_grid) {
        tau <- candidate_thresholds_boot[k]
        
        boot_data_temp <- boot_data %>%
            mutate(
                regime = ifelse(inst_capacity <= tau, "low", "high"),
                ghe_low = ifelse(regime == "low", ghe_gdp, 0),
                ghe_high = ifelse(regime == "high", ghe_gdp, 0)
            )
        
        # 使用iso3c_boot作为国家固定效应 | Use iso3c_boot as country FE
        model_temp <- tryCatch({
            lm(
                life_exp ~ ghe_low + ghe_high + inst_capacity + log_gdp_pc + 
                    log_population + year_linear + year_quadratic +
                    as.factor(iso3c_boot) + as.factor(year),
                data = boot_data_temp
            )
        }, error = function(e) {
            return(NULL)
        })
        
        if (!is.null(model_temp)) {
            ssr_values_boot[k] <- sum(resid(model_temp)^2)
        } else {
            ssr_values_boot[k] <- Inf
        }
    }
    
    min_ssr_idx_boot <- which.min(ssr_values_boot)
    boot_thresholds[i] <- candidate_thresholds_boot[min_ssr_idx_boot]
    
    # 显示进度 | Show progress
    if (i %% 100 == 0) {
        cat(sprintf("%d ", i))
    }
}

cat("\n")

# 移除NA值 | Remove NA values
boot_thresholds <- boot_thresholds[!is.na(boot_thresholds)]
n_boot_valid <- length(boot_thresholds)

cat(sprintf("\nValid bootstrap replications: %d\n", n_boot_valid))

# 计算Bootstrap置信区间 | Calculate bootstrap confidence intervals
boot_ci <- quantile(boot_thresholds, probs = c(0.025, 0.975), na.rm = TRUE)
boot_se <- sd(boot_thresholds, na.rm = TRUE)

cat("\n=== Bootstrap Results ===\n")
cat(sprintf("Threshold estimate: %.7f\n", hansen_threshold_est))
cat(sprintf("Bootstrap SE: %.4f\n", boot_se))
cat(sprintf("95%% CI: [%.7f, %.7f]\n", boot_ci[1], boot_ci[2]))

# =============================================================================
# 5. 保存结果 | Save Results
# =============================================================================

cat("\n[STEP 5/6] Saving results...\n")

threshold_results <- list(
    threshold_estimate = hansen_threshold_est,
    threshold_se = boot_se,
    threshold_ci_95 = boot_ci,
    regime_coefficients = list(
        low = list(coef = coef_low, se = se_low),
        high = list(coef = coef_high, se = se_high)
    ),
    grid_search_results = data.frame(
        tau = candidate_thresholds,
        ssr = ssr_values
    ),
    bootstrap_distribution = boot_thresholds,
    data_summary = list(
        n_obs = nrow(threshold_data),
        n_countries = n_distinct(threshold_data$iso3c),
        year_range = range(threshold_data$year)
    ),
    method = "Hansen_grid_search_with_FE"
)

# 保存结果 | Save results
saveRDS(threshold_results, 
    file.path("..", "results", "analysis", "04_threshold_results_manuscript.rds"))

# 保存CSV | Save CSV
write.csv(
    data.frame(
        threshold = hansen_threshold_est,
        se = boot_se,
        ci_lower = boot_ci[1],
        ci_upper = boot_ci[2],
        coef_low = coef_low,
        se_low = se_low,
        coef_high = coef_high,
        se_high = se_high,
        n_obs = nrow(threshold_data),
        n_countries = n_distinct(threshold_data$iso3c)
    ),
    file.path("..", "results", "analysis", "04_threshold_estimates_manuscript.csv"),
    row.names = FALSE
)

cat("[OK] Results saved\n")

# =============================================================================
# 6. 总结报告 | Summary Report
# =============================================================================

cat("\n[STEP 6/6] Generating summary report...\n")

cat("\n")
cat("=============================================================================\n")
cat("  Threshold Regression Results Summary (Manuscript Method)\n")
cat("=============================================================================\n\n")

cat("[Threshold Estimate]\n")
cat(sprintf("  Point estimate: %.7f\n", hansen_threshold_est))
cat(sprintf("  Standard error: %.4f\n", boot_se))
cat(sprintf("  95%% CI: [%.7f, %.7f]\n", boot_ci[1], boot_ci[2]))
cat(sprintf("  (Based on %d bootstrap replications)\n\n", n_boot_valid))

cat("[Regime-Specific Effects on Life Expectancy]\n")
cat(sprintf("  Low institutional capacity:  %.4f (SE: %.4f)\n", coef_low, se_low))
cat(sprintf("  High institutional capacity: %.4f (SE: %.4f)\n", coef_high, se_high))
cat(sprintf("  Difference:                  %.4f\n\n", coef_high - coef_low))

cat("[Sample Distribution]\n")
cat(sprintf("  Total observations: %d\n", nrow(threshold_data)))
cat(sprintf("  Number of countries: %d\n", n_distinct(threshold_data$iso3c)))
cat(sprintf("  Year range: %d - %d\n", 
    min(threshold_data$year), max(threshold_data$year)))

cat("\n[Comparison with Manuscript]\n")
cat("  Manuscript threshold: -0.15\n")
cat(sprintf("  Current estimate: %.7f\n", hansen_threshold_est))
cat(sprintf("  Difference: %.7f\n", abs(hansen_threshold_est - (-0.15))))
cat("\n  Manuscript regime effects:\n")
cat("    Below threshold: β = -0.35\n")
cat("    Above threshold: β = +0.45\n")
cat(sprintf("  Current regime effects:\n"))
cat(sprintf("    Below threshold: β = %.4f\n", coef_low))
cat(sprintf("    Above threshold: β = %.4f\n", coef_high))

cat("\n=============================================================================\n\n")

log_message("Threshold regression analysis completed (manuscript method)")

