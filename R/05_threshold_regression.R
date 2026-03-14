# =============================================================================
# 04_threshold_regression.R
# Threshold Regression Analysis | 门槛回归分析
# =============================================================================
#
# 【功能概述 | Function Overview】
# 执行门槛回归分析，识别制度能力的关键阈值
# Perform threshold regression analysis to identify critical institutional capacity thresholds:
# - Hansen (1999) threshold estimation | Hansen门槛估计
# - Bootstrap confidence intervals | Bootstrap置信区间
# - Regime-specific effects | 不同区制的效应
#
# 【输入文件 | Input Files】
# - data/processed/02_integrated_panel_data.rds
# - results/analysis/03_fe_results.rds
#
# 【输出文件 | Output Files】
# - results/analysis/04_threshold_results.rds
# - results/analysis/04_threshold_estimates.csv
# - tables/Table_S3_Threshold_Analysis.csv
#
# 【运行时间 | Runtime】
# ~ 2-5 minutes (C++ accelerated, parallel bootstrap) | ~2-5分钟（C++加速版，并行Bootstrap）
# ~ 10-15 minutes (serial bootstrap with 1000 iterations) | ~10-15分钟（串行Bootstrap，1000次迭代）
#
# 【并行计算 | Parallel Computing】
# - Bootstrap计算已启用并行（使用所有可用CPU核心）
# - Bootstrap computation is parallelized (uses all available CPU cores)
# - 可通过环境变量BOOTSTRAP_ITERATIONS减少迭代次数（用于快速测试）
# - Can reduce iterations via BOOTSTRAP_ITERATIONS env var (for quick testing)
#
# 【最后更新 | Last Updated】
# 2025-12-18 (Parallel Bootstrap Optimization)
# =============================================================================

# 运行配置脚本 | Run configuration script
if (!exists("CONFIG")) {
    source("01_setup_and_configuration.R")
}

# =============================================================================
# 0. 编译和加载C++加速代码 | Compile and Load C++ Code
# =============================================================================

cat("\n[STEP 0/5] Loading C++ accelerated threshold functions...\n")

# 检查是否需要编译C++代码
if (!require("Rcpp", quietly = TRUE)) {
    stop("Rcpp package is required but not installed.")
}

cpp_file <- "threshold_cpp_enhanced.cpp"
if (file.exists(cpp_file)) {
    tryCatch(
        {
            Rcpp::sourceCpp(cpp_file)
            cat("[OK] C++ functions compiled and loaded successfully\n")
            cat("  Available functions:\n")
            cat("    - threshold_search_fe() - Fast threshold search with FE\n")
            cat("    - estimate_regime_effects_fe() - Regime effects estimation\n")
            cat("    - within_transformation() - Fixed effects transformation\n")
            use_cpp <- TRUE
        },
        error = function(e) {
            cat("[WARN] C++ compilation failed, falling back to R version\n")
            cat("  Error:", e$message, "\n")
            use_cpp <- FALSE
        }
    )
} else {
    cat("[WARN] C++ file not found, using R version\n")
    use_cpp <- FALSE
}

cat("\n")
cat("=============================================================================\n")
cat("  04 - Threshold Regression Analysis\n")
cat("=============================================================================\n\n")

log_message("Starting threshold regression analysis")

# =============================================================================
# 1. 加载数据和准备 | Load Data and Preparation
# =============================================================================

cat("\n[STEP 1/5] Loading data and preparing for threshold analysis...\n")

panel_data <- readRDS(file.path("..", "data", "processed", "02_integrated_panel_data.rds"))

# 准备门槛分析数据 | Prepare threshold analysis data
# Use government effectiveness as threshold variable (as specified in manuscript)
# 使用政府有效性作为阈值变量（与文稿中一致）
# Note: Data filtering should match the flowchart (N=4,337), which uses gdp_pc filter
# 注意：数据筛选应匹配流程图（N=4,337），流程图使用gdp_pc筛选
threshold_data <- panel_data %>%
    filter(!is.na(che_gdp), !is.na(ghe_gdp), !is.na(life_exp), !is.na(gdp_pc), !is.na(government_effectiveness)) %>%
    mutate(
        # 创建制度能力指标 | Create institutional capacity indicators
        # 使用政府有效性作为制度能力指标（与文稿一致）| Use government effectiveness as institutional capacity indicator (consistent with manuscript)
        inst_capacity = government_effectiveness,

        # 标准化变量 | Standardize variables
        ghe_gdp_std = scale(ghe_gdp)[, 1],
        life_exp_std = scale(life_exp)[, 1],

        # 创建时间固定效应 | Create time fixed effects
        year_factor = as.factor(year)
    ) %>%
    arrange(iso3c, year)

cat(sprintf("[OK] Threshold analysis data prepared: %d observations\n", nrow(threshold_data)))

# =============================================================================
# 2. Hansen门槛检验 | Hansen Threshold Test
# =============================================================================

cat("\n[STEP 2/5] Performing Hansen threshold test...\n")

# 选择使用C++或R版本 | Choose C++ or R version
if (exists("use_cpp") && use_cpp) {
    cat("[INFO] Using C++ accelerated threshold search...\n")

    # 准备数据用于C++函数
    y <- as.numeric(threshold_data$life_exp)
    x <- as.numeric(threshold_data$ghe_gdp)
    z <- as.numeric(threshold_data$inst_capacity)
    country_ids <- as.integer(as.factor(threshold_data$iso3c))
    time_ids <- as.integer(threshold_data$year - min(threshold_data$year) + 1)

    # 控制变量矩阵
    X_controls <- as.matrix(threshold_data %>% dplyr::select(log_population))

    # 设置网格搜索参数
    z_min <- quantile(z, 0.10, na.rm = TRUE)
    z_max <- quantile(z, 0.90, na.rm = TRUE)
    n_grid <- 100 # 网格点数

    cat(sprintf(
        "  Searching threshold in range [%.4f, %.4f] with %d grid points\n",
        z_min, z_max, n_grid
    ))

    # 使用C++快速搜索阈值
    start_time <- Sys.time()
    threshold_est <- threshold_search_fe(
        y = y,
        x = x,
        z = z,
        country_ids = country_ids,
        time_ids = time_ids,
        X_controls = X_controls,
        z_min = z_min,
        z_max = z_max,
        n_grid = n_grid
    )
    end_time <- Sys.time()

    cat(sprintf(
        "[OK] Threshold identified: %.4f (in %.2f seconds)\n",
        threshold_est, as.numeric(end_time - start_time, units = "secs")
    ))

    threshold_result <- list(
        threshold = threshold_est,
        se = NA, # 将在Bootstrap中计算
        success = TRUE,
        method = "cpp"
    )
    
    # Store z_min, z_max, n_grid for bootstrap
    # 保存z_min, z_max, n_grid用于bootstrap
    z_min <- z_min
    z_max <- z_max
    n_grid <- n_grid
} else {
    cat("[INFO] Using R version (segmented package)...\n")

    # 首先估计线性模型 | First estimate linear model
    linear_model <- lm(
        life_exp ~ ghe_gdp + inst_capacity + I(ghe_gdp * inst_capacity) +
            log_population + as.factor(year),
        data = threshold_data
    )

    cat("[OK] Linear model estimated\n")

    # 使用segmented包识别断点
    cat("[INFO] Searching for threshold in institutional capacity...\n")

    threshold_result <- tryCatch(
        {
            seg_model <- segmented(
                linear_model,
                seg.Z = ~inst_capacity,
                psi = median(threshold_data$inst_capacity, na.rm = TRUE)
            )

            threshold_est <- seg_model$psi[, "Est."]
            threshold_se <- seg_model$psi[, "St.Err"]

            cat(sprintf(
                "[OK] Threshold identified: %.4f (SE: %.4f)\n",
                threshold_est, threshold_se
            ))

            # Define z_min, z_max, n_grid for bootstrap (even when using R version)
            # 为Bootstrap定义z_min, z_max, n_grid（即使使用R版本）
            z_min <- quantile(threshold_data$inst_capacity, 0.10, na.rm = TRUE)
            z_max <- quantile(threshold_data$inst_capacity, 0.90, na.rm = TRUE)
            n_grid <- 100

            list(
                model = seg_model,
                threshold = threshold_est,
                se = threshold_se,
                success = TRUE,
                method = "segmented"
            )
        },
        error = function(e) {
            cat(sprintf("[WARN] Automatic threshold detection failed: %s\n", e$message))
            cat("[INFO] Using manual threshold search...\n")

            candidates <- quantile(threshold_data$inst_capacity,
                probs = c(0.25, 0.33, 0.50, 0.67, 0.75),
                na.rm = TRUE
            )

            # 计算每个候选阈值的SSR | Calculate SSR for each candidate threshold
            ssr_results <- map_dfr(candidates, function(tau) {
                # 创建区制指示变量 | Create regime indicator
                threshold_data_temp <- threshold_data %>%
                    mutate(
                        regime = ifelse(inst_capacity <= tau, "low", "high"),
                        ghe_low = ifelse(regime == "low", ghe_gdp, 0),
                        ghe_high = ifelse(regime == "high", ghe_gdp, 0)
                    )

                # 估计区制特定模型 | Estimate regime-specific model
                model_temp <- lm(
                    life_exp ~ ghe_low + ghe_high + inst_capacity + log_population + as.factor(year),
                    data = threshold_data_temp
                )

                tibble(
                    threshold = tau,
                    ssr = sum(resid(model_temp)^2),
                    aic = AIC(model_temp),
                    bic = BIC(model_temp)
                )
            })

            # 选择SSR最小的阈值 | Select threshold with minimum SSR
            best_threshold <- ssr_results %>%
                filter(ssr == min(ssr)) %>%
                pull(threshold) %>%
                first()

            cat(sprintf("[OK] Best threshold found: %.4f\n", best_threshold))

            # Define z_min, z_max, n_grid for bootstrap
            # 为Bootstrap定义z_min, z_max, n_grid
            z_min <- quantile(threshold_data$inst_capacity, 0.10, na.rm = TRUE)
            z_max <- quantile(threshold_data$inst_capacity, 0.90, na.rm = TRUE)
            n_grid <- 100

            list(
                model = NULL,
                threshold = best_threshold,
                se = NA,
                candidates = ssr_results,
                success = TRUE
            )
        }
    )
}

# =============================================================================
# 3. 区制特定效应估计 | Regime-Specific Effects Estimation
# =============================================================================

cat("\n[STEP 3/5] Estimating regime-specific effects...\n")

# 使用识别的阈值创建区制 | Create regimes using identified threshold
threshold_data <- threshold_data %>%
    mutate(
        regime = ifelse(inst_capacity <= threshold_result$threshold, "Low", "High"),
        regime_factor = as.factor(regime),

        # 创建区制特定的交互项 | Create regime-specific interactions
        ghe_low = ifelse(regime == "Low", ghe_gdp, 0),
        ghe_high = ifelse(regime == "High", ghe_gdp, 0)
    )

# 估计区制特定模型 | Estimate regime-specific model
regime_model <- lm(
    life_exp ~ ghe_low + ghe_high + inst_capacity + log_population +
        as.factor(year) + as.factor(iso3c),
    data = threshold_data
)

# 计算稳健标准误 | Calculate robust standard errors
regime_model_robust <- coeftest(regime_model, vcov = vcovHC(regime_model, type = "HC1"))

cat("[OK] Regime-specific model estimated\n")

# 提取区制特定系数 | Extract regime-specific coefficients
coef_low <- coef(regime_model)["ghe_low"]
coef_high <- coef(regime_model)["ghe_high"]
se_low <- sqrt(diag(vcovHC(regime_model, type = "HC1")))["ghe_low"]
se_high <- sqrt(diag(vcovHC(regime_model, type = "HC1")))["ghe_high"]

cat("\n[INFO] Regime-specific effects:\n")
cat(sprintf("  Low capacity regime:  %.4f (SE: %.4f)\n", coef_low, se_low))
cat(sprintf("  High capacity regime: %.4f (SE: %.4f)\n", coef_high, se_high))
cat(sprintf("  Difference:           %.4f\n", coef_high - coef_low))

# =============================================================================
# 4. Bootstrap置信区间 | Bootstrap Confidence Intervals
# =============================================================================

cat("\n[STEP 4/5] Computing bootstrap confidence intervals...\n")

# Ensure GLOBAL_SEED is available for reproducibility
# 确保GLOBAL_SEED可用于可重复性
if (!exists("GLOBAL_SEED")) {
    GLOBAL_SEED <- 20251024
    cat("[INFO] GLOBAL_SEED not found, using default: 20251024\n")
}
cat(sprintf("[INFO] Using random seed for bootstrap: GLOBAL_SEED = %d\n", GLOBAL_SEED))

# Bootstrap阈值估计 | Bootstrap threshold estimation
# Allow reducing iterations for faster testing (set via environment variable)
# 允许减少迭代次数以加快测试（通过环境变量设置）
n_boot <- as.integer(Sys.getenv("BOOTSTRAP_ITERATIONS", unset = CONFIG$bootstrap$n_iterations))
if (n_boot < CONFIG$bootstrap$n_iterations) {
    cat(sprintf("[INFO] Using reduced bootstrap iterations: %d (default: %d)\n", 
        n_boot, CONFIG$bootstrap$n_iterations))
    cat("[INFO] Set BOOTSTRAP_ITERATIONS environment variable to override\n")
}
boot_thresholds <- numeric(n_boot)

cat(sprintf("[INFO] Running %d bootstrap replications...\n", n_boot))
if (CONFIG$parallel$enabled) {
    cat(sprintf("[INFO] Using parallel computing with %d cores\n", getDoParWorkers()))
    estimated_time <- n_boot / getDoParWorkers() * 0.5  # Rough estimate: 0.5 sec per iteration
    cat(sprintf("[INFO] Estimated time: ~%.1f minutes\n", estimated_time / 60))
}

# 设置并行计算 | Setup parallel computing
# Note: Parallel cluster is closed after setup script, so we always recreate
# 注意：并行集群在setup脚本后被关闭，所以总是重新创建
if (CONFIG$parallel$enabled) {
    # Always create new cluster for bootstrap (setup script closes it)
    # 总是为Bootstrap创建新集群（setup脚本会关闭它）
    library(doParallel)
    cl_bootstrap <- makeCluster(CONFIG$parallel$n_cores)
    registerDoParallel(cl_bootstrap)
    cat(sprintf("[INFO] Parallel backend created for bootstrap (%d workers)\n", getDoParWorkers()))
    
    # Register cleanup
    # 注册清理函数
    on.exit({
        if (exists("cl_bootstrap")) {
            stopCluster(cl_bootstrap)
            cat("[INFO] Bootstrap parallel cluster closed\n")
        }
    }, add = TRUE)

    # 创建进度文件 | Create progress file
    progress_file <- "/tmp/bootstrap_progress.txt"
    writeLines("0", progress_file)

    cat(sprintf("[INFO] Bootstrap progress will be saved to: %s\n", progress_file))
    cat("[INFO] Monitor progress: watch -n 5 'cat /tmp/bootstrap_progress.txt'\n")

    # 根据是否使用C++选择不同的包
    required_packages <- if (exists("use_cpp") && use_cpp) {
        c("dplyr", "Rcpp")
    } else {
        c("dplyr", "segmented")
    }

    # Parallel bootstrap with progress tracking
    # 并行Bootstrap，带进度跟踪
    # Ensure z_min, z_max, n_grid are defined (should be set in threshold detection step)
    # 确保z_min, z_max, n_grid已定义（应在阈值检测步骤中设置）
    if (!exists("z_min") || !exists("z_max") || !exists("n_grid")) {
        cat("[WARN] z_min, z_max, n_grid not found, defining from threshold_data...\n")
        z_min <- quantile(threshold_data$inst_capacity, 0.10, na.rm = TRUE)
        z_max <- quantile(threshold_data$inst_capacity, 0.90, na.rm = TRUE)
        n_grid <- 100
    }
    
    # Prepare export list (only export threshold_search_fe if using C++)
    # 准备导出列表（仅在使用C++时导出threshold_search_fe）
    export_list <- c(
        "threshold_data", "use_cpp", "z_min", "z_max", "n_grid", 
        "threshold_result", "progress_file", "n_boot"
    )
    if (exists("use_cpp") && use_cpp && exists("threshold_search_fe")) {
        export_list <- c(export_list, "threshold_search_fe")
    }
    
    # Export all necessary variables and functions to parallel workers
    # 导出所有必要的变量和函数到并行workers
    # Set random seed for reproducibility in parallel bootstrap
    # 设置随机种子以确保并行Bootstrap的可重复性
    # Use .options.snow with preschedule=FALSE to ensure proper seed handling
    # 使用.options.snow和preschedule=FALSE确保正确的种子处理
    boot_results <- foreach(
        i = 1:n_boot, 
        .combine = rbind,
        .packages = required_packages,
        .export = c(export_list, "GLOBAL_SEED"),
        .options.snow = list(preschedule = FALSE)  # Better seed handling in parallel
    ) %dopar% {
        # Set seed for each iteration to ensure reproducibility
        # 为每次迭代设置种子以确保可重复性
        # Use iteration number + global seed to create unique but reproducible seeds
        # 使用迭代次数+全局种子创建唯一但可重复的种子
        set.seed(GLOBAL_SEED + i)
        
        # 重抽样 | Resample
        boot_sample <- threshold_data %>%
            group_by(iso3c) %>%
            slice_sample(prop = 1, replace = TRUE) %>%
            ungroup()

        # 估计Bootstrap阈值
        result <- tryCatch(
            {
                if (exists("use_cpp") && use_cpp) {
                    # C++版本：直接调用C++函数
                    boot_y <- as.numeric(boot_sample$life_exp)
                    boot_x <- as.numeric(boot_sample$ghe_gdp)
                    boot_z <- as.numeric(boot_sample$inst_capacity)
                    boot_country_ids <- as.integer(as.factor(boot_sample$iso3c))
                    boot_time_ids <- as.integer(boot_sample$year - min(boot_sample$year) + 1)
                    boot_X_controls <- as.matrix(boot_sample %>% dplyr::select(log_population))

                    boot_threshold <- threshold_search_fe(
                        y = boot_y,
                        x = boot_x,
                        z = boot_z,
                        country_ids = boot_country_ids,
                        time_ids = boot_time_ids,
                        X_controls = boot_X_controls,
                        z_min = z_min,
                        z_max = z_max,
                        n_grid = n_grid
                    )
                } else {
                    # R版本：使用segmented包
                    boot_linear <- lm(
                        life_exp ~ ghe_gdp + inst_capacity + I(ghe_gdp * inst_capacity) +
                            log_population + as.factor(year),
                        data = boot_sample
                    )

                    boot_seg <- segmented(
                        boot_linear,
                        seg.Z = ~inst_capacity,
                        psi = threshold_result$threshold
                    )
                    boot_threshold <- boot_seg$psi[, "Est."]
                }

                c(threshold = boot_threshold)
            },
            error = function(e) {
                c(threshold = NA)
            }
        )

        # 更新进度文件（每10次输出一次以减少I/O）
        # Update progress file (every 10 iterations to reduce I/O)
        if (i %% 10 == 0) {
            tryCatch(
                {
                    writeLines(
                        sprintf(
                            "Bootstrap: %d/%d (%.1f%%) - %s",
                            i, n_boot, i / n_boot * 100,
                            format(Sys.time(), "%H:%M:%S")
                        ),
                        progress_file
                    )
                },
                error = function(e) {}
            )
        }

        result
    }

    boot_thresholds <- boot_results[, "threshold"]
    boot_thresholds <- boot_thresholds[!is.na(boot_thresholds)]

    cat(sprintf(
        "[OK] Bootstrap completed: %d successful replications\n",
        length(boot_thresholds)
    ))
} else {
    # 串行Bootstrap | Serial bootstrap
    for (i in 1:n_boot) {
        if (i %% 100 == 0) cat(sprintf("  Progress: %d/%d\n", i, n_boot))

        # Set seed for each iteration to ensure reproducibility
        # 为每次迭代设置种子以确保可重复性
        # Use GLOBAL_SEED (same as parallel version) for consistency
        # 使用GLOBAL_SEED（与并行版本相同）以保持一致性
        set.seed(GLOBAL_SEED + i)

        boot_sample <- threshold_data %>%
            group_by(iso3c) %>%
            slice_sample(prop = 1, replace = TRUE) %>%
            ungroup()

        boot_thresholds[i] <- tryCatch(
            {
                boot_linear <- lm(
                    life_exp ~ ghe_gdp + inst_capacity + log_population + as.factor(year),
                    data = boot_sample
                )

                boot_seg <- segmented(boot_linear,
                    seg.Z = ~inst_capacity,
                    psi = threshold_result$threshold
                )

                boot_seg$psi[, "Est."]
            },
            error = function(e) NA
        )
    }

    boot_thresholds <- boot_thresholds[!is.na(boot_thresholds)]
}

# 计算置信区间 | Calculate confidence intervals
if (length(boot_thresholds) > 10) {
    ci_lower <- quantile(boot_thresholds, 0.025, na.rm = TRUE)
    ci_upper <- quantile(boot_thresholds, 0.975, na.rm = TRUE)

    cat(sprintf("[OK] 95%% Confidence Interval: [%.4f, %.4f]\n", ci_lower, ci_upper))
} else {
    ci_lower <- NA
    ci_upper <- NA
    cat("[WARN] Insufficient successful bootstrap replications for CI\n")
}

# =============================================================================
# 5. 保存结果 | Save Results
# =============================================================================

cat("\n[STEP 5/5] Saving threshold analysis results...\n")

# 保存完整结果对象 | Save complete results object
threshold_results_full <- list(
    threshold_estimate = threshold_result$threshold,
    threshold_se = threshold_result$se,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    regime_model = regime_model,
    regime_coefficients = list(
        low = c(coef = coef_low, se = se_low),
        high = c(coef = coef_high, se = se_high)
    ),
    boot_distribution = boot_thresholds,
    n_boot = length(boot_thresholds),
    data_summary = list(
        n_obs = nrow(threshold_data),
        n_low_regime = sum(threshold_data$regime == "Low"),
        n_high_regime = sum(threshold_data$regime == "High")
    )
)

saveRDS(
    threshold_results_full,
    file.path("..", "results", "analysis", "04_threshold_results.rds")
)
cat("[OK] Complete results saved\n")

# 创建阈值估计摘要表 | Create threshold estimates summary table
threshold_summary <- tibble(
    Statistic = c(
        "Threshold Estimate",
        "Standard Error",
        "95% CI Lower",
        "95% CI Upper",
        "Low Regime Effect (GHE)",
        "Low Regime SE",
        "High Regime Effect (GHE)",
        "High Regime SE",
        "Difference (High - Low)",
        "Observations",
        "Countries in Low Regime",
        "Countries in High Regime"
    ),
    Value = c(
        threshold_result$threshold,
        threshold_result$se,
        ci_lower,
        ci_upper,
        coef_low,
        se_low,
        coef_high,
        se_high,
        coef_high - coef_low,
        nrow(threshold_data),
        n_distinct(threshold_data$iso3c[threshold_data$regime == "Low"]),
        n_distinct(threshold_data$iso3c[threshold_data$regime == "High"])
    )
)

write_csv(
    threshold_summary,
    file.path("..", "results", "analysis", "04_threshold_estimates.csv")
)
cat("[OK] Threshold estimates table saved\n")

# 创建区制分类表（哪些国家属于哪个区制）| Create regime classification table
regime_classification <- threshold_data %>%
    group_by(iso3c, regime) %>%
    summarise(
        n_years = n(),
        avg_inst_capacity = mean(inst_capacity, na.rm = TRUE),
        avg_ghe = mean(ghe_gdp, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    arrange(regime, avg_inst_capacity)

write_csv(
    regime_classification,
    file.path("..", "results", "analysis", "04_regime_classification.csv")
)
cat("[OK] Regime classification table saved\n")

# =============================================================================
# 6. 打印结果摘要 | Print Results Summary
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("  Threshold Regression Results Summary\n")
cat("=============================================================================\n\n")

cat("[Threshold Estimate]\n")
cat(sprintf("  Point estimate: %.4f\n", threshold_result$threshold))
cat(sprintf("  Standard error: %.4f\n", threshold_result$se))
cat(sprintf("  95%% CI: [%.4f, %.4f]\n", ci_lower, ci_upper))
cat(sprintf("  (Based on %d bootstrap replications)\n\n", length(boot_thresholds)))

cat("[Regime-Specific Effects on Life Expectancy]\n")
cat(sprintf("  Low institutional capacity:  %.4f (SE: %.4f)\n", coef_low, se_low))
cat(sprintf("  High institutional capacity: %.4f (SE: %.4f)\n", coef_high, se_high))
cat(sprintf("  Difference:                  %.4f\n\n", coef_high - coef_low))

cat("[Sample Distribution]\n")
cat(sprintf("  Total observations: %d\n", nrow(threshold_data)))
cat(sprintf(
    "  Low regime:  %d obs (%d countries)\n",
    sum(threshold_data$regime == "Low"),
    n_distinct(threshold_data$iso3c[threshold_data$regime == "Low"])
))
cat(sprintf(
    "  High regime: %d obs (%d countries)\n\n",
    sum(threshold_data$regime == "High"),
    n_distinct(threshold_data$iso3c[threshold_data$regime == "High"])
))

# =============================================================================
# 7. 完成 | Complete
# =============================================================================

log_message("Threshold regression analysis completed successfully")

cat("\n")
cat("=============================================================================\n")
cat("  [OK] Script 04 completed successfully!\n")
cat("=============================================================================\n")
cat(sprintf("  - Threshold identified: %.4f\n", threshold_result$threshold))
cat(sprintf("  - Bootstrap replications: %d\n", length(boot_thresholds)))
cat("  - Results saved to: ../results/analysis/\n")
cat("\n")

log_message("Script 04 execution completed")
