# =============================================================================
# 05_ml_validation_ENHANCED.R
#
# 【功能概述 | Function Overview】
# Academic-standard machine learning validation system with comprehensive
# cross-validation, hyperparameter tuning, and robustness checks.
#
# 采用学术标准的机器学习验证系统，包含全面的交叉验证、超参数调优和稳健性检验。
#
# 【方法学框架 | Methodological Framework】
# - Theory-driven feature engineering (理论驱动的特征工程)
# - Nested cross-validation for unbiased performance estimation (嵌套交叉验证避免乐观偏倚)
# - Bootstrap validation for robust confidence intervals (Bootstrap验证提供稳健置信区间)
# - Multi-algorithm comparison (多算法对比)
# - Parallel computing for efficiency (并行计算提高效率)
#
# 【核心算法 | Core Algorithms】
# 1. Random Forest - Ensemble learning (随机森林 - 集成学习)
# 2. XGBoost - Gradient boosting (极限梯度提升)
# 3. GBM - Stochastic gradient boosting (随机梯度提升)
# 4. SVM - Support vector machine (支持向量机)
# 5. Elastic Net - Regularized regression (正则化回归)
# 6. Linear Model - Baseline (线性模型 - 基准线)
#
# 【验证策略 | Validation Strategy】
# - Geographic split to avoid data leakage (地理分割避免数据泄露)
# - 10-fold CV × 5 repeats = 50 validations (10折CV×5次重复=50次验证)
# - Nested CV: 5 outer × 5 inner folds (嵌套CV: 5外层×5内层折叠)
# - 1000 bootstrap resamples (1000次Bootstrap重采样)
# - Comprehensive marginal effects estimation (全面的边际效应估计)
#
# 【最后更新 | Last Updated】
# 2025-11-06
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("  05 - Enhanced Machine Learning Validation (Academic Standard)\n")
cat("=============================================================================\n\n")

# Fix OpenMP library conflict | 修复OpenMP库冲突
# This is a workaround for multiple OpenMP runtime libraries
# 这是多个OpenMP运行时库的临时解决方案
Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")

# Load global configuration
source("01_setup_and_configuration.R")

# Load additional ML-specific packages | 加载额外的ML专用包
if (!require("caret", quietly = TRUE)) install.packages("caret")
if (!require("ranger", quietly = TRUE)) install.packages("ranger")
library(caret)
library(ranger)

# =============================================================================
# 1. 设置并行计算环境 | Setup Parallel Computing Environment
# =============================================================================

cat("\n[STEP 1/9] Setting up parallel computing environment...\n")

# Stop any existing parallel backend to avoid conflicts
# 停止任何现有的并行后端以避免冲突
tryCatch({
    if (exists("cl")) {
        tryCatch(stopCluster(cl), error = function(e) {})
        rm(cl)
    }
    foreach::registerDoSEQ()
}, error = function(e) {
    # Ignore if no cluster exists
})

# Detect available cores
n_cores <- max(1, detectCores() - 1)
cat(sprintf("[INFO] Using %d CPU cores for parallel computing\n", n_cores))

# Create and register parallel cluster (will be recreated in training function if needed)
# 创建并注册并行集群（如果需要，将在训练函数中重新创建）
# Note: We don't create cluster here because 01_setup_and_configuration.R's on.exit will close it
# 注意：我们不在这里创建集群，因为01_setup_and_configuration.R的on.exit会关闭它
cat("[INFO] Parallel cluster will be created when needed during model training\n")

# =============================================================================
# 2. 加载和准备数据 | Load and Prepare Data
# =============================================================================

cat("\n[STEP 2/9] Loading and preparing data...\n")

# Load integrated panel data
panel_data <- readRDS(file.path("..", "data", "processed", "02_integrated_panel_data.rds"))

# Apply analysis sample filtering consistent with manuscript (same as 03_data_integration.R)
# 应用与文稿一致的分析样本过滤条件（与03_data_integration.R一致）
countries_with_missing_data <- c("GIB", "PRK", "LIE", "MAF", "VEN")

# Theory-driven feature engineering | 理论驱动的特征工程
# Based on economic theory and empirical evidence
ml_data_temp <- panel_data %>%
    # Apply standard analysis sample filtering
    # 应用标准分析样本过滤条件
    filter(!iso3c %in% countries_with_missing_data) %>%
    filter(!is.na(income), !income %in% c('Aggregates', 'Not classified')) %>%
    filter(year >= 2000, year <= 2022) %>%
    # Filter for ML-specific variables
    # 过滤ML特定变量
    filter(
        !is.na(ghe_gdp),
        !is.na(life_exp),
        !is.na(gdp_pc),
        !is.na(population)
    )

# Add engineered features
ml_data <- ml_data_temp %>%
    mutate(
        # 1. Core interaction terms - strong economic theory support
        # 核心交互项 - 有强经济学理论支持
        ghe_gdp_interaction = ghe_gdp * log(gdp_pc + 1),

        # 2. Moderate non-linearity - diminishing marginal returns
        # 适度非线性 - 边际效应递减
        ghe_squared = ghe_gdp^2,

        # 3. Time trend - control for technological progress
        # 时间趋势 - 控制技术进步
        year_trend = year - 2000,

        # 4. Income group effects - development stage differences
        # 收入组效应 - 发展阶段差异
        high_income = ifelse(income == "High income", 1, 0),
        middle_income = ifelse(str_detect(income, "middle"), 1, 0),

        # 5. Health system structure - policy relevant
        # 健康系统结构 - 政策相关
        ghe_total_ratio = ghe_gdp / (che_gdp + 0.01),

        # 6. Basic development level - control variable
        # 基础发展水平 - 控制变量
        mortality_burden = log(infant_mort + 1),

        # 7. Population scale effects - economies of scale
        # 人口规模效应 - 规模经济
        log_pop = log(population + 1),
        population_scale = ifelse(population > 50000000, 1, 0)
    )

# Select relevant columns
ml_data <- ml_data[, c(
    "life_exp", "ghe_gdp", "gdp_pc", "population", "che_gdp", "infant_mort",
    "ghe_gdp_interaction", "ghe_squared", "year_trend",
    "high_income", "middle_income", "ghe_total_ratio",
    "mortality_burden", "log_pop", "population_scale", "iso3c"
)]

# Remove incomplete cases
ml_data <- ml_data[complete.cases(ml_data), ]

cat(sprintf(
    "[OK] ML dataset prepared: %d observations, %d features\n",
    nrow(ml_data),
    ncol(ml_data) - 2 # Exclude dependent variable and country ID
))

# =============================================================================
# 3. 地理分割数据 | Geographic Data Split
# =============================================================================

cat("\n[STEP 3/9] Splitting data by country (geographic split)...\n")

# Split by country to avoid data leakage
# 按国家分割避免数据泄露
reset_seed()
unique_countries <- unique(ml_data$iso3c)
n_countries <- length(unique_countries)
train_countries <- sample(unique_countries, floor(0.8 * n_countries))

train_data <- ml_data %>% filter(iso3c %in% train_countries)
test_data <- ml_data %>% filter(!iso3c %in% train_countries)

cat(sprintf(
    "[OK] Training set: %d observations from %d countries\n",
    nrow(train_data),
    length(train_countries)
))
cat(sprintf(
    "[OK] Test set: %d observations from %d countries\n",
    nrow(test_data),
    n_countries - length(train_countries)
))

# Prepare feature matrices | 准备特征矩阵
feature_cols <- setdiff(names(ml_data), c("life_exp", "iso3c"))
X_train <- train_data[, feature_cols]
y_train <- train_data$life_exp
X_test <- test_data[, feature_cols]
y_test <- test_data$life_exp

# =============================================================================
# 4. 配置交叉验证策略 | Configure Cross-Validation Strategy
# =============================================================================

cat("\n[STEP 4/9] Configuring cross-validation strategies...\n")

# Main CV control for hyperparameter selection
# 主要CV控制用于超参数选择
cv_control <- trainControl(
    method = "repeatedcv", # Repeated cross-validation
    number = 10, # 10 folds
    repeats = 5, # 5 repeats
    verboseIter = FALSE, # Reduce output
    allowParallel = TRUE, # Enable parallel computing
    savePredictions = "final",
    summaryFunction = defaultSummary,
    returnResamp = "all" # Save all resampling results
)

cat("[OK] Main CV strategy: 10-fold CV x 5 repeats = 50 validations\n")

# Bootstrap control for confidence intervals
# Bootstrap控制用于置信区间
bootstrap_control <- trainControl(
    method = "boot", # Bootstrap resampling
    number = 1000, # 1000 bootstrap samples
    verboseIter = FALSE,
    allowParallel = TRUE,
    savePredictions = "final",
    returnResamp = "all"
)

cat("[OK] Bootstrap strategy: 1000 resamples for robust CI estimation\n")

# =============================================================================
# 5. 嵌套交叉验证函数 | Nested Cross-Validation Function
# =============================================================================

cat("\n[INFO] Defining nested CV function for unbiased performance estimation...\n")

nested_cv_evaluation <- function(X, y, method, tune_grid, n_outer = 5, n_inner = 5) {
    # Outer CV: evaluate generalization performance
    # Inner CV: select hyperparameters
    # 外层CV: 评估泛化性能
    # 内层CV: 选择超参数

    reset_seed()
    n_obs <- nrow(X)
    fold_ids <- sample(rep(1:n_outer, length.out = n_obs))

    outer_scores <- numeric(n_outer)

    for (fold in 1:n_outer) {
        # Outer split
        test_idx <- which(fold_ids == fold)
        train_idx <- which(fold_ids != fold)

        X_outer_train <- X[train_idx, ]
        y_outer_train <- y[train_idx]
        X_outer_test <- X[test_idx, ]
        y_outer_test <- y[test_idx]

        # Inner CV for hyperparameter selection
        inner_control <- trainControl(
            method = "cv",
            number = n_inner,
            verboseIter = FALSE,
            allowParallel = TRUE
        )

        # Train model
        model <- train(
            x = X_outer_train,
            y = y_outer_train,
            method = method,
            trControl = inner_control,
            tuneGrid = tune_grid,
            verbose = FALSE
        )

        # Evaluate on outer test set
        pred <- predict(model, X_outer_test)
        outer_scores[fold] <- sqrt(mean((y_outer_test - pred)^2))
    }

    return(list(
        mean_rmse = mean(outer_scores),
        sd_rmse = sd(outer_scores),
        all_scores = outer_scores
    ))
}

cat("[OK] Nested CV function defined\n")

# =============================================================================
# 6. Bootstrap置信区间函数 | Bootstrap Confidence Interval Function
# =============================================================================

bootstrap_confidence_interval <- function(model, X_test, y_test, n_boot = 1000) {
    # Calculate bootstrap confidence intervals for RMSE
    # 计算RMSE的Bootstrap置信区间

    reset_seed()
    n_obs <- nrow(X_test)
    boot_rmse <- numeric(n_boot)

    for (i in 1:n_boot) {
        # Bootstrap resample
        boot_idx <- sample(1:n_obs, n_obs, replace = TRUE)
        X_boot <- X_test[boot_idx, ]
        y_boot <- y_test[boot_idx]

        # Predict and evaluate
        pred_boot <- predict(model, X_boot)
        boot_rmse[i] <- sqrt(mean((y_boot - pred_boot)^2))
    }

    return(list(
        mean_rmse = mean(boot_rmse),
        ci_lower = quantile(boot_rmse, 0.025),
        ci_upper = quantile(boot_rmse, 0.975),
        se = sd(boot_rmse)
    ))
}

cat("[OK] Bootstrap CI function defined\n")

# =============================================================================
# 7. 边际效应计算函数 | Marginal Effects Calculation Functions
# =============================================================================

# Standard numerical differentiation
# 标准数值微分
calculate_marginal_effect_standard <- function(model, X_data, predict_func = predict, delta = 1) {
    base_pred <- predict_func(model, X_data)

    X_modified <- X_data
    X_modified$ghe_gdp <- X_modified$ghe_gdp + delta

    # Update related features | 更新相关特征
    if ("ghe_gdp_interaction" %in% names(X_modified)) {
        X_modified$ghe_gdp_interaction <- X_modified$ghe_gdp * log(X_modified$gdp_pc + 1)
    }
    if ("ghe_squared" %in% names(X_modified)) {
        X_modified$ghe_squared <- X_modified$ghe_gdp^2
    }
    if ("ghe_total_ratio" %in% names(X_modified)) {
        X_modified$ghe_total_ratio <- X_modified$ghe_gdp / (X_modified$che_gdp + 0.01)
    }

    modified_pred <- predict_func(model, X_modified)
    marginal_effect <- mean(modified_pred - base_pred, na.rm = TRUE)

    return(marginal_effect)
}

# Central difference method
# 中心差分法
calculate_marginal_effect_central_diff <- function(model, X_data, predict_func = predict, delta = 0.1) {
    # Upward perturbation | 向上扰动
    X_plus <- X_data
    X_plus$ghe_gdp <- X_plus$ghe_gdp + delta
    if ("ghe_gdp_interaction" %in% names(X_plus)) {
        X_plus$ghe_gdp_interaction <- X_plus$ghe_gdp * log(X_plus$gdp_pc + 1)
    }
    if ("ghe_squared" %in% names(X_plus)) {
        X_plus$ghe_squared <- X_plus$ghe_gdp^2
    }

    # Downward perturbation | 向下扰动
    X_minus <- X_data
    X_minus$ghe_gdp <- pmax(X_minus$ghe_gdp - delta, 0.01)
    if ("ghe_gdp_interaction" %in% names(X_minus)) {
        X_minus$ghe_gdp_interaction <- X_minus$ghe_gdp * log(X_minus$gdp_pc + 1)
    }
    if ("ghe_squared" %in% names(X_minus)) {
        X_minus$ghe_squared <- X_minus$ghe_gdp^2
    }

    pred_plus <- predict_func(model, X_plus)
    pred_minus <- predict_func(model, X_minus)

    # Central difference | 中心差分
    marginal_effect <- mean(pred_plus - pred_minus, na.rm = TRUE) / (2 * delta)

    return(marginal_effect)
}

# Bootstrap confidence interval for marginal effects
# 边际效应的Bootstrap置信区间
calculate_marginal_effect_with_ci <- function(model, X_data, predict_func = predict,
                                              delta = 1, n_bootstrap = 100, alpha = 0.05) {
    # Main effect | 主效应
    main_effect <- calculate_marginal_effect_standard(model, X_data, predict_func, delta)

    # Bootstrap resampling | Bootstrap重采样
    n_obs <- nrow(X_data)
    bootstrap_effects <- numeric(n_bootstrap)

    for (i in 1:n_bootstrap) {
        boot_indices <- sample(1:n_obs, n_obs, replace = TRUE)
        X_boot <- X_data[boot_indices, ]
        bootstrap_effects[i] <- calculate_marginal_effect_standard(model, X_boot, predict_func, delta)
    }

    # Calculate confidence interval | 计算置信区间
    ci_lower <- quantile(bootstrap_effects, alpha / 2, na.rm = TRUE)
    ci_upper <- quantile(bootstrap_effects, 1 - alpha / 2, na.rm = TRUE)

    return(list(
        marginal_effect = main_effect,
        ci_lower = ci_lower,
        ci_upper = ci_upper,
        bootstrap_effects = bootstrap_effects,
        se = sd(bootstrap_effects, na.rm = TRUE)
    ))
}

# Comprehensive marginal effects analysis
# 综合边际效应分析
comprehensive_marginal_effects <- function(model, X_data, predict_func = predict) {
    # 1. Standard method | 标准方法
    standard <- calculate_marginal_effect_standard(model, X_data, predict_func, delta = 1)

    # 2. Small delta method | 小增量方法
    small_delta <- calculate_marginal_effect_standard(model, X_data, predict_func, delta = 0.1) * 10

    # 3. Central difference method | 中心差分法
    central_diff <- calculate_marginal_effect_central_diff(model, X_data, predict_func, delta = 0.1)

    # 4. With confidence interval | 带置信区间
    with_ci <- calculate_marginal_effect_with_ci(model, X_data, predict_func, n_bootstrap = 100)

    return(list(
        standard_method = standard,
        small_delta_method = small_delta,
        central_diff_method = central_diff,
        with_confidence_interval = with_ci
    ))
}

cat("[OK] Marginal effects functions defined\n")

# =============================================================================
# 8. 通用训练函数 | Generic Training Function
# =============================================================================

cat("\n[STEP 5/9] Defining enhanced model training function...\n")

train_enhanced_model <- function(model_name, method, tune_grid, preprocess = NULL, disable_parallel = FALSE, ...) {
    cat(sprintf("\n[INFO] Training %s model...\n", model_name))
    
    # Ensure parallel cluster is active before training
    # 确保在训练前并行集群是活动的
    if (!disable_parallel) {
        tryCatch({
            # Check if cluster is registered
            if (!getDoParRegistered()) {
                cat("[INFO] Re-creating parallel cluster for model training...\n")
                if (exists("cl")) {
                    tryCatch(stopCluster(cl), error = function(e) {})
                }
                n_cores <- max(1, detectCores() - 1)
                cl <<- makeCluster(n_cores)
                registerDoParallel(cl)
                cat("[OK] Parallel cluster re-initialized\n")
            }
        }, error = function(e) {
            cat("[WARN] Parallel cluster issue, continuing with sequential processing\n")
            foreach::registerDoSEQ()
        })
    }
    
    reset_seed()

    cat(sprintf("[INFO] Grid search: %d combinations\n", nrow(tune_grid)))

    # For XGBoost, temporarily disable parallel to avoid OpenMP conflict
    # 对于XGBoost，暂时禁用并行以避免OpenMP冲突
    if (disable_parallel) {
        # Create non-parallel control for this model
        # 为此模型创建非并行控制
        cv_control_single <- cv_control
        cv_control_single$allowParallel <- FALSE
        tr_control <- cv_control_single
    } else {
        tr_control <- cv_control
    }

    # Main model training | 主模型训练
    model <- train(
        x = X_train,
        y = y_train,
        method = method,
        trControl = tr_control,
        tuneGrid = tune_grid,
        preProcess = preprocess,
        ...
    )

    # Test set prediction | 测试集预测
    pred <- predict(model, X_test)
    test_rmse <- sqrt(mean((y_test - pred)^2))
    test_r2 <- cor(y_test, pred)^2

    cat(sprintf("[OK] %s - Test RMSE: %.4f, R²: %.4f\n", model_name, test_rmse, test_r2))

    # Nested cross-validation | 嵌套交叉验证
    cat(sprintf("[INFO] Running nested CV for %s...\n", model_name))
    nested_cv <- nested_cv_evaluation(X_train, y_train, method, tune_grid, n_outer = 5, n_inner = 5)
    cat(sprintf("[OK] Nested CV RMSE: %.4f ± %.4f\n", nested_cv$mean_rmse, nested_cv$sd_rmse))

    # Bootstrap confidence interval | Bootstrap置信区间
    cat(sprintf("[INFO] Computing bootstrap CI for %s...\n", model_name))
    bootstrap_ci <- bootstrap_confidence_interval(model, X_test, y_test, n_boot = 1000)
    cat(sprintf(
        "[OK] Bootstrap RMSE: %.4f [%.4f, %.4f]\n",
        bootstrap_ci$mean_rmse,
        bootstrap_ci$ci_lower,
        bootstrap_ci$ci_upper
    ))

    # Marginal effects analysis | 边际效应分析
    cat(sprintf("[INFO] Computing marginal effects for %s...\n", model_name))
    marginal_comprehensive <- comprehensive_marginal_effects(model, X_test)
    cat(sprintf(
        "[OK] Marginal effect: %.4f years per 1%% GDP\n",
        marginal_comprehensive$standard_method
    ))

    return(list(
        model = model,
        predictions = pred,
        test_rmse = test_rmse,
        test_r2 = test_r2,
        nested_cv_results = nested_cv,
        bootstrap_ci = bootstrap_ci,
        marginal_effects_comprehensive = marginal_comprehensive,
        best_tune = model$bestTune,
        cv_results = model$resample
    ))
}

cat("[OK] Enhanced training function defined\n")

# =============================================================================
# 9. 训练所有模型 | Train All Models
# =============================================================================

cat("\n[STEP 6/9] Training all ML models with enhanced validation...\n")
cat("[INFO] This may take several minutes depending on your hardware...\n\n")

ml_results <- list()

# 1. Random Forest | 随机森林
rf_grid <- expand.grid(
    mtry = c(3, 5, 8),
    splitrule = "variance",
    min.node.size = c(5, 10)
)

ml_results$rf <- train_enhanced_model(
    "Random Forest",
    "ranger",
    rf_grid,
    importance = "impurity",
    num.trees = 500,
    verbose = FALSE
)

# 2. XGBoost | 极限梯度提升
# Note: Set nthread=1 to avoid OpenMP conflict with caret's parallel processing
# 注意：设置nthread=1以避免与caret并行处理的OpenMP冲突
xgb_grid <- expand.grid(
    nrounds = c(100, 300),
    max_depth = c(3, 6),
    eta = c(0.1, 0.3),
    gamma = 0,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    subsample = 0.8
)

# For XGBoost, try to train with parallel disabled to avoid OpenMP conflict
# 对于XGBoost，尝试禁用并行训练以避免OpenMP冲突
# Note: XGBoost has known OpenMP conflicts on some systems
# 注意：XGBoost在某些系统上有已知的OpenMP冲突
cat("[INFO] Attempting to train XGBoost with parallel disabled...\n")
tryCatch({
    ml_results$xgb <- train_enhanced_model(
        "XGBoost",
        "xgbTree",
        xgb_grid,
        disable_parallel = TRUE,  # 禁用并行以避免OpenMP冲突
        nthread = 1,  # 设置单线程
        verbose = FALSE
    )
    cat("[OK] XGBoost model trained successfully\n")
}, error = function(e) {
    cat("[WARN] XGBoost training failed: ", e$message, "\n")
    cat("[INFO] This is a known issue on macOS with multiple OpenMP runtimes\n")
    cat("[INFO] Continuing with other models...\n")
    ml_results$xgb <- NULL
})

# 3. Gradient Boosting Machine | 梯度提升机
gbm_grid <- expand.grid(
    interaction.depth = c(1, 3),
    n.trees = c(100, 300),
    shrinkage = c(0.1, 0.3),
    n.minobsinnode = c(5, 10)
)

ml_results$gbm <- train_enhanced_model(
    "GBM",
    "gbm",
    gbm_grid,
    verbose = FALSE
)

# 4. Support Vector Machine | 支持向量机
svm_grid <- expand.grid(
    sigma = c(0.01, 0.1),
    C = c(1, 10)
)

ml_results$svm <- train_enhanced_model(
    "SVM",
    "svmRadial",
    svm_grid,
    preprocess = c("center", "scale")
)

# 5. Elastic Net | 弹性网络
glmnet_grid <- expand.grid(
    alpha = c(0.5, 1.0),
    lambda = c(0.01, 0.1, 1)
)

ml_results$glmnet <- train_enhanced_model(
    "Elastic Net",
    "glmnet",
    glmnet_grid,
    preprocess = c("center", "scale")
)

# 6. Linear Model | 线性模型 (baseline)
lm_grid <- expand.grid(intercept = TRUE)

ml_results$lm <- train_enhanced_model(
    "Linear Model",
    "lm",
    lm_grid,
    preprocess = c("center", "scale")
)

# =============================================================================
# 10. 提取和汇总结果 | Extract and Summarize Results
# =============================================================================

cat("\n[STEP 7/9] Extracting and summarizing results...\n")

# Extract marginal effects | 提取边际效应
marginal_effects <- sapply(ml_results, function(x) {
    x$marginal_effects_comprehensive$standard_method
})

# Extract performance metrics | 提取性能指标
test_rmse <- sapply(ml_results, function(x) x$test_rmse)
test_r2 <- sapply(ml_results, function(x) x$test_r2)
nested_cv_rmse <- sapply(ml_results, function(x) x$nested_cv_results$mean_rmse)
nested_cv_sd <- sapply(ml_results, function(x) x$nested_cv_results$sd_rmse)
bootstrap_rmse <- sapply(ml_results, function(x) x$bootstrap_ci$mean_rmse)
bootstrap_ci_lower <- sapply(ml_results, function(x) x$bootstrap_ci$ci_lower)
bootstrap_ci_upper <- sapply(ml_results, function(x) x$bootstrap_ci$ci_upper)

# Extract marginal effects CI | 提取边际效应置信区间
marginal_ci_lower <- sapply(ml_results, function(x) {
    x$marginal_effects_comprehensive$with_confidence_interval$ci_lower
})
marginal_ci_upper <- sapply(ml_results, function(x) {
    x$marginal_effects_comprehensive$with_confidence_interval$ci_upper
})

cat("[OK] Results extracted\n")

# =============================================================================
# 11. 保存结果 | Save Results
# =============================================================================

cat("\n[STEP 8/9] Saving results...\n")

# Comprehensive results object | 综合结果对象
final_ml_results <- list(
    # Individual model results | 单个模型结果
    individual_results = ml_results,

    # Summary statistics | 汇总统计
    summary_stats = list(
        sample_size = nrow(ml_data),
        train_size = nrow(train_data),
        test_size = nrow(test_data),
        n_features = length(feature_cols),
        n_countries = n_countries,
        train_countries = length(train_countries),
        test_countries = n_countries - length(train_countries),
        cv_method = "10-fold CV x 5 repeats (50 validations)",
        nested_cv_method = "5 outer folds x 5 inner folds",
        bootstrap_method = "1000 bootstrap resamples",
        feature_engineering = "theory-driven academic standard",
        n_cores_used = n_cores
    ),

    # Marginal effects | 边际效应
    marginal_effects = marginal_effects,
    marginal_ci_lower = marginal_ci_lower,
    marginal_ci_upper = marginal_ci_upper,

    # Performance metrics | 性能指标
    performance_metrics = list(
        test_rmse = test_rmse,
        test_r2 = test_r2,
        nested_cv_rmse = nested_cv_rmse,
        nested_cv_sd = nested_cv_sd,
        bootstrap_rmse = bootstrap_rmse,
        bootstrap_ci_lower = bootstrap_ci_lower,
        bootstrap_ci_upper = bootstrap_ci_upper
    ),

    # Best parameters | 最佳参数
    best_parameters = lapply(ml_results, function(x) x$best_tune)
)

# Save RDS file | 保存RDS文件
saveRDS(
    final_ml_results,
    file.path("..", "results", "analysis", "05_ml_results.rds")
)
cat("[OK] Complete results saved (RDS format)\n")

# Create summary table | 创建汇总表
ml_summary_table <- tibble(
    Model = toupper(names(ml_results)),
    Test_RMSE = test_rmse,
    Test_R2 = test_r2,
    NestedCV_RMSE = nested_cv_rmse,
    NestedCV_SD = nested_cv_sd,
    Bootstrap_RMSE = bootstrap_rmse,
    Bootstrap_CI_Lower = bootstrap_ci_lower,
    Bootstrap_CI_Upper = bootstrap_ci_upper,
    Marginal_Effect = marginal_effects,
    Marginal_CI_Lower = marginal_ci_lower,
    Marginal_CI_Upper = marginal_ci_upper
)

write_csv(
    ml_summary_table,
    file.path("..", "results", "analysis", "05_ml_summary.csv")
)
cat("[OK] Summary table saved (CSV format)\n")

# Create feature importance table | 创建特征重要性表
if ("importance" %in% names(ml_results$rf$model$finalModel)) {
    rf_importance <- importance(ml_results$rf$model$finalModel)
    importance_df <- tibble(
        Feature = rownames(rf_importance),
        Importance = rf_importance[, 1]
    ) %>%
        arrange(desc(Importance))

    write_csv(
        importance_df,
        file.path("..", "results", "analysis", "05_feature_importance.csv")
    )
    cat("[OK] Feature importance saved\n")
}

# =============================================================================
# 12. 打印最终摘要 | Print Final Summary
# =============================================================================

cat("\n[STEP 9/9] Generating final summary...\n\n")

cat("=============================================================================\n")
cat("  Enhanced Machine Learning Validation Results\n")
cat("=============================================================================\n\n")

cat("[Data Information]\n")
cat(sprintf("  Sample size: %d observations\n", nrow(ml_data)))
cat(sprintf("  Training set: %d observations (%d countries)\n", nrow(train_data), length(train_countries)))
cat(sprintf("  Test set: %d observations (%d countries)\n", nrow(test_data), n_countries - length(train_countries)))
cat(sprintf("  Features: %d (theory-driven)\n", length(feature_cols)))
cat("\n")

cat("[Validation Strategy]\n")
cat("  Cross-validation: 10-fold CV x 5 repeats = 50 validations\n")
cat("  Nested CV: 5 outer x 5 inner folds (unbiased estimation)\n")
cat("  Bootstrap: 1000 resamples for robust confidence intervals\n")
cat("  Parallel cores: ", n_cores, "\n\n")

cat("[Model Performance]\n")
cat(sprintf("%-15s %10s %10s %20s %20s\n", "Model", "Test_RMSE", "Test_R²", "Nested_CV_RMSE", "Bootstrap_RMSE"))
cat(paste(rep("-", 85), collapse = ""), "\n")
for (i in 1:length(test_rmse)) {
    cat(sprintf(
        "%-15s %10.4f %10.4f %12.4f ± %.4f %12.4f [%.4f, %.4f]\n",
        toupper(names(ml_results)[i]),
        test_rmse[i],
        test_r2[i],
        nested_cv_rmse[i],
        nested_cv_sd[i],
        bootstrap_rmse[i],
        bootstrap_ci_lower[i],
        bootstrap_ci_upper[i]
    ))
}
cat("\n")

cat("[Marginal Effects - Years per 1% GDP in Government Health Expenditure]\n")
cat(sprintf("%-15s %12s %25s\n", "Model", "Effect", "95% CI"))
cat(paste(rep("-", 60), collapse = ""), "\n")
for (i in 1:length(marginal_effects)) {
    cat(sprintf(
        "%-15s %12.4f [%8.4f, %8.4f]\n",
        toupper(names(ml_results)[i]),
        marginal_effects[i],
        marginal_ci_lower[i],
        marginal_ci_upper[i]
    ))
}
cat("\n")

cat("[Summary Statistics]\n")
cat(sprintf("  Mean marginal effect: %.4f years\n", mean(marginal_effects, na.rm = TRUE)))
cat(sprintf("  SD of marginal effects: %.4f\n", sd(marginal_effects, na.rm = TRUE)))
cat(sprintf("  Range: [%.4f, %.4f]\n", min(marginal_effects), max(marginal_effects)))
cat("\n")

# Best model | 最佳模型
best_idx <- which.min(test_rmse)
cat(sprintf(
    "[Best Model]: %s (Test RMSE: %.4f, R²: %.4f)\n\n",
    toupper(names(ml_results)[best_idx]),
    test_rmse[best_idx],
    test_r2[best_idx]
))

cat("=============================================================================\n")
cat("  [OK] Script 05 completed successfully!\n")
cat("=============================================================================\n")
cat("  - Models trained: ", length(ml_results), "\n")
cat("  - Best model: ", toupper(names(ml_results)[best_idx]), "\n")
cat("  - Results saved to: ../results/analysis/\n")
cat("  - Validation: Nested CV + Bootstrap CI + 50x repeated CV\n\n")
