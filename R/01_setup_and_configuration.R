# =============================================================================
# 01_setup_and_configuration.R
# Setup and Global Configuration | 全局设置与配置
# =============================================================================
#
# 【功能概述 | Function Overview】
# 全局配置和环境设置
# Global configuration and environment setup
# - Set random seed for reproducibility | 设置随机种子确保可复现性
# - Configure global parameters | 配置全局参数
# - Load required R packages | 加载必要的R包
# - Create output directory structure | 创建输出目录结构
#
# 【输入文件 | Input Files】
# None | 无
#
# 【输出文件 | Output Files】
# None (environment setup only) | 无（仅设置环境）
#
# 【运行时间 | Runtime】
# < 1 minute | <1分钟
#
# 【最后更新 | Last Updated】
# 2025-11-05
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("  Lancet Health Expenditure Analysis - Setup and Configuration\n")
cat("=============================================================================\n\n")

# =============================================================================
# 1. 全局随机种子设置 | Global Random Seed Configuration
# =============================================================================

# 设置全局随机种子以确保所有随机过程可重现
# Set global random seed to ensure reproducibility of all random processes
GLOBAL_SEED <- 20251024

set.seed(GLOBAL_SEED)
cat("[OK] Global random seed set:", GLOBAL_SEED, "\n")

# 定义重置种子的函数（供后续脚本使用）
# Define function to reset seed (for use in subsequent scripts)
reset_seed <- function() {
    set.seed(GLOBAL_SEED)
    cat("[INFO] Random seed reset:", GLOBAL_SEED, "\n")
}

# =============================================================================
# 2. 全局参数配置 | Global Parameter Configuration
# =============================================================================

# 分析参数 | Analysis parameters
CONFIG <- list(
    # 数据参数 | Data parameters
    data = list(
        start_year = 2000,
        end_year = 2022,
        min_observations_per_country = 10
    ),

    # Bootstrap参数 | Bootstrap parameters
    bootstrap = list(
        n_iterations = 1000,
        confidence_level = 0.95,
        method = "bca" # bias-corrected and accelerated
    ),

    # 并行计算参数 | Parallel computing parameters
    parallel = list(
        enabled = TRUE,
        n_cores = parallel::detectCores() - 1
    ),

    # 输出格式 | Output formats
    output = list(
        figure_dpi = 300,
        figure_format = "pdf",
        table_format = c("csv", "tex")
    ),

    # 显著性水平 | Significance levels
    significance = list(
        alpha = 0.05,
        fdr_method = "BH" # Benjamini-Hochberg
    )
)

cat("[OK] Global configuration parameters loaded\n")

# =============================================================================
# 3. 目录结构创建 | Directory Structure Creation
# =============================================================================

# 定义目录结构 | Define directory structure
DIRS <- list(
    root = getwd(),
    data_raw = file.path("..", "data", "raw"),
    data_processed = file.path("..", "data", "processed"),
    results = file.path("..", "results"),
    results_analysis = file.path("..", "results", "analysis"),
    results_tables = file.path("..", "results", "tables"),
    results_figures = file.path("..", "results", "figures"),
    figures = file.path("..", "figures"),
    tables = file.path("..", "tables"),
    logs = file.path("..", "logs")
)

# 创建必要的目录 | Create necessary directories
for (dir_name in names(DIRS)) {
    dir_path <- DIRS[[dir_name]]
    if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
        cat("[INFO] Created directory:", dir_name, "\n")
    }
}

cat("[OK] All output directories ready\n")

# =============================================================================
# 4. R包管理和加载 | R Package Management and Loading
# =============================================================================

cat("\n[INFO] Checking and loading required R packages...\n")

# 定义所有需要的包 | Define all required packages
required_packages <- c(
    # 数据处理 | Data manipulation
    "dplyr", "readr", "tidyr", "purrr", "stringr",

    # 数据导入 | Data import
    "readxl", "haven", "countrycode",

    # 面板数据分析 | Panel data analysis
    "plm", "lmtest", "sandwich",

    # 工具变量 | Instrumental variables
    "AER", "ivreg", "systemfit",

    # 门槛回归 | Threshold regression
    "segmented",

    # 机器学习 | Machine learning
    "randomForest", "e1071", "xgboost", "glmnet", "gbm",

    # 可视化 | Visualization
    "ggplot2", "gridExtra", "cowplot", "patchwork",

    # 表格生成 | Table generation
    "kableExtra", "flextable", "gt",

    # 并行计算 | Parallel computing
    "parallel", "foreach", "doParallel",

    # 统计检验 | Statistical tests
    "car", "broom", "stats"
)

# 检查并安装缺失的包 | Check and install missing packages
missing_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]

if (length(missing_packages) > 0) {
    cat("[WARN] Found", length(missing_packages), "missing packages, installing...\n")
    install.packages(missing_packages, repos = "https://cloud.r-project.org/")
}

# 加载所有包（静默加载）| Load all packages (silent loading)
suppressPackageStartupMessages({
    for (pkg in required_packages) {
        library(pkg, character.only = TRUE)
    }
})

cat("[OK] All R packages loaded\n")

# =============================================================================
# 5. 并行计算设置 | Parallel Computing Setup
# =============================================================================

if (CONFIG$parallel$enabled) {
    library(doParallel)

    cl <- makeCluster(CONFIG$parallel$n_cores)
    registerDoParallel(cl)

    cat(sprintf("[OK] Parallel computing enabled: %d cores\n", CONFIG$parallel$n_cores))

    # 注册清理函数（在脚本结束时关闭集群）
    # Register cleanup function (close cluster on script exit)
    on.exit(
        {
            if (exists("cl")) {
                stopCluster(cl)
                cat("[INFO] Parallel computing cluster closed\n")
            }
        },
        add = TRUE
    )
}

# =============================================================================
# 6. 辅助函数定义 | Helper Function Definitions
# =============================================================================

# 保存数据的标准化函数 | Standardized function for saving data
save_data <- function(data, filename, subdirectory = "analysis") {
    filepath <- file.path(DIRS$results, subdirectory, filename)

    # 根据文件扩展名选择保存方法 | Choose save method based on file extension
    if (grepl("\\.csv$", filename)) {
        write_csv(data, filepath)
    } else if (grepl("\\.rds$", filename)) {
        saveRDS(data, filepath)
    } else {
        stop("Unsupported file format")
    }

    cat(sprintf("[SAVE] Data saved: %s\n", filename))
    return(invisible(filepath))
}

# 加载数据的标准化函数 | Standardized function for loading data
load_data <- function(filename, subdirectory = "analysis") {
    filepath <- file.path(DIRS$results, subdirectory, filename)

    if (!file.exists(filepath)) {
        stop(sprintf("File not found: %s", filepath))
    }

    # 根据文件扩展名选择读取方法 | Choose load method based on file extension
    if (grepl("\\.csv$", filename)) {
        data <- read_csv(filepath, show_col_types = FALSE)
    } else if (grepl("\\.rds$", filename)) {
        data <- readRDS(filepath)
    } else {
        stop("Unsupported file format")
    }

    cat(sprintf("[LOAD] Data loaded: %s\n", filename))
    return(data)
}

# 验证数据与手稿一致性的函数 | Function to verify consistency with manuscript
verify_consistency <- function(computed_value, manuscript_value,
                               description, tolerance = 0.001) {
    diff <- abs(computed_value - manuscript_value)
    passed <- diff < tolerance

    status <- if (passed) "[OK]" else "[FAIL]"

    cat(sprintf(
        "%s %s: computed=%.4f, manuscript=%.4f, diff=%.4f\n",
        status, description, computed_value, manuscript_value, diff
    ))

    if (!passed) {
        warning(sprintf("Data inconsistency: %s", description))
    }

    return(passed)
}

# =============================================================================
# 7. 日志系统设置 | Logging System Setup
# =============================================================================

# 创建日志文件 | Create log file
log_file <- file.path(DIRS$logs, sprintf(
    "analysis_log_%s.txt",
    format(Sys.time(), "%Y%m%d_%H%M%S")
))

# 日志记录函数 | Logging function
log_message <- function(message, level = "INFO") {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    log_entry <- sprintf("[%s] [%s] %s\n", timestamp, level, message)

    # 写入日志文件 | Write to log file
    cat(log_entry, file = log_file, append = TRUE)

    # 同时输出到控制台（错误级别）| Also output to console (for ERROR level)
    if (level == "ERROR") {
        cat(log_entry)
    }
}

log_message("Analysis pipeline started")
log_message(sprintf("R version: %s", R.version.string))
log_message(sprintf("Working directory: %s", getwd()))

# =============================================================================
# 8. 完成设置 | Setup Complete
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("  [OK] Setup complete! Ready to begin analysis\n")
cat("=============================================================================\n")
cat("\n")
cat("[INFO] System information:\n")
cat(sprintf("  - R version: %s\n", R.version.string))
cat(sprintf("  - Random seed: %d\n", GLOBAL_SEED))
cat(sprintf("  - Parallel cores: %d\n", if (CONFIG$parallel$enabled) CONFIG$parallel$n_cores else 0))
cat(sprintf("  - Working directory: %s\n", getwd()))
cat("\n")
cat("[INFO] Output directories:\n")
cat(sprintf("  - Results: %s\n", DIRS$results))
cat(sprintf("  - Figures: %s\n", DIRS$figures))
cat(sprintf("  - Tables: %s\n", DIRS$tables))
cat(sprintf("  - Logs: %s\n", log_file))
cat("\n")

# 返回配置对象供其他脚本使用 | Return configuration object for use in other scripts
invisible(list(CONFIG = CONFIG, DIRS = DIRS, GLOBAL_SEED = GLOBAL_SEED))
