# =============================================================================
# 00_run_all_analyses.R
# Master Script - Run Complete Analysis Pipeline | 主脚本 - 运行完整分析流程
# =============================================================================
#
# 【功能概述 | Function Overview】
# 按顺序执行所有分析脚本，生成完整的分析结果
# Execute all analysis scripts in sequence to generate complete analysis results
#
# 【脚本顺序 | Script Sequence】
# 00. Setup and Configuration | 全局设置与配置
# 01. Data Loading and Cleaning | 数据加载与清洗
# 02. Data Integration | 数据整合与合并
# 03. Core Econometric Analysis | 核心计量经济分析
# 04. Threshold Regression | 门槛回归分析
# 05. Machine Learning Validation | 机器学习验证
#
# 【运行时间 | Total Runtime】
# ~ 30-45 minutes | ~30-45分钟
#
# 【最后更新 | Last Updated】
# 2025-11-05
# =============================================================================

# 记录开始时间 | Record start time
start_time <- Sys.time()

cat("\n")
cat("=============================================================================\n")
cat("  LANCET HEALTH EXPENDITURE ANALYSIS - MASTER PIPELINE\n")
cat("=============================================================================\n")
cat("\n")
cat(sprintf("  Start time: %s\n", format(start_time, "%Y-%m-%d %H:%M:%S")))
cat("\n")
cat("  This script will run the complete analysis pipeline:\n")
cat("    - Data loading and cleaning\n")
cat("    - Data integration\n")
cat("    - Core econometric analysis (FE, IV, GMM)\n")
cat("    - Threshold regression\n")
cat("    - Machine learning validation\n")
cat("\n")
cat("  Estimated time: 30-45 minutes\n")
cat("\n")
cat("=============================================================================\n")
cat("\n")

# 创建日志文件 | Create log file
log_dir <- file.path("..", "logs")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)

log_file <- file.path(log_dir, sprintf(
    "master_pipeline_%s.log",
    format(Sys.time(), "%Y%m%d_%H%M%S")
))

# 日志函数 | Logging function
log_to_file <- function(message) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    log_entry <- sprintf("[%s] %s\n", timestamp, message)
    cat(log_entry, file = log_file, append = TRUE)
    cat(log_entry)
}

log_to_file("Master pipeline started")

# =============================================================================
# 脚本执行函数 | Script Execution Function
# =============================================================================

run_script <- function(script_name, script_description) {
    cat("\n")
    cat("-----------------------------------------------------------------------------\n")
    cat(sprintf("  Running: %s - %s\n", script_name, script_description))
    cat("-----------------------------------------------------------------------------\n")

    log_to_file(sprintf("Starting %s", script_name))

    script_start <- Sys.time()

    # 执行脚本 | Execute script
    tryCatch(
        {
            source(script_name, echo = FALSE)

            script_end <- Sys.time()
            duration <- as.numeric(difftime(script_end, script_start, units = "mins"))

            log_to_file(sprintf(
                "Completed %s successfully (%.2f minutes)",
                script_name, duration
            ))

            return(list(success = TRUE, duration = duration, error = NULL))
        },
        error = function(e) {
            script_end <- Sys.time()
            duration <- as.numeric(difftime(script_end, script_start, units = "mins"))

            error_msg <- sprintf("ERROR in %s: %s", script_name, e$message)
            log_to_file(error_msg)

            cat("\n")
            cat("[ERROR] Script failed!\n")
            cat(sprintf("  Error message: %s\n", e$message))
            cat("\n")

            return(list(success = FALSE, duration = duration, error = e$message))
        }
    )
}

# =============================================================================
# 执行分析流程 | Execute Analysis Pipeline
# =============================================================================

# 存储结果 | Store results
pipeline_results <- list()

# 00. Setup (已在此脚本中运行) | Setup (already run in this script)
cat("\n[INFO] Running initial setup...\n")
pipeline_results[["00_setup"]] <- run_script(
    "01_setup_and_configuration.R",
    "Setup and Configuration | 全局设置与配置"
)

if (!pipeline_results[["00_setup"]]$success) {
    log_to_file("CRITICAL: Setup failed. Aborting pipeline.")
    stop("Setup failed. Cannot continue.")
}

# 01. Data Loading
pipeline_results[["01_loading"]] <- run_script(
    "02_data_loading_and_cleaning.R",
    "Data Loading and Cleaning | 数据加载与清洗"
)

if (!pipeline_results[["01_loading"]]$success) {
    log_to_file("CRITICAL: Data loading failed. Aborting pipeline.")
    stop("Data loading failed. Cannot continue.")
}

# 02. Data Integration
pipeline_results[["02_integration"]] <- run_script(
    "03_data_integration.R",
    "Data Integration | 数据整合与合并"
)

if (!pipeline_results[["02_integration"]]$success) {
    log_to_file("CRITICAL: Data integration failed. Aborting pipeline.")
    stop("Data integration failed. Cannot continue.")
}

# 03. Core Analysis
pipeline_results[["03_core_analysis"]] <- run_script(
    "04_core_econometric_analysis.R",
    "Core Econometric Analysis | 核心计量经济分析"
)

if (!pipeline_results[["03_core_analysis"]]$success) {
    log_to_file("WARNING: Core analysis failed, but continuing with pipeline.")
    cat("\n[WARN] Core analysis failed, but continuing with remaining steps.\n")
}

# 04. Threshold Regression
pipeline_results[["04_threshold"]] <- run_script(
    "05_threshold_regression.R",
    "Threshold Regression | 门槛回归分析"
)

if (!pipeline_results[["04_threshold"]]$success) {
    log_to_file("WARNING: Threshold regression failed, but continuing.")
    cat("\n[WARN] Threshold regression failed, but continuing.\n")
}

# 05. Machine Learning
pipeline_results[["05_ml"]] <- run_script(
    "06_ml_validation.R",
    "Machine Learning Validation | 机器学习验证"
)

if (!pipeline_results[["05_ml"]]$success) {
    log_to_file("WARNING: ML validation failed.")
    cat("\n[WARN] ML validation failed.\n")
}

# 06. Heterogeneity Analysis
pipeline_results[["06_heterogeneity"]] <- run_script(
    "07_heterogeneity_analysis.R",
    "Heterogeneity Analysis | 异质性分析"
)

# 07. Robustness Checks
pipeline_results[["07_robustness"]] <- run_script(
    "08_robustness_checks.R",
    "Robustness Checks | 稳健性检验"
)

# 10. Extended Analysis
pipeline_results[["10_extended"]] <- run_script(
    "09_extended_analysis.R",
    "Extended Analysis | 扩展分析"
)

# 11. Quasi-Experimental Analysis
pipeline_results[["11_quasi_experimental"]] <- run_script(
    "10_quasi_experimental_analysis.R",
    "Quasi-Experimental Analysis | 准实验分析"
)

# =============================================================================
# 生成图表 | Generate Figures and Tables
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("  GENERATING FIGURES AND TABLES | 生成图表\n")
cat("=============================================================================\n")

# 12. Main Figures
pipeline_results[["12_main_figures"]] <- run_script(
    "10_generate_main_figures.R",
    "Main Figures (Figures 1-3) | 主文本图形"
)

# 13. Main Tables
pipeline_results[["13_main_tables"]] <- run_script(
    "11_generate_main_tables.R",
    "Main Tables (Tables 1-3) | 主文本表格"
)

# 14. Supplementary Figures
pipeline_results[["14_supp_figures"]] <- run_script(
    "12_generate_supplementary_figures.R",
    "Supplementary Figures (S1-S12) | 附图"
)

# 15. Supplementary Tables
pipeline_results[["15_supp_tables"]] <- run_script(
    "13_generate_supplementary_tables.R",
    "Supplementary Tables (S1-S9) | 附表"
)

# 16. Quasi-Experimental Figures
pipeline_results[["16_quasi_experimental_figures"]] <- run_script(
    "15_generate_quasi_experimental_figures.R",
    "Quasi-Experimental Figures (S13) | 准实验分析图表"
)

# =============================================================================
# 生成执行摘要 | Generate Execution Summary
# =============================================================================

end_time <- Sys.time()
total_duration <- as.numeric(difftime(end_time, start_time, units = "mins"))

cat("\n")
cat("=============================================================================\n")
cat("  PIPELINE EXECUTION SUMMARY\n")
cat("=============================================================================\n\n")

# 创建摘要表 | Create summary table
summary_df <- tibble(
    Script = names(pipeline_results),
    Status = sapply(pipeline_results, function(x) ifelse(x$success, "SUCCESS", "FAILED")),
    Duration_Minutes = sapply(pipeline_results, function(x) round(x$duration, 2)),
    Error = sapply(pipeline_results, function(x) ifelse(is.null(x$error), "", x$error))
)

print(summary_df, n = 20)

cat("\n")
cat(sprintf("  Total execution time: %.2f minutes\n", total_duration))
cat(sprintf("  End time: %s\n", format(end_time, "%Y-%m-%d %H:%M:%S")))
cat("\n")

# 保存摘要 | Save summary
write_csv(summary_df, file.path("..", "results", "analysis", "00_pipeline_summary.csv"))
cat("[OK] Execution summary saved to: ../results/analysis/00_pipeline_summary.csv\n")

# 统计成功失败 | Count successes and failures
n_success <- sum(summary_df$Status == "SUCCESS")
n_failed <- sum(summary_df$Status == "FAILED")

cat("\n")
cat(sprintf("  Scripts completed successfully: %d/%d\n", n_success, nrow(summary_df)))
if (n_failed > 0) {
    cat(sprintf("  Scripts failed: %d\n", n_failed))
}
cat("\n")

# 最终状态 | Final status
if (n_failed == 0) {
    cat("=============================================================================\n")
    cat("  [SUCCESS] All analyses completed successfully!\n")
    cat("=============================================================================\n")
    log_to_file("Master pipeline completed successfully")
} else if (n_failed <= 2) {
    cat("=============================================================================\n")
    cat("  [PARTIAL SUCCESS] Most analyses completed\n")
    cat("=============================================================================\n")
    log_to_file("Master pipeline completed with some failures")
} else {
    cat("=============================================================================\n")
    cat("  [FAILED] Multiple analyses failed\n")
    cat("=============================================================================\n")
    log_to_file("Master pipeline completed with multiple failures")
}

cat("\n")
cat("[INFO] Output locations:\n")
cat("  - Results: ../results/analysis/\n")
cat("  - Tables: ../tables/\n")
cat("  - Figures: ../figures/\n")
cat("  - Logs: ../logs/\n")
cat("\n")
cat(sprintf("[INFO] Full log saved to: %s\n", log_file))
cat("\n")

# 保存工作空间（用于后续分析）| Save workspace (for subsequent analysis)
save.image(file.path("..", "results", "analysis", "00_workspace.RData"))
cat("[OK] Workspace saved for future reference\n\n")

log_to_file("Master pipeline finished")
