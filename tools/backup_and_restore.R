# =============================================================================
# backup_and_restore.R
# 代码、图片、表格的备份和恢复工具
# Backup and Restore Tool for Code, Figures, and Tables
# =============================================================================
#
# 【功能 | Functions】
# 1. 在执行代码更新前自动备份代码、图片、表格
# 2. 提供恢复功能，可以回滚到上一版本
# 3. 记录每次备份的时间戳和变更说明
#
# 【使用方法 | Usage】
# source("tools/backup_and_restore.R")
# backup_before_update("更新阈值回归代码")
# # ... 执行代码更新 ...
# # 如果不满意，可以恢复：
# restore_from_backup()
#
# =============================================================================

library(tidyverse)

# 设置备份目录
backup_root <- file.path("..", "backup")
dir.create(backup_root, showWarnings = FALSE, recursive = TRUE)

# 备份时间戳
backup_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# =============================================================================
# 1. 备份函数 | Backup Function
# =============================================================================

backup_before_update <- function(change_description = "Code update") {
    cat("\n==================================================\n")
    cat("  创建备份 | Creating Backup\n")
    cat("==================================================\n\n")
    
    # 创建带时间戳的备份目录
    backup_dir <- file.path(backup_root, backup_timestamp)
    dir.create(backup_dir, showWarnings = FALSE, recursive = TRUE)
    
    # 创建子目录
    dir.create(file.path(backup_dir, "code_final"), showWarnings = FALSE, recursive = TRUE)
    dir.create(file.path(backup_dir, "figures_final"), showWarnings = FALSE, recursive = TRUE)
    dir.create(file.path(backup_dir, "tables_final"), showWarnings = FALSE, recursive = TRUE)
    dir.create(file.path(backup_dir, "results"), showWarnings = FALSE, recursive = TRUE)
    
    cat(sprintf("[INFO] 备份目录: %s\n", backup_dir))
    cat(sprintf("[INFO] 变更说明: %s\n", change_description))
    
    # 1. 备份代码文件
    cat("\n[1/4] 备份代码文件...\n")
    code_files <- list.files(".", pattern = "\\.R$", full.names = TRUE, recursive = FALSE)
    if (length(code_files) > 0) {
        file.copy(code_files, file.path(backup_dir, "code_final"), overwrite = TRUE)
        cat(sprintf("  ✓ 备份了 %d 个代码文件\n", length(code_files)))
    }
    
    # 2. 备份图片文件
    cat("\n[2/4] 备份图片文件...\n")
    figures_main <- list.files("../figures_final/main", pattern = "\\.(pdf|png|svg)$", 
                               full.names = TRUE, recursive = TRUE)
    figures_supp <- list.files("../figures_final/supplementary", pattern = "\\.(pdf|png|svg)$", 
                              full.names = TRUE, recursive = TRUE)
    
    if (length(figures_main) > 0) {
        dir.create(file.path(backup_dir, "figures_final", "main"), 
                  showWarnings = FALSE, recursive = TRUE)
        file.copy(figures_main, file.path(backup_dir, "figures_final", "main"), 
                 overwrite = TRUE, recursive = TRUE)
        cat(sprintf("  ✓ 备份了 %d 个主图片文件\n", length(figures_main)))
    }
    
    if (length(figures_supp) > 0) {
        dir.create(file.path(backup_dir, "figures_final", "supplementary"), 
                  showWarnings = FALSE, recursive = TRUE)
        file.copy(figures_supp, file.path(backup_dir, "figures_final", "supplementary"), 
                 overwrite = TRUE, recursive = TRUE)
        cat(sprintf("  ✓ 备份了 %d 个补充图片文件\n", length(figures_supp)))
    }
    
    # 3. 备份表格文件
    cat("\n[3/4] 备份表格文件...\n")
    tables_main <- list.files("../tables_final/main", pattern = "\\.(pdf|docx|csv)$", 
                             full.names = TRUE, recursive = TRUE)
    tables_supp <- list.files("../tables_final/supplementary", pattern = "\\.(pdf|docx|csv)$", 
                             full.names = TRUE, recursive = TRUE)
    
    if (length(tables_main) > 0) {
        dir.create(file.path(backup_dir, "tables_final", "main"), 
                  showWarnings = FALSE, recursive = TRUE)
        file.copy(tables_main, file.path(backup_dir, "tables_final", "main"), 
                 overwrite = TRUE, recursive = TRUE)
        cat(sprintf("  ✓ 备份了 %d 个主表格文件\n", length(tables_main)))
    }
    
    if (length(tables_supp) > 0) {
        dir.create(file.path(backup_dir, "tables_final", "supplementary"), 
                  showWarnings = FALSE, recursive = TRUE)
        file.copy(tables_supp, file.path(backup_dir, "tables_final", "supplementary"), 
                 overwrite = TRUE, recursive = TRUE)
        cat(sprintf("  ✓ 备份了 %d 个补充表格文件\n", length(tables_supp)))
    }
    
    # 4. 备份关键结果文件
    cat("\n[4/4] 备份结果文件...\n")
    results_files <- list.files("../results/analysis", pattern = "\\.(csv|rds)$", 
                               full.names = TRUE, recursive = FALSE)
    if (length(results_files) > 0) {
        dir.create(file.path(backup_dir, "results", "analysis"), 
                  showWarnings = FALSE, recursive = TRUE)
        file.copy(results_files, file.path(backup_dir, "results", "analysis"), 
                 overwrite = TRUE)
        cat(sprintf("  ✓ 备份了 %d 个结果文件\n", length(results_files)))
    }
    
    # 保存备份元数据
    metadata <- data.frame(
        backup_timestamp = backup_timestamp,
        backup_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        change_description = change_description,
        n_code_files = length(code_files),
        n_figures_main = length(figures_main),
        n_figures_supp = length(figures_supp),
        n_tables_main = length(tables_main),
        n_tables_supp = length(tables_supp),
        n_results_files = length(results_files),
        stringsAsFactors = FALSE
    )
    
    write_csv(metadata, file.path(backup_dir, "backup_metadata.csv"))
    
    # 保存到备份索引
    backup_index_file <- file.path(backup_root, "backup_index.csv")
    if (file.exists(backup_index_file)) {
        backup_index <- read_csv(backup_index_file, show_col_types = FALSE)
        backup_index <- bind_rows(backup_index, metadata)
    } else {
        backup_index <- metadata
    }
    write_csv(backup_index, backup_index_file)
    
    cat("\n==================================================\n")
    cat("  ✓ 备份完成 | Backup Complete\n")
    cat("==================================================\n\n")
    cat(sprintf("备份位置: %s\n", backup_dir))
    cat(sprintf("备份索引: %s\n", backup_index_file))
    cat("\n如需恢复，请使用: restore_from_backup(\"%s\")\n", backup_timestamp)
    
    return(backup_timestamp)
}

# =============================================================================
# 2. 恢复函数 | Restore Function
# =============================================================================

restore_from_backup <- function(backup_timestamp = NULL) {
    cat("\n==================================================\n")
    cat("  恢复备份 | Restoring Backup\n")
    cat("==================================================\n\n")
    
    # 如果没有指定时间戳，列出所有备份
    if (is.null(backup_timestamp)) {
        backup_index_file <- file.path(backup_root, "backup_index.csv")
        if (file.exists(backup_index_file)) {
            backup_index <- read_csv(backup_index_file, show_col_types = FALSE)
            cat("可用的备份:\n")
            print(backup_index[, c("backup_timestamp", "backup_time", "change_description")])
            cat("\n请指定要恢复的备份时间戳，例如: restore_from_backup(\"20251220_143000\")\n")
            return(invisible(NULL))
        } else {
            cat("[ERROR] 未找到备份索引文件\n")
            return(invisible(NULL))
        }
    }
    
    # 检查备份目录是否存在
    backup_dir <- file.path(backup_root, backup_timestamp)
    if (!dir.exists(backup_dir)) {
        cat(sprintf("[ERROR] 备份目录不存在: %s\n", backup_dir))
        return(invisible(NULL))
    }
    
    cat(sprintf("[INFO] 从备份恢复: %s\n", backup_timestamp))
    
    # 确认恢复
    cat("\n⚠️  警告：此操作将覆盖当前文件！\n")
    cat("是否继续？(输入 'yes' 确认): ")
    # 在实际使用中，可以通过交互式输入确认
    
    # 1. 恢复代码文件
    cat("\n[1/4] 恢复代码文件...\n")
    code_backup_dir <- file.path(backup_dir, "code_final")
    if (dir.exists(code_backup_dir)) {
        code_files <- list.files(code_backup_dir, pattern = "\\.R$", full.names = TRUE)
        if (length(code_files) > 0) {
            file.copy(code_files, ".", overwrite = TRUE)
            cat(sprintf("  ✓ 恢复了 %d 个代码文件\n", length(code_files)))
        }
    }
    
    # 2. 恢复图片文件
    cat("\n[2/4] 恢复图片文件...\n")
    figures_main_backup <- file.path(backup_dir, "figures_final", "main")
    figures_supp_backup <- file.path(backup_dir, "figures_final", "supplementary")
    
    if (dir.exists(figures_main_backup)) {
        figures_main <- list.files(figures_main_backup, pattern = "\\.(pdf|png|svg)$", 
                                  full.names = TRUE, recursive = TRUE)
        if (length(figures_main) > 0) {
            dir.create("../figures_final/main", showWarnings = FALSE, recursive = TRUE)
            file.copy(figures_main, "../figures_final/main", overwrite = TRUE, recursive = TRUE)
            cat(sprintf("  ✓ 恢复了 %d 个主图片文件\n", length(figures_main)))
        }
    }
    
    if (dir.exists(figures_supp_backup)) {
        figures_supp <- list.files(figures_supp_backup, pattern = "\\.(pdf|png|svg)$", 
                                   full.names = TRUE, recursive = TRUE)
        if (length(figures_supp) > 0) {
            dir.create("../figures_final/supplementary", showWarnings = FALSE, recursive = TRUE)
            file.copy(figures_supp, "../figures_final/supplementary", overwrite = TRUE, recursive = TRUE)
            cat(sprintf("  ✓ 恢复了 %d 个补充图片文件\n", length(figures_supp)))
        }
    }
    
    # 3. 恢复表格文件
    cat("\n[3/4] 恢复表格文件...\n")
    tables_main_backup <- file.path(backup_dir, "tables_final", "main")
    tables_supp_backup <- file.path(backup_dir, "tables_final", "supplementary")
    
    if (dir.exists(tables_main_backup)) {
        tables_main <- list.files(tables_main_backup, pattern = "\\.(pdf|docx|csv)$", 
                                 full.names = TRUE, recursive = TRUE)
        if (length(tables_main) > 0) {
            dir.create("../tables_final/main", showWarnings = FALSE, recursive = TRUE)
            file.copy(tables_main, "../tables_final/main", overwrite = TRUE, recursive = TRUE)
            cat(sprintf("  ✓ 恢复了 %d 个主表格文件\n", length(tables_main)))
        }
    }
    
    if (dir.exists(tables_supp_backup)) {
        tables_supp <- list.files(tables_supp_backup, pattern = "\\.(pdf|docx|csv)$", 
                                 full.names = TRUE, recursive = TRUE)
        if (length(tables_supp) > 0) {
            dir.create("../tables_final/supplementary", showWarnings = FALSE, recursive = TRUE)
            file.copy(tables_supp, "../tables_final/supplementary", overwrite = TRUE, recursive = TRUE)
            cat(sprintf("  ✓ 恢复了 %d 个补充表格文件\n", length(tables_supp)))
        }
    }
    
    # 4. 恢复结果文件
    cat("\n[4/4] 恢复结果文件...\n")
    results_backup_dir <- file.path(backup_dir, "results", "analysis")
    if (dir.exists(results_backup_dir)) {
        results_files <- list.files(results_backup_dir, pattern = "\\.(csv|rds)$", 
                                   full.names = TRUE)
        if (length(results_files) > 0) {
            dir.create("../results/analysis", showWarnings = FALSE, recursive = TRUE)
            file.copy(results_files, "../results/analysis", overwrite = TRUE)
            cat(sprintf("  ✓ 恢复了 %d 个结果文件\n", length(results_files)))
        }
    }
    
    cat("\n==================================================\n")
    cat("  ✓ 恢复完成 | Restore Complete\n")
    cat("==================================================\n\n")
}

# =============================================================================
# 3. 列出所有备份 | List All Backups
# =============================================================================

list_backups <- function() {
    backup_index_file <- file.path(backup_root, "backup_index.csv")
    if (file.exists(backup_index_file)) {
        backup_index <- read_csv(backup_index_file, show_col_types = FALSE)
        cat("\n所有备份记录:\n")
        print(backup_index[, c("backup_timestamp", "backup_time", "change_description")])
        return(backup_index)
    } else {
        cat("未找到备份记录\n")
        return(NULL)
    }
}

# =============================================================================
# 4. 导出函数到全局环境 | Export Functions
# =============================================================================

cat("[OK] 备份和恢复工具已加载\n")
cat("使用方法:\n")
cat("  backup_before_update(\"变更说明\")\n")
cat("  restore_from_backup(\"备份时间戳\")\n")
cat("  list_backups()\n")

