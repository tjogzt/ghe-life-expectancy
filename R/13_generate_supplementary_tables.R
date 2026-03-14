# =============================================================================
# 13_generate_supplementary_tables.R
# Generate Supplementary Tables for Lancet | 生成 Lancet 补充表格
# =============================================================================
#
# 【功能概述 | Function Overview】
# 生成所有补充材料表格 (Tables S1-S11)
# Generate all supplementary tables (Tables S1-S11):
#   - Table S1: Complete Descriptive Statistics | 完整描述性统计
#   - Table S2: Correlation Matrix | 相关系数矩阵
#   - Table S3: Data Sources | 数据来源说明
#   - Table S4: IV Validity Tests | IV有效性检验
#   - Table S5: Heterogeneity by Income | 收入组异质性
#   - Table S6: Heterogeneity by Region | 地区异质性
#   - Table S7-S8: Mediation Analysis | 中介效应分析
#   - Table S9: Country List | 国家列表
#
# 【输出文件 | Output Files】
#   - tables/supplementary/Table_S1_*.tex/.pdf
#   - tables/supplementary/Table_S2_*.tex/.pdf
#   - ... (S1-S11)
#
# 【运行时间 | Runtime】
# ~ 5-10 minutes | ~5-10分钟
#
# 【最后更新 | Last Updated】
# 2025-11-07
# =============================================================================

library(tidyverse)
library(kableExtra)

cat("\n==================================================\n")
cat("  生成所有附表 (Tables S1-S9)\n")
cat("==================================================\n\n")

# =============================================================================
# 辅助函数：通用表格生成
# =============================================================================

generate_standard_table <- function(table_id, table_name, table_title, table_note,
                                   col_types = NULL, escape_special = TRUE,
                                   decimal_cols = NULL, left_align = FALSE, custom_col_widths = NULL) {
    
    cat(sprintf("[%s] 生成 %s...\n", table_id, table_name))
    
    csv_file <- sprintf("../tables/supplementary/%s.csv", table_name)
    if (!file.exists(csv_file)) {
        cat(sprintf("  ⚠ 文件不存在，跳过: %s\n", csv_file))
        return(NULL)
    }
    table_data <- read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE)
    
    # 转义特殊字符
    if (escape_special) {
        colnames(table_data) <- gsub("_", "\\\\_", colnames(table_data))
        colnames(table_data) <- gsub("%", "\\\\%", colnames(table_data))
        
        for (col in names(table_data)) {
            table_data[[col]] <- gsub("&", "\\\\&", table_data[[col]])
            table_data[[col]] <- gsub("%", "\\\\%", table_data[[col]])
            table_data[[col]] <- gsub("_", "\\\\_", table_data[[col]])
        }
    }
    
    # 自动确定列类型
    if (is.null(col_types)) {
        col_types <- rep("c", ncol(table_data))
        col_types[1] <- "l"
    }
    
    # Decimal alignment setup
    decimal_info <- list()
    if (!is.null(decimal_cols)) {
        for (col in decimal_cols) {
            if (col %in% colnames(table_data)) {
                numeric_vals <- suppressWarnings(as.numeric(table_data[[col]]))
                valid_vals <- numeric_vals[!is.na(numeric_vals)]
                if (length(valid_vals) > 0) {
                    before <- max(nchar(sub("[-.].*$", "", formatC(abs(valid_vals), format = "f", digits = 6)))) # digits before decimal
                    after <- max(nchar(sub("^[^\\.]*\\.?","", sub("0+$","", sub(".*\\.","", formatC(abs(valid_vals), format = "f", digits = 6)))))) # digits after
                    if (after == 0) after <- 1
                    decimal_info[[col]] <- list(format = sprintf("S[table-format=%d.%d]", before, after),
                                                digits = after,
                                                values = numeric_vals)
                }
            }
        }
    }
    
    # Update column spec with S columns for decimal alignment or custom widths
    for (idx in seq_along(col_types)) {
        col_name <- colnames(table_data)[idx]
        if (!is.null(decimal_info[[col_name]])) {
            col_types[idx] <- decimal_info[[col_name]]$format
        } else if (!is.null(custom_col_widths) && idx <= length(custom_col_widths)) {
            col_types[idx] <- custom_col_widths[idx]
        } else if (col_types[idx] == "c") {
            col_types[idx] <- "p{1.2cm}"
        } else if (col_types[idx] == "l") {
            col_types[idx] <- "p{4.0cm}"
        }
    }
    
    col_spec <- paste0("@{}", paste(col_types, collapse = ""), "@{}")
    
    # 构建LaTeX表格
    latex_rows <- c()
    for (i in 1:nrow(table_data)) {
        row_values <- as.character(table_data[i, ])
        for (j in seq_along(row_values)) {
            col_name <- colnames(table_data)[j]
            if (!is.null(decimal_info[[col_name]])) {
                val <- decimal_info[[col_name]]$values[i]
                row_values[j] <- ifelse(is.na(val), "", formatC(val, format = "f", digits = decimal_info[[col_name]]$digits))
            } else {
                cell <- row_values[j]
                cell <- gsub("&", "\\\\&", cell)
                cell <- gsub("%", "\\\\%", cell)
                cell <- gsub("_", "\\\\_", cell)
                row_values[j] <- cell
            }
        }
        latex_rows <- c(latex_rows, paste(row_values, collapse = " & "))
    }
    
    table_body <- paste(latex_rows, collapse = " \\\\\n")
    header_row <- paste(sprintf("\\textbf{%s}", colnames(table_data)), collapse = " & ")
    
    # Keep \textless and \textgreater as is (T1 fontenc will handle them)
    table_note_fixed <- table_note
    
    alignment_prefix <- if (left_align) "\\noindent" else ""
    
    # Only include siunitx if decimal alignment is used
    siunitx_packages <- if (length(decimal_info) > 0) {
        "\\usepackage{siunitx}\n\\sisetup{detect-all}\n"
    } else {
        ""
    }
    
    latex_doc <- paste0(
        "\\documentclass[11pt]{article}\n",
        "\\usepackage[T1]{fontenc}\n",
        "\\usepackage[T1]{fontenc}
        \usepackage[margin=0.15in]{geometry}\n",
        "\\usepackage{booktabs}\n",
        "\\usepackage{array}\n",
        siunitx_packages,
        "\\usepackage{amsmath}\n",
        "\\usepackage{helvet}\n",
        "\\renewcommand{\\familydefault}{\\sfdefault}\n",
        "\\begin{document}\n\n",
        "\\newsavebox{\\tablebox}\n",
        "\\begin{table}[h!]\n",
        alignment_prefix, "{\\fontsize{8}{10}\\selectfont\\textbf{", table_title, "}}\\\\[2mm]\n",
        if (left_align) "\\noindent" else "\\centering\n",
        "\\savebox{\\tablebox}{\n",
        "{\\fontsize{8}{10}\\selectfont\n",
        "\\setlength{\\tabcolsep}{4pt}\n",
        "\\begin{tabular}{", col_spec, "}\n",
        "\\toprule\n",
        header_row, " \\\\\n",
        "\\midrule\n",
        table_body, " \\\\\n",
        "\\bottomrule\n",
        "\\end{tabular}\n",
        "}}\n",
        "\\usebox{\\tablebox}\n\n",
        "\\vspace{2mm}\n\n",
        if (left_align) "\\noindent" else "",
        "\\begin{minipage}{\\wd\\tablebox}\n",
        "{\\fontsize{7}{9}\\selectfont\\itshape ", table_note_fixed, "}\n",
        "\\end{minipage}\n",
        "\\end{table}\n\n",
        "\\end{document}\n"
    )
    
    tex_file <- sprintf("../tables/supplementary/%s.tex", table_name)
    writeLines(latex_doc, tex_file)
    
    # 编译PDF
    setwd("../tables/supplementary")
    system(sprintf("pdflatex -interaction=nonstopmode %s.tex > /dev/null 2>&1", table_name))
    system(sprintf("pdflatex -interaction=nonstopmode %s.tex > /dev/null 2>&1", table_name))
    file.remove(sprintf("%s.aux", table_name), sprintf("%s.log", table_name))
    setwd("../R")
    
    cat(sprintf("  ✓ %s 生成完成\n\n", table_name))
}

# =============================================================================
# 特殊表格：需要合并标准误的表格 (S3, S5, S6)
# =============================================================================

generate_table_with_merged_se <- function(table_id, table_name, table_title, table_note,
                                         col_types = NULL) {
    
    cat(sprintf("[%s] 生成 %s (合并标准误)...\n", table_id, table_name))
    
    csv_file <- sprintf("../tables/supplementary/%s.csv", table_name)
    if (!file.exists(csv_file)) {
        cat(sprintf("  ⚠ 文件不存在，跳过: %s\n", csv_file))
        return(NULL)
    }
    table_data <- read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE)
    
    # 转义特殊字符
    colnames(table_data) <- gsub("_", "\\\\_", colnames(table_data))
    colnames(table_data) <- gsub("%", "\\\\%", colnames(table_data))
    
    for (col in names(table_data)) {
        table_data[[col]] <- gsub("&", "\\\\&", table_data[[col]])
        table_data[[col]] <- gsub("%", "\\\\%", table_data[[col]])
    }
    
    # 识别标准误行并合并
    merged_rows <- list()
    i <- 1
    while (i <= nrow(table_data)) {
        if (i < nrow(table_data)) {
            # 检查下一行是否为标准误行
            next_row <- table_data[i + 1, ]
            is_se <- any(grepl("^\\(", as.character(next_row[-1])))
            
            if (is_se) {
                # 合并当前行和下一行
                merged_row <- table_data[i, ]
                for (col_idx in 2:ncol(table_data)) {
                    coef <- as.character(table_data[i, col_idx])
                    se <- as.character(table_data[i + 1, col_idx])
                    # 安全的NA检查
                    coef_valid <- !is.na(coef) && coef != "" && coef != "NA"
                    se_valid <- !is.na(se) && se != "" && se != "NA"
                    if (coef_valid && se_valid) {
                        merged_row[col_idx] <- sprintf("%s %s", coef, se)
                    } else if (coef_valid) {
                        merged_row[col_idx] <- coef
                    } else {
                        merged_row[col_idx] <- ""
                    }
                }
                merged_rows[[length(merged_rows) + 1]] <- merged_row
                i <- i + 2  # 跳过下一行
            } else {
                merged_rows[[length(merged_rows) + 1]] <- table_data[i, ]
                i <- i + 1
            }
        } else {
            merged_rows[[length(merged_rows) + 1]] <- table_data[i, ]
            i <- i + 1
        }
    }
    
    table_data_merged <- do.call(rbind, merged_rows)
    rownames(table_data_merged) <- NULL
    
    # 移除NA
    for (col in names(table_data_merged)) {
        table_data_merged[[col]][table_data_merged[[col]] == "NA"] <- ""
    }
    
    # 自动确定列类型
    if (is.null(col_types)) {
        # 检查是否是Robustness Checks表（有GHE_Coefficient列）
        if ("GHE\\_Coefficient" %in% colnames(table_data_merged)) {
            col_types <- c("l", "r", rep("c", ncol(table_data_merged) - 2))  # 第2列右对齐
        } else {
            col_types <- c("l", rep("c", ncol(table_data_merged) - 1))
        }
    }
    
    col_spec <- paste0("@{}", paste(col_types, collapse = ""), "@{}")
    
    # 构建LaTeX表格
    latex_rows <- c()
    for (i in 1:nrow(table_data_merged)) {
        row_values <- as.character(table_data_merged[i, ])
        latex_rows <- c(latex_rows, paste(row_values, collapse = " & "))
    }
    
    table_body <- paste(latex_rows, collapse = " \\\\\n")
    header_row <- paste(sprintf("\\textbf{%s}", colnames(table_data_merged)), collapse = " & ")
    title_row <- paste0("\\multicolumn{", ncol(table_data_merged), "}{@{}l}{\\textbf{", table_title, "}}\\\\[2mm]\n")
    
    # Keep \textless and \textgreater as is (T1 fontenc will handle them)
    table_note_fixed <- table_note
    
    latex_doc <- paste0(
        "\\documentclass[11pt]{article}\n",
        "\\usepackage[T1]{fontenc}\n",
        "\\usepackage[T1]{fontenc}
        \usepackage[margin=0.15in]{geometry}\n",
        "\\usepackage{booktabs}\n",
        "\\usepackage{array}\n",
        "\\usepackage{amsmath}\n",
        "\\usepackage{helvet}\n",
        "\\renewcommand{\\familydefault}{\\sfdefault}\n",
        "\\begin{document}\n\n",
        "\\newsavebox{\\tablebox}\n",
        "\\begin{table}[h!]\n",
        "\\centering\n",
        "\\savebox{\\tablebox}{\n",
        "{\\fontsize{8}{10}\\selectfont\n",
        "\\begin{tabular}{", col_spec, "}\n",
        title_row,
        "\\toprule\n",
        header_row, " \\\\\n",
        "\\midrule\n",
        table_body, " \\\\\n",
        "\\bottomrule\n",
        "\\end{tabular}\n",
        "}}\n",
        "\\usebox{\\tablebox}\n\n",
        "\\vspace{2mm}\n\n",
        "\\begin{minipage}{\\wd\\tablebox}\n",
        "{\\fontsize{7}{9}\\selectfont\\itshape ", table_note_fixed, "}\n",
        "\\end{minipage}\n",
        "\\end{table}\n\n",
        "\\end{document}\n"
    )
    
    tex_file <- sprintf("../tables/supplementary/%s.tex", table_name)
    writeLines(latex_doc, tex_file)
    
    # 编译PDF
    setwd("../tables/supplementary")
    system(sprintf("pdflatex -interaction=nonstopmode %s.tex > /dev/null 2>&1", table_name))
    system(sprintf("pdflatex -interaction=nonstopmode %s.tex > /dev/null 2>&1", table_name))
    file.remove(sprintf("%s.aux", table_name), sprintf("%s.log", table_name))
    setwd("../R")
    
    cat(sprintf("  ✓ %s 生成完成\n\n", table_name))
}

# =============================================================================
# 特殊表格：S7 和 S8 (需要特殊格式)
# =============================================================================

generate_table_s7 <- function() {
    cat("[S7] 生成 Table S7 (Mediation Analysis)...\n")
    
    csv_file <- "../tables/supplementary/Table_S7_Mediation_Analysis.csv"
    if (!file.exists(csv_file)) {
        cat(sprintf("  ⚠ 文件不存在，跳过: %s\n", csv_file))
        return(NULL)
    }
    table_data <- read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE)
    
    # 转义特殊字符
    for (col in names(table_data)) {
        table_data[[col]] <- gsub("&", "\\\\&", table_data[[col]])
        table_data[[col]] <- gsub("%", "\\\\%", table_data[[col]])
    }
    
    # 用\mbox包裹置信区间以防止换行
    for (i in 1:nrow(table_data)) {
        if (grepl("\\[", table_data$Indirect_Effect[i])) {
            table_data$Indirect_Effect[i] <- paste0("\\mbox{", table_data$Indirect_Effect[i], "}")
        }
    }
    
    # 构建LaTeX表格
    latex_rows <- c()
    for (i in 1:nrow(table_data)) {
        row_values <- as.character(table_data[i, ])
        latex_rows <- c(latex_rows, paste(row_values, collapse = " & "))
    }
    
    table_body <- paste(latex_rows, collapse = " \\\\\n")
    header_row <- paste(sprintf("\\textbf{%s}", colnames(table_data)), collapse = " & ")
    
    table_title <- "Table S7. Mediation analysis"
    title_row <- paste0("\\multicolumn{", ncol(table_data), "}{@{}l}{\\textbf{", table_title, "}}\\\\[2mm]\n")
    table_note <- "Note: Mediation analysis of government health expenditure effect on life expectancy through healthcare access and quality. Indirect effect = Path coefficient × Mediator effect on outcome. 95% confidence intervals from bootstrap (1,000 iterations). All models include country and year fixed effects with clustered standard errors."
    
    latex_doc <- paste0(
        "\\documentclass[11pt]{article}\n",
        "\\usepackage[T1]{fontenc}\n",
        "\\usepackage[T1]{fontenc}
        \usepackage[margin=0.15in]{geometry}\n",
        "\\usepackage{booktabs}\n",
        "\\usepackage{array}\n",
        "\\usepackage{amsmath}\n",
        "\\usepackage{helvet}\n",
        "\\renewcommand{\\familydefault}{\\sfdefault}\n",
        "\\begin{document}\n\n",
        "\\newsavebox{\\tablebox}\n",
        "\\begin{table}[h!]\n",
        "\\centering\n",
        "\\setlength{\\tabcolsep}{6pt}\n",
        "\\savebox{\\tablebox}{\n",
        "{\\fontsize{8}{10}\\selectfont\n",
        "\\begin{tabular}{@{}lcc@{}}\n",
        title_row,
        "\\toprule\n",
        header_row, " \\\\\n",
        "\\midrule\n",
        table_body, " \\\\\n",
        "\\bottomrule\n",
        "\\end{tabular}\n",
        "}}\n",
        "\\usebox{\\tablebox}\n\n",
        "\\vspace{2mm}\n\n",
        "\\begin{minipage}{\\wd\\tablebox}\n",
        "{\\fontsize{7}{9}\\selectfont\\textit{", table_note, "}}\n",
        "\\end{minipage}\n",
        "\\end{table}\n\n",
        "\\end{document}\n"
    )
    
    tex_file <- "../tables/supplementary/Table_S7_Mediation_Analysis.tex"
    writeLines(latex_doc, tex_file)
    
    # 编译PDF
    setwd("../tables/supplementary")
    system("pdflatex -interaction=nonstopmode Table_S7_Mediation_Analysis.tex > /dev/null 2>&1")
    system("pdflatex -interaction=nonstopmode Table_S7_Mediation_Analysis.tex > /dev/null 2>&1")
    file.remove("Table_S7_Mediation_Analysis.aux", "Table_S7_Mediation_Analysis.log")
    setwd("../R")
    
    cat("  ✓ Table S7 生成完成\n\n")
}

generate_table_s8 <- function() {
    cat("[S8] 生成 Table S8 (Mediation Analysis)...\n")
    
    csv_file <- "../tables/supplementary/Table_S8_Mediation_Analysis.csv"
    if (!file.exists(csv_file)) {
        cat(sprintf("  ⚠ 文件不存在，跳过: %s\n", csv_file))
        return(NULL)
    }
    table_data <- read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE)
    
    # 转义特殊字符
    for (col in names(table_data)) {
        table_data[[col]] <- gsub("&", "\\\\&", table_data[[col]])
        table_data[[col]] <- gsub("%", "\\\\%", table_data[[col]])
        table_data[[col]] <- gsub("_", "\\\\_", table_data[[col]])
    }
    
    # 用\mbox包裹置信区间以防止换行
    ci_col <- grep("Indirect Effect|CI", colnames(table_data), value = TRUE)[1]
    if (!is.na(ci_col) && ci_col %in% colnames(table_data)) {
        for (i in 1:nrow(table_data)) {
            if (grepl("\\[", table_data[[ci_col]][i])) {
                table_data[[ci_col]][i] <- paste0("\\mbox{", table_data[[ci_col]][i], "}")
            }
        }
    }
    
    # 构建LaTeX表格
    latex_rows <- c()
    for (i in 1:nrow(table_data)) {
        row_values <- as.character(table_data[i, ])
        latex_rows <- c(latex_rows, paste(row_values, collapse = " & "))
    }
    
    table_body <- paste(latex_rows, collapse = " \\\\\n")
    header_row <- paste(sprintf("\\textbf{%s}", colnames(table_data)), collapse = " & ")
    
    table_title <- "Table S8. Mediation analysis"
    title_row <- paste0("\\multicolumn{", ncol(table_data), "}{@{}l}{\\textbf{", table_title, "}}\\\\[2mm]\n")
    table_note <- "Note: Mediation analysis of government health expenditure effect on life expectancy through healthcare access and quality mediators. Path a (GHE→M) = direct effect of GHE on mediator; Path b (M→Y) = direct effect of mediator on life expectancy. Indirect effect = Path a × Path b, calculated using product-of-coefficients method. 95% confidence intervals calculated using Delta method. Standard errors in parentheses. Statistical significance: *** p<0.001, ** p<0.01, * p<0.05. All models include country and year fixed effects with clustered standard errors."
    
    latex_doc <- paste0(
        "\\documentclass[11pt]{article}\n",
        "\\usepackage[T1]{fontenc}\n",
        "\\usepackage[T1]{fontenc}
        \usepackage[margin=0.15in]{geometry}\n",
        "\\usepackage{booktabs}\n",
        "\\usepackage{array}\n",
        "\\usepackage{amsmath}\n",
        "\\usepackage{helvet}\n",
        "\\renewcommand{\\familydefault}{\\sfdefault}\n",
        "\\begin{document}\n\n",
        "\\newsavebox{\\tablebox}\n",
        "\\begin{table}[h!]\n",
        "\\centering\n",
        "\\setlength{\\tabcolsep}{6pt}\n",
        "\\savebox{\\tablebox}{\n",
        "{\\fontsize{8}{10}\\selectfont\n",
        "\\begin{tabular}{@{}lcc@{}}\n",
        title_row,
        "\\toprule\n",
        header_row, " \\\\\n",
        "\\midrule\n",
        table_body, " \\\\\n",
        "\\bottomrule\n",
        "\\end{tabular}\n",
        "}}\n",
        "\\usebox{\\tablebox}\n\n",
        "\\vspace{2mm}\n\n",
        "\\begin{minipage}{\\wd\\tablebox}\n",
        "{\\fontsize{6}{8}\\selectfont\\textit{", table_note, "}}\n",
        "\\end{minipage}\n",
        "\\end{table}\n\n",
        "\\end{document}\n"
    )
    
    tex_file <- "../tables/supplementary/Table_S8_Mediation_Analysis.tex"
    writeLines(latex_doc, tex_file)
    
    # 编译PDF
    setwd("../tables/supplementary")
    system("pdflatex -interaction=nonstopmode Table_S8_Mediation_Analysis.tex > /dev/null 2>&1")
    system("pdflatex -interaction=nonstopmode Table_S8_Mediation_Analysis.tex > /dev/null 2>&1")
    file.remove("Table_S8_Mediation_Analysis.aux", "Table_S8_Mediation_Analysis.log")
    setwd("../R")
    
    cat("  ✓ Table S8 生成完成\n\n")
}

generate_table_s6 <- function() {
    cat("[S6] 生成 Table S6 (Heterogeneity Region) with right alignment...\n")
    
    csv_file <- "../tables/supplementary/Table_S6_Heterogeneity_Region.csv"
    if (!file.exists(csv_file)) {
        cat(sprintf("  ⚠ 文件不存在，跳过: %s\n", csv_file))
        return(NULL)
    }
    table_data <- read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE)
    
    # 转义特殊字符
    colnames(table_data) <- gsub("_", "\\\\_", colnames(table_data))
    for (col in names(table_data)) {
        table_data[[col]] <- gsub("&", "\\\\&", table_data[[col]])
        table_data[[col]] <- gsub("%", "\\\\%", table_data[[col]])
    }
    
    # 合并标准误行
    merged_rows <- list()
    i <- 1
    while (i <= nrow(table_data)) {
        if (i < nrow(table_data)) {
            next_row <- table_data[i + 1, ]
            is_se <- any(grepl("^\\(", as.character(next_row[-1])))
            
            if (is_se) {
                merged_row <- table_data[i, ]
                for (col_idx in 2:ncol(table_data)) {
                    coef <- as.character(table_data[i, col_idx])
                    se <- as.character(table_data[i + 1, col_idx])
                    coef_valid <- !is.na(coef) && coef != "" && coef != "NA"
                    se_valid <- !is.na(se) && se != "" && se != "NA"
                    if (coef_valid && se_valid) {
                        merged_row[col_idx] <- sprintf("%s %s", coef, se)
                    } else if (coef_valid) {
                        merged_row[col_idx] <- coef
                    } else {
                        merged_row[col_idx] <- ""
                    }
                }
                merged_rows[[length(merged_rows) + 1]] <- merged_row
                i <- i + 2
            } else {
                merged_rows[[length(merged_rows) + 1]] <- table_data[i, ]
                i <- i + 1
            }
        } else {
            merged_rows[[length(merged_rows) + 1]] <- table_data[i, ]
            i <- i + 1
        }
    }
    
    table_data_merged <- do.call(rbind, merged_rows)
    rownames(table_data_merged) <- NULL
    
    # 移除NA
    for (col in names(table_data_merged)) {
        table_data_merged[[col]][table_data_merged[[col]] == "NA"] <- ""
    }
    
    # 构建LaTeX表格行 - 右对齐
    latex_rows <- c()
    for (i in 1:nrow(table_data_merged)) {
        row_values <- as.character(table_data_merged[i, ])
        latex_rows <- c(latex_rows, paste(row_values, collapse = " & "))
    }
    
    table_body <- paste(latex_rows, collapse = " \\\\\n")
    header_row <- paste(sprintf("\\textbf{%s}", colnames(table_data_merged)), collapse = " & ")
    
    table_title <- "Table S6. Heterogeneity analysis by geographic region"
    table_note <- "Note: Heterogeneity analysis by WHO geographic region. Standard errors in parentheses (clustered at country level). *** p\\textless0.01, ** p\\textless0.05, * p\\textless0.1. Regions defined by WHO classification. All models include country and year fixed effects. Sample sizes vary by region availability."
    
    latex_doc <- paste0(
        "\\documentclass[11pt]{article}\n",
        "\\usepackage[T1]{fontenc}\n",
        "\\usepackage[T1]{fontenc}
        \usepackage[margin=0.15in]{geometry}\n",
        "\\usepackage{booktabs}\n",
        "\\usepackage{array}\n",
        "\\usepackage{helvet}\n",
        "\\renewcommand{\\familydefault}{\\sfdefault}\n",
        "\\begin{document}\n\n",
        "\\newsavebox{\\tablebox}\n",
        "\\begin{table}[h!]\n",
        "\\noindent{\\fontsize{8}{10}\\selectfont\\textbf{", table_title, "}}\\\\[2mm]\n",
        "\\noindent\\savebox{\\tablebox}{\n",
        "{\\fontsize{8}{10}\\selectfont\n",
        "\\setlength{\\tabcolsep}{5pt}\n",
        "\\begin{tabular}{@{}p{4.5cm}rr@{}}\n",  # Right-aligned columns for data
        "\\toprule\n",
        header_row, " \\\\\n",
        "\\midrule\n",
        table_body, " \\\\\n",
        "\\bottomrule\n",
        "\\end{tabular}\n",
        "}}\n",
        "\\usebox{\\tablebox}\n\n",
        "\\vspace{2mm}\n\n",
        "\\noindent\\begin{minipage}{\\wd\\tablebox}\n",
        "{\\fontsize{7}{9}\\selectfont\\itshape ", table_note, "}\n",
        "\\end{minipage}\n",
        "\\end{table}\n\n",
        "\\end{document}\n"
    )
    
    tex_file <- "../tables/supplementary/Table_S6_Heterogeneity_Region.tex"
    writeLines(latex_doc, tex_file)
    
    # 编译PDF
    setwd("../tables/supplementary")
    system("pdflatex -interaction=nonstopmode Table_S6_Heterogeneity_Region.tex > /dev/null 2>&1")
    system("pdflatex -interaction=nonstopmode Table_S6_Heterogeneity_Region.tex > /dev/null 2>&1")
    file.remove("Table_S6_Heterogeneity_Region.aux", "Table_S6_Heterogeneity_Region.log")
    setwd("../R")
    
    cat("  ✓ Table S6 生成完成\n\n")
}

generate_table_s5 <- function() {
    cat("[S5] 生成 Table S5 (Heterogeneity Income) with decimal alignment...\n")
    
    csv_file <- "../tables/supplementary/Table_S5_Heterogeneity_Income.csv"
    if (!file.exists(csv_file)) {
        cat(sprintf("  ⚠ 文件不存在，跳过: %s\n", csv_file))
        return(NULL)
    }
    table_data <- read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE)
    
    # 转义特殊字符
    colnames(table_data) <- gsub("_", "\\\\_", colnames(table_data))
    for (col in names(table_data)) {
        table_data[[col]] <- gsub("&", "\\\\&", table_data[[col]])
        table_data[[col]] <- gsub("%", "\\\\%", table_data[[col]])
    }
    
    # 合并标准误行
    merged_rows <- list()
    i <- 1
    while (i <= nrow(table_data)) {
        if (i < nrow(table_data)) {
            next_row <- table_data[i + 1, ]
            is_se <- any(grepl("^\\(", as.character(next_row[-1])))
            
            if (is_se) {
                merged_row <- table_data[i, ]
                for (col_idx in 2:ncol(table_data)) {
                    coef <- as.character(table_data[i, col_idx])
                    se <- as.character(table_data[i + 1, col_idx])
                    coef_valid <- !is.na(coef) && coef != "" && coef != "NA"
                    se_valid <- !is.na(se) && se != "" && se != "NA"
                    if (coef_valid && se_valid) {
                        merged_row[col_idx] <- sprintf("%s %s", coef, se)
                    } else if (coef_valid) {
                        merged_row[col_idx] <- coef
                    } else {
                        merged_row[col_idx] <- ""
                    }
                }
                merged_rows[[length(merged_rows) + 1]] <- merged_row
                i <- i + 2
            } else {
                merged_rows[[length(merged_rows) + 1]] <- table_data[i, ]
                i <- i + 1
            }
        } else {
            merged_rows[[length(merged_rows) + 1]] <- table_data[i, ]
            i <- i + 1
        }
    }
    
    table_data_merged <- do.call(rbind, merged_rows)
    rownames(table_data_merged) <- NULL
    
    # 移除NA
    for (col in names(table_data_merged)) {
        table_data_merged[[col]][table_data_merged[[col]] == "NA"] <- ""
    }
    
    # 构建LaTeX表格行 - 处理小数点对齐
    latex_rows <- c()
    for (i in 1:nrow(table_data_merged)) {
        income_group <- as.character(table_data_merged[i, 1])
        ghe_coef_text <- as.character(table_data_merged[i, 2])
        obs_text <- as.character(table_data_merged[i, 3])
        
        # For GHE coefficient, wrap in multicolumn to allow text with parens in d column
        if (grepl("\\(", ghe_coef_text)) {
            # Has parentheses - use multicolumn
            ghe_coef_formatted <- paste0("\\multicolumn{1}{c}{", ghe_coef_text, "}")
        } else if (ghe_coef_text != "" && ghe_coef_text != "NA") {
            # Pure number
            ghe_coef_formatted <- ghe_coef_text
        } else {
            ghe_coef_formatted <- ""
        }
        
        row_values <- c(income_group, ghe_coef_formatted, obs_text)
        latex_rows <- c(latex_rows, paste(row_values, collapse = " & "))
    }
    
    table_body <- paste(latex_rows, collapse = " \\\\\n")
    
    # Header with multicolumn
    header_parts <- c(
        "\\textbf{Income Group}",
        "\\multicolumn{1}{c}{\\textbf{GHE Coefficient}}",
        "\\multicolumn{1}{c}{\\textbf{Observations}}"
    )
    header_row <- paste(header_parts, collapse = " & ")
    
    table_title <- "Table S5. Heterogeneity analysis by income level"
    table_note <- "Note: Heterogeneity analysis by World Bank income classification. Standard errors in parentheses (clustered at country level). *** p\\textless0.01, ** p\\textless0.05, * p\\textless0.1. Low income: GNI per capita \\textless\\$1,085; Lower middle: \\$1,086--\\$4,255; Upper middle: \\$4,256--\\$13,205; High income: \\textgreater\\$13,205 (2022 thresholds). All models include country and year fixed effects."
    
    latex_doc <- paste0(
        "\\documentclass[11pt]{article}\n",
        "\\usepackage[T1]{fontenc}\n",
        "\\usepackage[T1]{fontenc}
        \usepackage[margin=0.15in]{geometry}\n",
        "\\usepackage{booktabs}\n",
        "\\usepackage{array}\n",
        "\\usepackage{dcolumn}\n",
        "\\newcolumntype{d}[1]{D{.}{.}{#1}}\n",
        "\\usepackage{helvet}\n",
        "\\renewcommand{\\familydefault}{\\sfdefault}\n",
        "% Make dcolumn use Helvetica font for numbers\n",
        "\\DeclareSymbolFont{SFdigits}{OT1}{phv}{m}{n}\n",
        "\\DeclareMathSymbol{0}{\\mathalpha}{SFdigits}{`0}\n",
        "\\DeclareMathSymbol{1}{\\mathalpha}{SFdigits}{`1}\n",
        "\\DeclareMathSymbol{2}{\\mathalpha}{SFdigits}{`2}\n",
        "\\DeclareMathSymbol{3}{\\mathalpha}{SFdigits}{`3}\n",
        "\\DeclareMathSymbol{4}{\\mathalpha}{SFdigits}{`4}\n",
        "\\DeclareMathSymbol{5}{\\mathalpha}{SFdigits}{`5}\n",
        "\\DeclareMathSymbol{6}{\\mathalpha}{SFdigits}{`6}\n",
        "\\DeclareMathSymbol{7}{\\mathalpha}{SFdigits}{`7}\n",
        "\\DeclareMathSymbol{8}{\\mathalpha}{SFdigits}{`8}\n",
        "\\DeclareMathSymbol{9}{\\mathalpha}{SFdigits}{`9}\n",
        "\\DeclareMathSymbol{.}{\\mathalpha}{SFdigits}{`.}\n",
        "\\DeclareMathSymbol{-}{\\mathalpha}{SFdigits}{`-}\n",
        "\\DeclareMathSymbol{(}{\\mathalpha}{SFdigits}{`(}\n",
        "\\DeclareMathSymbol{)}{\\mathalpha}{SFdigits}{`)}\n",
        "\\DeclareMathSymbol{,}{\\mathalpha}{SFdigits}{`,}\n",
        "\\begin{document}\n\n",
        "\\newsavebox{\\tablebox}\n",
        "\\begin{table}[h!]\n",
        "\\noindent{\\fontsize{8}{10}\\selectfont\\textbf{", table_title, "}}\\\\[2mm]\n",
        "\\noindent\\savebox{\\tablebox}{\n",
        "{\\fontsize{8}{10}\\selectfont\n",
        "\\setlength{\\tabcolsep}{5pt}\n",
        "\\begin{tabular}{@{}p{3.0cm}d{2.3}d{5.0}@{}}\n",  # Both numeric columns use d for alignment
        "\\toprule\n",
        header_row, " \\\\\n",
        "\\midrule\n",
        table_body, " \\\\\n",
        "\\bottomrule\n",
        "\\end{tabular}\n",
        "}}\n",
        "\\usebox{\\tablebox}\n\n",
        "\\vspace{2mm}\n\n",
        "\\noindent\\begin{minipage}{\\wd\\tablebox}\n",
        "{\\fontsize{7}{9}\\selectfont\\itshape ", table_note, "}\n",
        "\\end{minipage}\n",
        "\\end{table}\n\n",
        "\\end{document}\n"
    )
    
    tex_file <- "../tables/supplementary/Table_S5_Heterogeneity_Income.tex"
    writeLines(latex_doc, tex_file)
    
    # 编译PDF
    setwd("../tables/supplementary")
    system("pdflatex -interaction=nonstopmode Table_S5_Heterogeneity_Income.tex > /dev/null 2>&1")
    system("pdflatex -interaction=nonstopmode Table_S5_Heterogeneity_Income.tex > /dev/null 2>&1")
    file.remove("Table_S5_Heterogeneity_Income.aux", "Table_S5_Heterogeneity_Income.log")
    setwd("../R")
    
    cat("  ✓ Table S5 生成完成\n\n")
}

generate_table_s4 <- function() {
    cat("[S4] 生成 Table S4 (IV Validity Tests)...\n")
    
    csv_file <- "../tables/supplementary/Table_S4_IV_Validity_Tests.csv"
    if (!file.exists(csv_file)) {
        cat(sprintf("  ⚠ 文件不存在，跳过: %s\n", csv_file))
        return(NULL)
    }
    table_data <- read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE)
    
    # 转义特殊字符
    table_data$Test <- gsub("%", "\\\\%", table_data$Test)
    table_data$Test <- gsub("_", "\\\\_", table_data$Test)
    table_data$Test <- gsub("&", "\\\\&", table_data$Test)
    
    # 构建LaTeX表格行 - 使用dcolumn格式（数字对齐小数点）
    latex_rows <- c()
    for (i in 1:nrow(table_data)) {
        # Check if row contains numeric values
        val_fiscal <- suppressWarnings(as.numeric(table_data$IV_Fiscal[i]))
        val_gov <- suppressWarnings(as.numeric(table_data$IV_Governance[i]))
        val_geo <- suppressWarnings(as.numeric(table_data$IV_Geographic[i]))
        
        if (!is.na(val_fiscal) && !is.na(val_gov) && !is.na(val_geo)) {
            # Numeric row
            row_values <- c(
                table_data$Test[i],
                formatC(val_fiscal, format="f", digits=2),
                formatC(val_gov, format="f", digits=2),
                formatC(val_geo, format="f", digits=2)
            )
        } else {
            # Text row
            row_values <- c(
                table_data$Test[i],
                paste0("\\multicolumn{1}{c}{", table_data$IV_Fiscal[i], "}"),
                paste0("\\multicolumn{1}{c}{", table_data$IV_Governance[i], "}"),
                paste0("\\multicolumn{1}{c}{", table_data$IV_Geographic[i], "}")
            )
        }
        latex_rows <- c(latex_rows, paste(row_values, collapse = " & "))
    }
    
    table_body <- paste(latex_rows, collapse = " \\\\\n")
    
    # Header with multicolumn for D columns - 增加列宽确保能一行显示
    header_parts <- c(
        "\\textbf{Test}",
        "\\multicolumn{1}{c}{\\textbf{IV Fiscal}}",
        "\\multicolumn{1}{c}{\\textbf{IV Governance}}",
        "\\multicolumn{1}{c}{\\textbf{IV Geographic}}"
    )
    header_row <- paste(header_parts, collapse = " & ")
    
    table_title <- "Table S4. Instrumental variables validity tests"
    table_note <- "Note: Diagnostic tests for instrumental variable validity. First-stage F-statistic tests instrument strength (critical value ≈10). Kleibergen-Paap rk Wald F-statistic for weak identification. Hansen J-statistic tests overidentifying restrictions (p\\textgreater\\,0.10 indicates valid instruments). Endogeneity test (Wu-Hausman) compares IV and OLS (p\\textless\\,0.05 indicates endogeneity). All models include country and year fixed effects."
    
    latex_doc <- paste0(
        "\\documentclass[11pt]{article}\n",
        "\\usepackage[T1]{fontenc}\n",
        "\\usepackage[T1]{fontenc}
        \usepackage[margin=0.15in]{geometry}\n",
        "\\usepackage{booktabs}\n",
        "\\usepackage{array}\n",
        "\\usepackage{dcolumn}\n",
        "\\newcolumntype{d}[1]{D{.}{.}{#1}}\n",
        "\\usepackage{helvet}\n",
        "\\renewcommand{\\familydefault}{\\sfdefault}\n",
        "% Make dcolumn use Helvetica font for numbers\n",
        "\\DeclareSymbolFont{SFdigits}{OT1}{phv}{m}{n}\n",
        "\\DeclareMathSymbol{0}{\\mathalpha}{SFdigits}{`0}\n",
        "\\DeclareMathSymbol{1}{\\mathalpha}{SFdigits}{`1}\n",
        "\\DeclareMathSymbol{2}{\\mathalpha}{SFdigits}{`2}\n",
        "\\DeclareMathSymbol{3}{\\mathalpha}{SFdigits}{`3}\n",
        "\\DeclareMathSymbol{4}{\\mathalpha}{SFdigits}{`4}\n",
        "\\DeclareMathSymbol{5}{\\mathalpha}{SFdigits}{`5}\n",
        "\\DeclareMathSymbol{6}{\\mathalpha}{SFdigits}{`6}\n",
        "\\DeclareMathSymbol{7}{\\mathalpha}{SFdigits}{`7}\n",
        "\\DeclareMathSymbol{8}{\\mathalpha}{SFdigits}{`8}\n",
        "\\DeclareMathSymbol{9}{\\mathalpha}{SFdigits}{`9}\n",
        "\\DeclareMathSymbol{.}{\\mathalpha}{SFdigits}{`.}\n",
        "\\DeclareMathSymbol{-}{\\mathalpha}{SFdigits}{`-}\n",
        "\\begin{document}\n\n",
        "\\newsavebox{\\tablebox}\n",
        "\\begin{table}[h!]\n",
        "\\noindent{\\fontsize{8}{10}\\selectfont\\textbf{", table_title, "}}\\\\[2mm]\n",
        "\\noindent\\savebox{\\tablebox}{\n",
        "{\\fontsize{8}{10}\\selectfont\n",
        "\\setlength{\\tabcolsep}{5pt}\n",
        "\\begin{tabular}{@{}p{4.5cm}d{3.2}d{3.2}d{3.2}@{}}\n",  # Wider columns for headers
        "\\toprule\n",
        header_row, " \\\\\n",
        "\\midrule\n",
        table_body, " \\\\\n",
        "\\bottomrule\n",
        "\\end{tabular}\n",
        "}}\n",
        "\\usebox{\\tablebox}\n\n",
        "\\vspace{2mm}\n\n",
        "\\noindent\\begin{minipage}{\\wd\\tablebox}\n",
        "{\\fontsize{7}{9}\\selectfont\\itshape ", table_note, "}\n",
        "\\end{minipage}\n",
        "\\end{table}\n\n",
        "\\end{document}\n"
    )
    
    tex_file <- "../tables/supplementary/Table_S4_IV_Validity_Tests.tex"
    writeLines(latex_doc, tex_file)
    
    # 编译PDF
    setwd("../tables/supplementary")
    system("pdflatex -interaction=nonstopmode Table_S4_IV_Validity_Tests.tex > /dev/null 2>&1")
    system("pdflatex -interaction=nonstopmode Table_S4_IV_Validity_Tests.tex > /dev/null 2>&1")
    file.remove("Table_S4_IV_Validity_Tests.aux", "Table_S4_IV_Validity_Tests.log")
    setwd("../R")
    
    cat("  ✓ Table S4 生成完成\n\n")
}

generate_table_s2 <- function() {
    cat("[S2] 生成 Table S2 (Correlation Matrix)...\n")
    
    csv_file <- "../tables/supplementary/Table_S2_Correlation_Matrix.csv"
    if (!file.exists(csv_file)) {
        cat(sprintf("  ⚠ 文件不存在，跳过: %s\n", csv_file))
        return(NULL)
    }
    table_data <- read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE)
    
    # 转义特殊字符
    table_data$Variable <- gsub("%", "\\\\%", table_data$Variable)
    table_data$Variable <- gsub("_", "\\\\_", table_data$Variable)
    table_data$Variable <- gsub("&", "\\\\&", table_data$Variable)
    
    # 构建LaTeX表格行 - 使用dcolumn格式（数字对齐小数点）
    latex_rows <- c()
    for (i in 1:nrow(table_data)) {
        row_values <- c(
            table_data$Variable[i],
            formatC(table_data[[2]][i], format="f", digits=3),
            formatC(table_data[[3]][i], format="f", digits=3),
            formatC(table_data[[4]][i], format="f", digits=3),
            formatC(table_data[[5]][i], format="f", digits=3),
            formatC(table_data[[6]][i], format="f", digits=3),
            formatC(table_data[[7]][i], format="f", digits=3)
        )
        latex_rows <- c(latex_rows, paste(row_values, collapse = " & "))
    }
    
    table_body <- paste(latex_rows, collapse = " \\\\\n")
    
    # Header with multicolumn for D columns - 确保能一行显示
    header_parts <- c(
        "\\textbf{Variable}",
        "\\multicolumn{1}{c}{\\textbf{Life exp}}",
        "\\multicolumn{1}{c}{\\textbf{GHE/GDP}}",
        "\\multicolumn{1}{c}{\\textbf{Log GDP pc}}",
        "\\multicolumn{1}{c}{\\textbf{Log pop}}",
        "\\multicolumn{1}{c}{\\textbf{Gov eff}}",
        "\\multicolumn{1}{c}{\\textbf{Rule law}}"
    )
    header_row <- paste(header_parts, collapse = " & ")
    
    table_title <- "Table S2. Correlation matrix of key variables"
    table_note <- "Note: Pearson correlation coefficients for key variables. N=4,439 country-year observations. *** p\\textless0.01, ** p\\textless0.05, * p\\textless0.1. GHE = Government Health Expenditure as \\% of GDP; GDP = GDP per capita (log); Gov eff = Government Effectiveness; Rule law = Rule of Law; Life exp = Life Expectancy; Log pop = Log Population."
    
    latex_doc <- paste0(
        "\\documentclass[11pt]{article}\n",
        "\\usepackage[T1]{fontenc}\n",
        "\\usepackage[T1]{fontenc}
        \usepackage[margin=0.15in]{geometry}\n",
        "\\usepackage{booktabs}\n",
        "\\usepackage{array}\n",
        "\\usepackage{dcolumn}\n",
        "\\newcolumntype{d}[1]{D{.}{.}{#1}}\n",
        "\\usepackage{helvet}\n",
        "\\renewcommand{\\familydefault}{\\sfdefault}\n",
        "% Make dcolumn use Helvetica font for numbers\n",
        "\\DeclareSymbolFont{SFdigits}{OT1}{phv}{m}{n}\n",
        "\\DeclareMathSymbol{0}{\\mathalpha}{SFdigits}{`0}\n",
        "\\DeclareMathSymbol{1}{\\mathalpha}{SFdigits}{`1}\n",
        "\\DeclareMathSymbol{2}{\\mathalpha}{SFdigits}{`2}\n",
        "\\DeclareMathSymbol{3}{\\mathalpha}{SFdigits}{`3}\n",
        "\\DeclareMathSymbol{4}{\\mathalpha}{SFdigits}{`4}\n",
        "\\DeclareMathSymbol{5}{\\mathalpha}{SFdigits}{`5}\n",
        "\\DeclareMathSymbol{6}{\\mathalpha}{SFdigits}{`6}\n",
        "\\DeclareMathSymbol{7}{\\mathalpha}{SFdigits}{`7}\n",
        "\\DeclareMathSymbol{8}{\\mathalpha}{SFdigits}{`8}\n",
        "\\DeclareMathSymbol{9}{\\mathalpha}{SFdigits}{`9}\n",
        "\\DeclareMathSymbol{.}{\\mathalpha}{SFdigits}{`.}\n",
        "\\DeclareMathSymbol{-}{\\mathalpha}{SFdigits}{`-}\n",
        "\\begin{document}\n\n",
        "\\newsavebox{\\tablebox}\n",
        "\\begin{table}[h!]\n",
        "\\noindent{\\fontsize{8}{10}\\selectfont\\textbf{", table_title, "}}\\\\[2mm]\n",
        "\\noindent\\savebox{\\tablebox}{\n",
        "{\\fontsize{8}{10}\\selectfont\n",
        "\\setlength{\\tabcolsep}{4pt}\n",
        "\\begin{tabular}{@{}p{3.5cm}*{6}{d{2.3}}@{}}\n",  # 6 numeric columns with decimal alignment
        "\\toprule\n",
        header_row, " \\\\\n",
        "\\midrule\n",
        table_body, " \\\\\n",
        "\\bottomrule\n",
        "\\end{tabular}\n",
        "}}\n",
        "\\usebox{\\tablebox}\n\n",
        "\\vspace{2mm}\n\n",
        "\\noindent\\begin{minipage}{\\wd\\tablebox}\n",
        "{\\fontsize{7}{9}\\selectfont\\itshape ", table_note, "}\n",
        "\\end{minipage}\n",
        "\\end{table}\n\n",
        "\\end{document}\n"
    )
    
    tex_file <- "../tables/supplementary/Table_S2_Correlation_Matrix.tex"
    writeLines(latex_doc, tex_file)
    
    # 编译PDF
    setwd("../tables/supplementary")
    system("pdflatex -interaction=nonstopmode Table_S2_Correlation_Matrix.tex > /dev/null 2>&1")
    system("pdflatex -interaction=nonstopmode Table_S2_Correlation_Matrix.tex > /dev/null 2>&1")
    file.remove("Table_S2_Correlation_Matrix.aux", "Table_S2_Correlation_Matrix.log")
    setwd("../R")
    
    cat("  ✓ Table S2 生成完成\n\n")
}

generate_table_s1 <- function() {
    cat("[S1] 生成 Table S1 (Manual with decimal alignment)...\n")
    
    csv_file <- "../tables/supplementary/Table_S1_Complete_Descriptive_Statistics.csv"
    if (!file.exists(csv_file)) {
        cat(sprintf("  ⚠ 文件不存在，跳过: %s\n", csv_file))
        return(NULL)
    }
    table_data <- read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE)
    
    # 转义特殊字符
    table_data$Variable <- gsub("%", "\\\\%", table_data$Variable)
    table_data$Variable <- gsub("_", "\\\\_", table_data$Variable)
    table_data$Variable <- gsub("&", "\\\\&", table_data$Variable)
    
    # 构建LaTeX表格行 - 使用dcolumn格式（数字对齐小数点）
    latex_rows <- c()
    for (i in 1:nrow(table_data)) {
        row_values <- c(
            table_data$Variable[i],
            as.character(table_data$N[i]),  # dcolumn will handle formatting
            formatC(table_data$Mean[i], format="f", digits=2),
            formatC(table_data$SD[i], format="f", digits=2),
            formatC(table_data$Min[i], format="f", digits=2),
            formatC(table_data$P25[i], format="f", digits=2),
            formatC(table_data$Median[i], format="f", digits=2),
            formatC(table_data$P75[i], format="f", digits=2),
            formatC(table_data$Max[i], format="f", digits=2)
        )
        latex_rows <- c(latex_rows, paste(row_values, collapse = " & "))
    }
    
    table_body <- paste(latex_rows, collapse = " \\\\\n")
    
    # Header with multicolumn for D columns
    header_parts <- c(
        "\\textbf{Variable}",
        "\\multicolumn{1}{c}{\\textbf{N}}",
        "\\multicolumn{1}{c}{\\textbf{Mean}}",
        "\\multicolumn{1}{c}{\\textbf{SD}}",
        "\\multicolumn{1}{c}{\\textbf{Min}}",
        "\\multicolumn{1}{c}{\\textbf{P25}}",
        "\\multicolumn{1}{c}{\\textbf{Median}}",
        "\\multicolumn{1}{c}{\\textbf{P75}}",
        "\\multicolumn{1}{c}{\\textbf{Max}}"
    )
    header_row <- paste(header_parts, collapse = " & ")
    
    table_title <- "Table S1. Complete descriptive statistics"
    table_note <- "Note: Complete descriptive statistics for all variables used in the analysis (2000-2022). Sample size varies by variable due to data availability: Life expectancy and Log population have 4,370 observations from 190 countries; GHE/GDP and CHE/GDP have 4,310 observations; Governance indicators have 4,092-4,168 observations from 190 countries. CHE = Current Health Expenditure; GHE = Government Health Expenditure; GDP per capita in thousand USD (constant 2015 prices). Governance indicators range from -2.5 (weak) to 2.5 (strong)."
    
    latex_doc <- paste0(
        "\\documentclass[11pt]{article}\n",
        "\\usepackage[T1]{fontenc}\n",
        "\\usepackage[margin=0.15in]{geometry}\n",
        "\\usepackage{booktabs}\n",
        "\\usepackage{array}\n",
        "\\usepackage{dcolumn}\n",
        "\\newcolumntype{d}[1]{D{.}{.}{#1}}\n",
        "\\usepackage{helvet}\n",
        "\\renewcommand{\\familydefault}{\\sfdefault}\n",
        "% Make dcolumn use Helvetica font for numbers\n",
        "\\DeclareSymbolFont{SFdigits}{OT1}{phv}{m}{n}\n",
        "\\DeclareMathSymbol{0}{\\mathalpha}{SFdigits}{`0}\n",
        "\\DeclareMathSymbol{1}{\\mathalpha}{SFdigits}{`1}\n",
        "\\DeclareMathSymbol{2}{\\mathalpha}{SFdigits}{`2}\n",
        "\\DeclareMathSymbol{3}{\\mathalpha}{SFdigits}{`3}\n",
        "\\DeclareMathSymbol{4}{\\mathalpha}{SFdigits}{`4}\n",
        "\\DeclareMathSymbol{5}{\\mathalpha}{SFdigits}{`5}\n",
        "\\DeclareMathSymbol{6}{\\mathalpha}{SFdigits}{`6}\n",
        "\\DeclareMathSymbol{7}{\\mathalpha}{SFdigits}{`7}\n",
        "\\DeclareMathSymbol{8}{\\mathalpha}{SFdigits}{`8}\n",
        "\\DeclareMathSymbol{9}{\\mathalpha}{SFdigits}{`9}\n",
        "\\DeclareMathSymbol{.}{\\mathalpha}{SFdigits}{`.}\n",
        "\\DeclareMathSymbol{-}{\\mathalpha}{SFdigits}{`-}\n",
        "\\pagestyle{empty}\n",
        "\\setlength{\\parindent}{0pt}\n",
        "\\setlength{\\parskip}{0pt}\n",
        "\\setlength{\\topskip}{0pt}\n",
        "\\setlength{\\textheight}{\\maxdimen}\n",
        "\\begin{document}\n",
        "\\vspace*{0pt}\n\n",
        "\\newsavebox{\\tablebox}\n",
        "\\noindent{\\fontsize{8}{10}\\selectfont\\textbf{", table_title, "}}\\\\[2mm]\n",
        "\\savebox{\\tablebox}{%\n",
        "\\noindent{\\fontsize{8}{10}\\selectfont\n",
        "\\setlength{\\tabcolsep}{4pt}\n",
        "\\begin{tabular}{@{}p{3.8cm}d{4.0}d{3.2}d{2.2}d{3.2}d{3.2}d{3.2}d{3.2}d{3.2}@{}}\n",
        "\\toprule\n",
        header_row, " \\\\\n",
        "\\midrule\n",
        table_body, " \\\\\n",
        "\\bottomrule\n",
        "\\end{tabular}%\n",
        "}%\n",
        "}\n",
        "\\usebox{\\tablebox}\\\\[2mm]\n",
        "\\begin{minipage}{\\wd\\tablebox}\n",
        "{\\fontsize{7}{9}\\selectfont\\itshape ", table_note, "}\n",
        "\\end{minipage}\n",
        "\\end{document}\n"
    )
    
    tex_file <- "../tables/supplementary/Table_S1_Complete_Descriptive_Statistics.tex"
    writeLines(latex_doc, tex_file)
    
    # 编译PDF
    setwd("../tables/supplementary")
    system("pdflatex -interaction=nonstopmode Table_S1_Complete_Descriptive_Statistics.tex > /dev/null 2>&1")
    system("pdflatex -interaction=nonstopmode Table_S1_Complete_Descriptive_Statistics.tex > /dev/null 2>&1")
    # 尝试使用pdfcrop裁剪PDF（如果可用）
    if (system("which pdfcrop > /dev/null 2>&1") == 0) {
        system("pdfcrop --margins '5' Table_S1_Complete_Descriptive_Statistics.pdf Table_S1_Complete_Descriptive_Statistics.pdf > /dev/null 2>&1")
    }
    file.remove("Table_S1_Complete_Descriptive_Statistics.aux", "Table_S1_Complete_Descriptive_Statistics.log")
    setwd("../R")
    
    cat("  ✓ Table S1 生成完成\n\n")
}

generate_table_s9 <- function() {
    cat("[S9] 生成 Table S9 (Country List and Summary)...\n")
    
    csv_file <- "../tables/supplementary/Table_S9_Country_List.csv"
    if (!file.exists(csv_file)) {
        cat(sprintf("  ⚠ 文件不存在，跳过: %s\n", csv_file))
        return(NULL)
    }
    table_data <- read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE)
    
    sanitize_tex <- function(x) {
        x <- gsub("&", "\\\\&", x)
        x <- gsub("%", "\\\\%", x)
        x <- gsub("_", "\\\\_", x)
        x
    }
    
    table_data <- table_data %>%
        mutate(
            iso3c = sanitize_tex(iso3c),
            country = sanitize_tex(country),
            Income_Group = sanitize_tex(Income_Group),
            Years_Covered = as.character(Years_Covered),
            N_Observations = as.character(N_Observations),
            Data_Completeness = sprintf("%.0f\\%%", Data_Completeness),
            Mean_Life_Exp = sprintf("%.2f", Mean_Life_Exp),
            Mean_GHE = sprintf("%.2f", Mean_GHE),
            Mean_GDP_PC = formatC(Mean_GDP_PC, format = "f", digits = 0, big.mark = ",")
        )
    
    build_longtable <- function(df, title, note, file_stub, col_widths_cm) {
        header_labels <- sanitize_tex(colnames(df))
        title_tex <- sanitize_tex(title)
        note_tex <- sanitize_tex(note)
        header_row <- paste(sprintf("\\textbf{%s}", header_labels), collapse = " & ")
        table_body <- apply(df, 1, function(row) paste(row, collapse = " & "))
        table_body <- paste(table_body, collapse = " \\\\\n")
        
        col_spec <- paste0(
            "@{}",
            paste(sprintf(">{\\raggedright\\arraybackslash}p{%.1fcm}", col_widths_cm), collapse = ""),
            "@{}"
        )
        
        latex_doc <- paste0(
            "\\documentclass[11pt]{article}\n",
            "\\usepackage[paperwidth=8.27in,paperheight=11.69in,margin=0.9in]{geometry}\n",
            "\\usepackage{booktabs}\n",
            "\\usepackage{longtable}\n",
            "\\usepackage{array}\n",
            "\\usepackage{helvet}\n",
            "\\renewcommand{\\familydefault}{\\sfdefault}\n",
            "\\begin{document}\n\n",
            "\\setlength{\\tabcolsep}{3pt}\n",
            "\\setlength{\\LTleft}{0pt}\n",
            "\\setlength{\\LTright}{0pt}\n",
            "\\small\n",
            "\\begin{longtable}{", col_spec, "}\n",
            "\\caption*{\\textbf{", title_tex, "}}\\\\[2mm]\n",
            "\\toprule\n",
            header_row, " \\\\\n",
            "\\midrule\n",
            "\\endfirsthead\n",
            "\\multicolumn{", ncol(df), "}{l}{\\textit{(Continued from previous page)}}\\\\\n",
            "\\toprule\n",
            header_row, " \\\\\n",
            "\\midrule\n",
            "\\endhead\n",
            "\\midrule\n",
            "\\multicolumn{", ncol(df), "}{r}{\\textit{Continued on next page}}\\\\\n",
            "\\endfoot\n",
            "\\bottomrule\n",
            "\\endlastfoot\n",
            table_body, " \\\\\n",
            "\\end{longtable}\n",
            "\\vspace{2mm}\n",
            "\\noindent{\\fontsize{7}{9}\\selectfont\\textit{", note_tex, "}}\n",
            "\\end{document}\n"
        )
        
        tex_file <- sprintf("../tables/supplementary/%s.tex", file_stub)
        pdf_file <- sprintf("../tables/supplementary/%s.pdf", file_stub)
        writeLines(latex_doc, tex_file)
        
        setwd("../tables/supplementary")
        system(sprintf("pdflatex -interaction=nonstopmode %s.tex > /dev/null 2>&1", file_stub))
        system(sprintf("pdflatex -interaction=nonstopmode %s.tex > /dev/null 2>&1", file_stub))
        file.remove(paste0(file_stub, ".aux"), paste0(file_stub, ".log"))
        setwd("../R")
    }
    
    table_part1 <- table_data %>%
        transmute(
            ISO3 = iso3c,
            Country = country,
            `Income group` = Income_Group,
            `Years covered` = Years_Covered,
            `Observations` = N_Observations,
            `Data completeness (%)` = Data_Completeness
        )
    
    table_part2 <- table_data %>%
        transmute(
            ISO3 = iso3c,
            Country = country,
            `Life expectancy (years)` = Mean_Life_Exp,
            `GHE (% GDP)` = Mean_GHE,
            `GDP per capita (USD)` = Mean_GDP_PC
        )
    
    build_longtable(
        table_part1,
        "Table S9A. Country coverage and completeness",
        "Income group follows World Bank FY2023 classification. Coverage statistics list study years, observed country-year pairs, and the percentage of available observations over 2000-2022.",
        "Table_S9A_Country_Coverage",
        c(1.4, 4.2, 3.2, 2.4, 2.4, 3.0)
    )
    
    build_longtable(
        table_part2,
        "Table S9B. Health outcomes and fiscal metrics",
        "Life expectancy averages are measured in years. GHE = government health expenditure as % of GDP. GDP per capita (USD) is reported in constant international dollars with comma separators.",
        "Table_S9B_Outcomes_Spending",
        c(1.4, 4.2, 3.5, 3.0, 3.5)
    )
    
    cat("  ✓ Table S9 A/B 生成完成\n\n")
}

# =============================================================================
# 生成所有附表
# =============================================================================

# Table S1
generate_table_s1()

# Table S2
generate_table_s2()

# Table S4 (Robustness Checks - renumbered after integration of S6-S7 into Table 3)
generate_table_with_merged_se(
    "S4",
    "Table_S4_Robustness_Checks",
    "Table S4. Robustness checks: alternative specifications",
    "Note: Robustness checks with alternative specifications. Standard errors in parentheses (clustered at country level). *** p $<$ 0.01, ** p $<$ 0.05, * p $<$ 0.1. Alternative DVs: Infant and maternal mortality rates. Alternative samples: Exclude high-income countries and COVID period. Alternative estimator: System GMM with lagged dependent variable.",
    col_types = c("l", "r", "c", "c")  # GHE_Coefficient列右对齐
)

# Table S5 (IV Validity Tests - renumbered)
generate_table_s4()

# Table S5
generate_table_s5()

# Table S6
generate_table_s6()

# Table S7
generate_table_s7()

# Table S8
generate_table_s8()

# Table S9
s9_csv <- "../tables/supplementary/Table_S9_Country_List.csv"
if (file.exists(s9_csv)) {
    generate_table_s9()
} else {
    cat("[S9] Table S9 数据文件不存在，跳过生成...\n\n")
}

# =============================================================================
# 总结
# =============================================================================

cat("==================================================\n")
cat("  ✅ 所有附表生成完成\n")
cat("==================================================\n\n")

cat("生成的附表：\n")
for (i in 1:9) {
    cat(sprintf("  %d. Table S%d\n", i, i))
}
cat("\n")
