# =============================================================================
# setup_fonts.R
# Font Setup and Configuration | 字体设置与配置
# =============================================================================
#
# 【功能概述 | Function Overview】
# 设置Arial字体用于所有图表，确保符合期刊标准
# Set up Arial font for all figures to meet journal standards
#
# 【使用方法 | Usage】
# source("tools/setup_fonts.R")
#
# 【最后更新 | Last Updated】
# 2025-11-07
# =============================================================================

cat("\n==================================================\n")
cat("  字体设置和检查\n")
cat("==================================================\n\n")

# 方法1：使用showtext包（推荐，更简单）
if (!require("showtext", quietly = TRUE)) {
    install.packages("showtext", repos = "https://cloud.r-project.org")
    library(showtext)
}

cat("[1] 检查showtext包...\n")
cat("  ✓ showtext包已加载\n\n")

# 启用showtext
showtext_auto()

# 在Mac上，sans字体通常就是Arial或Helvetica
# 我们可以直接使用系统默认的sans字体
cat("[2] 字体设置...\n")
cat("  使用系统默认sans字体（Mac上通常是Arial/Helvetica）\n")
cat("  在R中，使用 family = 'sans' 即可\n\n")

# 验证字体
cat("[3] 字体验证...\n")
cat("  在ggplot2中使用：family = 'sans'\n")
cat("  或者使用：family = 'Arial'（如果extrafont已导入）\n\n")

# 方法2：使用extrafont（备选）
cat("[4] 备选方案：extrafont包...\n")
if (require("extrafont", quietly = TRUE)) {
    tryCatch({
        loadfonts(device = "pdf")
        arial_fonts <- fonts()[grepl("Arial", fonts(), ignore.case = TRUE)]
        if (length(arial_fonts) > 0) {
            cat(sprintf("  ✓ 找到Arial字体：%s\n", paste(arial_fonts, collapse = ", ")))
        } else {
            cat("  ⚠ 未找到Arial字体，使用sans作为默认\n")
        }
    }, error = function(e) {
        cat("  ⚠ extrafont加载失败，使用sans作为默认\n")
    })
} else {
    cat("  ⚠ extrafont未安装，使用sans作为默认\n")
}

cat("\n==================================================\n")
cat("  推荐设置\n")
cat("==================================================\n\n")
cat("在ggplot2代码中使用：\n")
cat("  family = 'sans'  # Mac上通常是Arial/Helvetica\n")
cat("  或者\n")
cat("  family = 'Arial'  # 如果extrafont已正确导入\n\n")

cat("字体大小转换：\n")
cat("  ggplot2的size参数单位 ≈ pt / 2.835\n")
cat("  例如：12 pt = size = 12/2.835 ≈ 4.23\n\n")

cat("==================================================\n")
cat("  ✓ 字体设置完成\n")
cat("==================================================\n\n")

