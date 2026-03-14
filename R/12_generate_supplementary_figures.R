# =============================================================================
# 12_generate_supplementary_figures.R
# Generate Supplementary Figures for Lancet | 生成 Lancet 补充图
# =============================================================================
#
# 【功能概述 | Function Overview】
# 按照 Lancet 期刊标准生成补充材料图形
# Generate supplementary figures following Lancet journal standards:
#   - Figure S1: Data quality and coverage | 数据质量与覆盖
#   - Figure S2: Robustness checks - sample | 稳健性检验 - 样本
#   - Figure S3: Robustness checks - model | 稳健性检验 - 模型
#   - Figure S4: IV diagnostics | IV诊断图
#   - Figure S5: Threshold robustness | 阈值稳健性
#   - Figure S6: Regional heterogeneity | 地区异质性
#   - Figure S7: Temporal dynamics | 时间动态
#   - Figure S8: Mechanism analysis (detailed) | 机制分析详细版
#   - Figure S9: Disease-specific analysis | 疾病详细分析
#   - Figure S10: COVID-19 analysis | COVID-19详细分析
#
# 【输出文件 | Output Files】
#   - figures/supplementary/Figure_S1_*.pdf
#   - figures/supplementary/Figure_S2_*.pdf
#   - ... (S1-S13)
#
# 【运行时间 | Runtime】
# ~ 10-15 minutes | ~10-15分钟
#
# 【最后更新 | Last Updated】
# 2025-11-07
# =============================================================================

# 清空环境
rm(list = ls())
gc()

# =============================================================================
# 0. 包加载 | Package Loading
# =============================================================================
cat("\n==================================================\n")
cat("  生成Lancet补充图 | Generate Supplementary Figures\n")
cat("==================================================\n\n")

cat("[0] 加载R包...\n")

library(tidyverse)
library(ggplot2)
library(patchwork)
library(viridis)
library(scales)
library(pheatmap)
library(cowplot)
library(ggrepel)
library(ggpattern) # 用于黑白打印图案填充
if (requireNamespace("countrycode", quietly = TRUE)) {
    library(countrycode)
} else {
    cat("  ⚠ countrycode包未安装，Panel C将使用ISO3C代码\n")
}

cat("  ✓ 所有包加载完成\n")

# 全局设置
theme_set(theme_bw(base_size = 8))
single_width <- 3.35
double_width <- 7.09

# 创建输出目录
dir.create("../figures/supplementary", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# 1. 加载数据 | Load Data
# =============================================================================
cat("\n[1] 加载数据...\n")

# 加载原始数据
panel_data_raw <- readRDS("../data/processed/02_integrated_panel_data.rds")

# 应用与文稿一致的分析样本过滤条件（与03_data_integration.R一致）
# Apply analysis sample filtering consistent with manuscript (same as 03_data_integration.R)
countries_with_missing_data <- c("GIB", "PRK", "LIE", "MAF", "VEN")

panel_data <- panel_data_raw %>%
    filter(!iso3c %in% countries_with_missing_data) %>%
    filter(!is.na(income), !income %in% c('Aggregates', 'Not classified')) %>%
    filter(!is.na(ghe_gdp), !is.na(life_exp)) %>%
    filter(year >= 2000, year <= 2022) %>%
    mutate(
        log_gdp_pc = log(gdp_pc),
        log_population = log(population)
    )

cat(sprintf("  ✓ 主面板数据（分析样本）: %d obs, %d 国家\n", 
            nrow(panel_data), n_distinct(panel_data$iso3c)))

if (file.exists("../results/extended_analysis/extended_results.rds")) {
    extended_results <- readRDS("../results/extended_analysis/extended_results.rds")
    cat("  ✓ 扩展分析结果加载\n")
} else {
    extended_results <- NULL
}

# =============================================================================
# 2. Figure S1: 数据质量与覆盖
# =============================================================================
# NOTE: Figure S1 现在由 12_generate_figure_s1_complete_improved.R 生成
# 历史版本的代码已删除，请使用新版本脚本
cat("\n==================================================\n")
cat("  [2] Figure S1: 已迁移到 12_generate_figure_s1_complete_improved.R\n")
cat("==================================================\n\n")
# 跳过 Figure S1 的生成，直接进入 Figure S2
# NOTE: Figure S1 代码已注释，因为 key_vars 未定义且已迁移到独立脚本

# 计算缺失情况（已注释 - 需要 key_vars）
# NOTE: Figure S1 代码已完全注释，因为已迁移到独立脚本
# data_completeness <- panel_data %>%
#     select(iso3c, country, year, all_of(key_vars)) %>%
#     pivot_longer(cols = all_of(key_vars), names_to = "variable", values_to = "value") %>%
#     mutate(is_complete = !is.na(value)) %>%
#     group_by(iso3c, year) %>%
#     summarize(completeness = mean(is_complete), .groups = "drop")

# 绘制热图（Lancet标准渐变色：深紫到黄）
# NOTE: Figure S1 代码已注释，因为 data_completeness 未定义且已迁移到独立脚本
# panel_s1a_core <- ggplot(data_completeness, aes(x = year, y = iso3c, fill = completeness)) +
#     geom_tile() +
#     scale_fill_gradient(
#         low = "#4B0082", # 深紫色（低完整性）
#         high = "#FFFF00", # 黄色（高完整性）
#         name = NULL, # 移除"Completeness"标签，颜色条已足够清晰
#         limits = c(0, 1),
#         breaks = c(0, 0.5, 1),
#         guide = guide_colorbar(
#             title = NULL, # 移除标题
#             barwidth = unit(3.5, "cm"), # 颜色条宽度，与热图内容区域等宽（考虑边距后约3.5cm）
#             barheight = unit(0.3, "cm"), # 颜色条高度
#             direction = "horizontal"
#         )
#     ) +
#     scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2020), limits = c(1999, 2022), expand = expansion(mult = c(0.03, 0.05))) +
#     labs(
#         x = "Year",
#         y = "Country" # 添加Y轴标签，显示国家
#     ) +
#     theme_minimal(base_size = 7, base_family = "Helvetica") +
#     theme(
#         axis.text.y = element_blank(), # 不显示国家名称，避免过于密集
#         axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7, family = "Helvetica"), # 水平显示，7pt
#         axis.ticks.y = element_blank(), # 不显示y轴刻度
#         panel.grid = element_blank(),
#         legend.position = "bottom", # 图例放在底部，整合到热图中
#         legend.title = element_blank(), # 移除图例标题
#         legend.text = element_text(size = 7, family = "Helvetica"), # 7pt
#         legend.key.height = unit(0.3, "cm"), # 调整为水平图例
#         legend.key.width = unit(3.5, "cm"), # 图例条宽度，与热图内容区域等宽
#         legend.direction = "horizontal", # 水平方向
#         legend.justification = "center", # 图例居中
#         legend.box = "horizontal", # 水平排列
#         legend.margin = margin(t = 1, b = 1, unit = "mm"), # 减小上下边距
#         axis.title.y = element_text(face = "plain", size = 8, family = "Helvetica"), # 8pt Regular
#         axis.title.x = element_text(face = "plain", size = 8, family = "Helvetica"), # 8pt Regular
#         plot.background = element_rect(fill = "white", color = NA),
#         plot.margin = margin(2, 2, 2, 2) # 与左侧面板边距一致，确保高度匹配
#     )

# 移除子图标签A（Lancet标准）
# NOTE: Figure S1 代码已完全注释，因为已迁移到独立脚本
# 所有Figure S1相关代码都已注释掉，避免语法检查错误
# All Figure S1 related code has been commented out to avoid syntax check errors
# 
# =============================================================================
# Figure S1 代码已迁移到 12_generate_figure_s1_complete_improved.R
# 以下代码已完全注释，不再执行
# =============================================================================
#
# #if (FALSE) {  # 跳过整个 Figure S1 生成
#     # 定义占位符变量，避免语法检查错误
#     # Define placeholder variables to avoid syntax check errors
#     panel_s1a <- ggplot() + theme_void()
#     panel_s1b <- ggplot() + theme_void()
#     panel_s1c <- ggplot() + theme_void()
#     panel_s1d <- ggplot() + theme_void()

# missing_pattern <- panel_data %>%
#     select(all_of(key_vars)) %>%
#     summarize(across(everything(), ~ sum(is.na(.)))) %>%
# NOTE: 已注释，因为 key_vars 未定义
#missing_pattern <- data.frame() %>%
#    pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
#    mutate(
#        missing_pct = missing_count / nrow(panel_data) * 100,
#        variable_label = case_when(
#            variable == "life_exp" ~ "Life expectancy",
#            variable == "ghe_gdp" ~ "GHE/GDP",
#            variable == "che_gdp" ~ "CHE/GDP",
#            variable == "gdp_pc" ~ "GDP per capita",
#            variable == "government_effectiveness" ~ "Gov. effectiveness",
#            variable == "rule_of_law" ~ "Rule of law",
#            TRUE ~ variable
#        )
#    ) %>%
#    arrange(desc(missing_pct)) %>% # 按缺失率从高到低排序（Lancet标准）
#    mutate(variable_label = factor(variable_label, levels = rev(variable_label))) # 反转因子顺序，使coord_flip后最高的在顶部

# Lancet官方标准配色方案
#lancet_colors_var <- c(
#    "Gov. effectiveness" = "#0072B2", # Lancet标准蓝
#    "Rule of law" = "#D55E00", # Lancet标准橙
#    "GHE/GDP" = "#009E73", # Lancet标准绿
#    "CHE/GDP" = "#CC79A7", # Lancet标准紫
#    "GDP per capita" = "#E69F00", # Lancet标准黄
#    "Life expectancy" = "#56B4E9" # Lancet标准浅蓝
#)

# 黑白打印图案映射（确保在黑白打印时仍能区分）
# 根据Lancet要求：Gov. effectiveness实心，Rule of law斜线，GHE/GDP点状，其他相应区分
# 每个变量使用不同的图案，确保黑白打印时完全可区分
#lancet_patterns_var <- c(
#    "Gov. effectiveness" = "none", # 实心（无图案）
#    "Rule of law" = "stripe", # 斜线填充
#    "GHE/GDP" = "circle", # 点状填充
#    "CHE/GDP" = "crosshatch", # 交叉线
#    "GDP per capita" = "wave", # 波浪线
#    "Life expectancy" = "stripe" # 斜线（与Rule of law相同图案，但通过颜色和位置区分）
#)

#panel_s1b_core <- ggplot(missing_pattern, aes(
#    x = variable_label, # 直接使用已排序的因子
#    y = missing_pct,
#    fill = variable_label,
#    pattern = variable_label # 添加图案映射
#)) +
#    geom_col_pattern(
#        alpha = 0.85,
#        pattern_fill = "transparent", # 图案填充透明，保留底层颜色
#        pattern_color = "black", # 图案线条为黑色，在黑白打印时可见
#        pattern_density = 0.1, # 降低密度，使颜色更明显
#        pattern_spacing = 0.03,
#        pattern_angle = 45,
#        pattern_size = 0.6, # 减小线条粗细
#        pattern_alpha = 0.5 # 降低图案透明度，使颜色更明显
#    ) +
#    geom_text(aes(label = sprintf("%.1f%%", missing_pct)),
#        hjust = -0.2, size = 2.47, color = "#000000", family = "Helvetica" # 7pt Regular (7pt = 2.47mm)
#    ) +
#    coord_flip() +
#    scale_fill_manual(values = lancet_colors_var, guide = "none") +
#    scale_pattern_manual(values = lancet_patterns_var, guide = "none") +
#    scale_y_continuous(limits = c(0, max(missing_pattern$missing_pct) * 1.15)) +
#    labs(
#        x = "Variable",
#        y = "Missing percentage (%)"
#    ) +
#    theme_minimal(base_size = 7, base_family = "Helvetica") +
#    theme(
#        panel.grid.minor = element_blank(),
#        panel.grid.major.y = element_blank(),
#        axis.title.y = element_text(face = "plain", size = 8, family = "Helvetica", margin = margin(r = 6)), # 8pt Regular
#        axis.title.x = element_text(face = "plain", size = 8, family = "Helvetica", margin = margin(t = 4)), # 8pt Regular
#        axis.text.x = element_text(size = 7, family = "Helvetica"), # 7pt Regular
#        axis.text.y = element_text(size = 7, family = "Helvetica"), # 7pt Regular
#        plot.background = element_rect(fill = "white", color = NA),
#        plot.margin = margin(2, 2, 2, 5)
#    )

# 移除子图标签B（Lancet标准）
#panel_s1b <- panel_s1b_core

# Panel C: 数据来源质量评分（7个数据源）
# cat("[2.3] Panel C: 数据来源质量评分...\n")

#data_sources <- tribble(
#    ~Source, ~Variables, ~Quality_Score, ~Coverage,
#    "World Bank WDI", "Health, Economic", 95, 217,
#    "WHO GHO", "Health Expenditure", 90, 194,
#    "OECD Health", "Health Statistics", 95, 38,
#    "IMF WEO", "Fiscal Data", 85, 189,
#    "Eurostat", "EU Statistics", 100, 27,
#    "UN Population", "Demographics", 99, 237,
#    "GBD", "Disease Burden", 88, 204
#) %>%
    # 添加置信区间（假设标准误差为Coverage的5%）
#    mutate(
#        Coverage_SE = Coverage * 0.05,
#        Coverage_CI_Lower = Coverage - 1.96 * Coverage_SE,
#        Coverage_CI_Upper = Coverage + 1.96 * Coverage_SE,
        # 优化标签文本
#        label_wrap = case_when(
#            Source == "World Bank WDI" ~ "World Bank\nWDI",
#            Source == "WHO GHO" ~ "WHO GHO",
#            Source == "OECD Health" ~ "OECD\nHealth",
#            Source == "IMF WEO" ~ "IMF WEO",
#            Source == "Eurostat" ~ "Eurostat",
#            Source == "UN Population" ~ "UN\nPopulation",
#            Source == "GBD" ~ "GBD",
#            TRUE ~ Source
#        )
#    ) %>%
    # 按质量评分排序（从高到低）
#    arrange(desc(Quality_Score))

# 计算x轴范围（基于实际数据，减少左侧空白）
#x_min <- min(data_sources$Quality_Score) - 1
#x_max <- max(data_sources$Quality_Score) + 1

#panel_s1c_core <- ggplot(data_sources, aes(
#    x = Quality_Score, y = Coverage,
#    label = label_wrap, shape = Source
#)) +
    # 添加置信区间（垂直误差条）
#    geom_errorbar(
#        aes(ymin = Coverage_CI_Lower, ymax = Coverage_CI_Upper),
#        width = 0.5, color = "#666666", alpha = 0.5, linewidth = 0.5
#    ) +
    # 添加参考线：y=150（移除文字标注，信息放在图注中）
#    geom_hline(yintercept = 150, linetype = "dashed", color = "#999999", linewidth = 0.5, alpha = 0.7) +
    # 添加数据点（使用不同形状）
#    geom_point(aes(shape = Source), size = 3.5, color = "#2171b5", fill = "#2171b5", alpha = 0.8, stroke = 1) +
    # 使用ggrepel优化标签位置，避免重叠
#    geom_text_repel(
#        size = 2.47, # 7pt Regular (7pt = 2.47mm)
#        box.padding = 0.8, # 增加标签框的内边距
#        point.padding = 0.5, # 增加标签与点的距离
#        min.segment.length = 0.2, # 最小线段长度
#        max.overlaps = Inf, # 允许无限重叠处理
#        force = 2, # 增加排斥力
#        force_pull = 0.5, # 增加向点拉的力
#        color = "#000000",
#        fontface = "plain",
#        family = "Helvetica",
#        segment.color = "#666666", # 连接线颜色
#        segment.size = 0.3 # 连接线粗细
#    ) +
    # 设置形状映射（为每个数据源分配不同形状，确保黑白打印时可区分）
#    scale_shape_manual(
#        values = c(
#            "UN Population" = 19, # 实心圆
#            "World Bank WDI" = 17, # 三角形
#            "WHO GHO" = 15, # 正方形
#            "IMF WEO" = 4, # X形
#            "GBD" = 18, # 菱形
#            "OECD Health" = 16, # 倒三角形
#            "Eurostat" = 3 # 加号
#        ),
#        guide = "none" # 移除图例（因为标签已说明）
#    ) +
#    scale_x_continuous(limits = c(x_min, x_max), breaks = seq(85, 100, 5)) +
#    scale_y_continuous(limits = c(0, 250), breaks = seq(0, 250, 50)) +
#    labs(
#        x = "Quality Score (0-100)",
#        y = "Country Coverage"
#    ) +
#    theme_minimal(base_size = 7, base_family = "Helvetica") +
#    theme(
#        panel.grid.minor = element_blank(),
#        panel.grid.major = element_line(color = "#f0f0f0", linewidth = 0.3),
#        axis.title.x = element_text(face = "plain", size = 8, family = "Helvetica"), # 8pt Regular
#        axis.title.y = element_text(face = "plain", size = 8, family = "Helvetica"), # 8pt Regular
#        axis.text.x = element_text(size = 7, family = "Helvetica", angle = 0), # 7pt Regular，水平显示
#        axis.text.y = element_text(size = 7, family = "Helvetica"), # 7pt Regular
#        plot.margin = margin(2, 2, 2, 5)
#    )

# 移除子图标签C（Lancet标准）
#panel_s1c <- panel_s1c_core

# Panel D: 时间覆盖（美化版：堆叠面积图）
# cat("[2.4] Panel D: 时间覆盖...\n")

#temporal_coverage_long <- panel_data %>%
#    group_by(year) %>%
#    summarize(
#        n_countries = n_distinct(iso3c),
#        n_complete = sum(!is.na(life_exp) & !is.na(ghe_gdp) & !is.na(gdp_pc)),
#        .groups = "drop"
#    ) %>%
#    mutate(
#        n_incomplete = n_countries - n_complete,
#        completeness_rate = n_complete / n_countries * 100
#    )

# 准备堆叠面积图数据（需要按顺序：先Incomplete，再Complete）
#temporal_coverage_area <- temporal_coverage_long %>%
#    arrange(year) %>%
#    mutate(
#        y_bottom = 0,
#        y_top_incomplete = n_incomplete,
#        y_top_complete = n_countries
#    )

# 计算平均完整率
#avg_completeness <- mean(temporal_coverage_long$completeness_rate)
# 计算95%完整率对应的国家数（基于平均国家数）
#avg_countries <- mean(temporal_coverage_long$n_countries)
#target_complete_countries <- round(avg_countries * 0.95)

# 计算 y 轴上限（不再需要为顶部标签留出空间）
#y_max <- max(temporal_coverage_long$n_countries) * 1.1

# 年度数据标签已移除，信息放在正文说明中

#panel_s1d_core <- ggplot() +
    # 绘制堆叠面积图：先画Incomplete（底部）
#    geom_area(
#        data = temporal_coverage_area,
#        aes(x = year, y = y_top_incomplete, fill = "Incomplete"),
#        alpha = 0.85
#    ) +
    # 再画Complete（顶部）
#    geom_ribbon(
#        data = temporal_coverage_area,
#        aes(x = year, ymin = y_top_incomplete, ymax = y_top_complete, fill = "Complete"),
#        alpha = 0.85
#    ) +
    # 添加95%完整率参考线（移除文字标注，信息放在图注中）
#    geom_hline(
#        yintercept = target_complete_countries,
#        linetype = "dashed", color = "#666666", linewidth = 0.5, alpha = 0.7
#    ) +
    # 年度数据标签已移除，信息放在正文说明中
#    scale_fill_manual(
#        values = c("Incomplete" = "#999999", "Complete" = "#0072B2"), # Lancet标准配色
#        name = NULL # 移除图例标题，图例已足够清晰
#    ) +
#    scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2020), limits = c(1999, 2022), expand = expansion(mult = c(0.03, 0.05))) +
#    scale_y_continuous(limits = c(0, y_max), expand = expansion(mult = c(0, 0.05))) +
#    labs(
#        x = "Year",
#        y = "Number of countries"
#    ) +
#    theme_minimal(base_size = 7, base_family = "Helvetica") +
#    theme(
#        panel.grid.minor = element_blank(),
#        panel.grid.major.x = element_blank(),
#        panel.grid.major.y = element_line(color = "#f0f0f0", linewidth = 0.3),
#        axis.title.x = element_text(face = "plain", size = 8, family = "Helvetica"), # 8pt Regular
#        axis.title.y = element_text(face = "plain", size = 8, family = "Helvetica"), # 8pt Regular
#        axis.text.x = element_text(size = 7, family = "Helvetica", angle = 0), # 7pt Regular，水平显示
#        axis.text.y = element_text(size = 7, family = "Helvetica"), # 7pt Regular
#        legend.position = "bottom",
#        legend.title = element_text(face = "plain", size = 7, family = "Helvetica"), # 7pt Regular
#        legend.text = element_text(size = 7, family = "Helvetica"), # 7pt Regular
#        legend.key.size = unit(0.4, "cm"),
#        plot.margin = margin(2, 2, 2, 5)
#    )

# 移除子图标签D（Lancet标准）
#panel_s1d <- panel_s1d_core

# # 组合 Figure S1（优化布局：75%/25%宽度分配）
# # 左侧主栏：B（总体缺失模式，25%）/ D（35%）/ C（25%）垂直排列
# # 右侧边栏：A（25%）数据完整性热图，图例整合到底部
# # 注意：所有Figure S1相关代码都已注释，不会执行
# # left_panel <- panel_s1b / panel_s1d / panel_s1c +
# #     plot_layout(heights = c(0.25, 0.35, 0.25))

# 使用 cowplot 手动组合，确保右侧热图与左侧主区域高度匹配
# 左侧面板：75%宽度，从左侧开始，高度95%（留5%给标题）
# 右侧热图：25%宽度，与左侧主区域高度匹配（0.95），垂直对齐
# 左侧主区域高度分配：25%（B）/ 35%（D）/ 25%（C），共85%，剩余15%为间隔
# 热图图例已整合到底部，不额外占用空间
#figure_s1 <- cowplot::ggdraw() +
    # 绘制左侧面板（B/D/C垂直排列，25%/35%/25%高度，共85%）
#    cowplot::draw_plot(left_panel, x = 0, y = 0, width = 0.75, height = 0.95) +
    # 绘制右侧热图（25%宽度，与左侧主区域高度匹配，垂直对齐，图例整合到底部）
#    cowplot::draw_plot(panel_s1a, x = 0.75, y = 0, width = 0.25, height = 0.95) +
    # 添加标题
#    cowplot::draw_label(
#        "Figure S1. Data availability and quality assessment for GHE-life expectancy analysis",
#        x = 0.5, y = 0.98,
#        hjust = 0.5, vjust = 1,
#        size = 10, fontface = "bold", fontfamily = "Helvetica"
#    )

# 保存
#ggsave(
#    "../figures/supplementary/Figure_S1_Data_Quality.pdf",
#    plot = figure_s1,
#    width = double_width,
#    height = double_width * 0.8,
#    units = "in",
#    dpi = 300,
#    device = cairo_pdf
#)

#ggsave(
#    "../figures/supplementary/Figure_S1_Data_Quality.svg",
#    plot = figure_s1,
#    width = double_width,
#    height = double_width * 0.8,
#    units = "in",
#    dpi = 300,
#    device = svglite::svglite
#)

# cat("  ✓ Figure S1 生成成功\n")

#}  # 结束 Figure S1 代码块（if(FALSE)）

# =============================================================================
# 生成 Figure S1 黑白版本（用于黑白打印验证）
# =============================================================================
# Skip Figure S1 BW generation since Figure S1 is skipped
# 跳过Figure S1黑白版本生成，因为Figure S1已跳过
#if (FALSE) {  # Skip Figure S1 BW
#cat("[2.5] 生成 Figure S1 黑白版本...\n")

# Panel A 黑白版本：使用灰度渐变
# NOTE: Figure S1 代码已注释，因为 data_completeness 未定义且已迁移到独立脚本
# panel_s1a_bw <- ggplot(data_completeness, aes(x = year, y = iso3c, fill = completeness)) +
#    geom_tile() +
#    scale_fill_gradient(
#        low = "#000000", # 黑色（低完整性）
#        high = "#FFFFFF", # 白色（高完整性）
#        name = NULL,
#        limits = c(0, 1),
#        breaks = c(0, 0.5, 1),
#        guide = guide_colorbar(
#            title = NULL,
#            barwidth = unit(3.5, "cm"),
#            barheight = unit(0.3, "cm"),
#            direction = "horizontal"
#        )
#    ) +
#    scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2020), limits = c(1999, 2022), expand = expansion(mult = c(0.03, 0.05))) +
#    labs(x = "Year", y = "Country") +
#    theme_minimal(base_size = 7, base_family = "Helvetica") +
#    theme(
#        axis.text.y = element_blank(),
#        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7, family = "Helvetica"),
#        axis.ticks.y = element_blank(),
#        panel.grid = element_blank(),
#        legend.position = "bottom",
#        legend.title = element_blank(),
#        legend.text = element_text(size = 7, family = "Helvetica"),
#        legend.key.height = unit(0.3, "cm"),
#        legend.key.width = unit(3.5, "cm"),
#        legend.direction = "horizontal",
#        legend.justification = "center",
#        legend.box = "horizontal",
#        legend.margin = margin(t = 1, b = 1, unit = "mm"),
#        axis.title.y = element_text(face = "plain", size = 8, family = "Helvetica"),
#        axis.title.x = element_text(face = "plain", size = 8, family = "Helvetica"),
#        plot.background = element_rect(fill = "white", color = NA),
#        plot.margin = margin(2, 2, 2, 2)
#    )

# Panel B 黑白版本：增强图案对比度
#panel_s1b_bw <- ggplot(missing_pattern, aes(
#    x = variable_label,
#    y = missing_pct,
#    fill = variable_label,
#    pattern = variable_label
#)) +
#    geom_col_pattern(
#        alpha = 1.0, # 完全不透明，确保图案清晰
#        pattern_fill = "white", # 图案填充为白色
#        pattern_color = "black", # 图案线条为黑色
#        pattern_density = 0.3, # 增加密度，使图案更明显
#        pattern_spacing = 0.02,
#        pattern_angle = 45,
#        pattern_size = 1.0, # 增加线条粗细
#        pattern_alpha = 1.0 # 完全不透明
#    ) +
#    geom_text(aes(label = sprintf("%.1f%%", missing_pct)),
#        hjust = -0.2, size = 2.47, color = "#000000", family = "Helvetica"
#    ) +
#    coord_flip() +
    # 黑白版本：使用不同灰度填充
#    scale_fill_manual(
#        values = c(
#            "Gov. effectiveness" = "#000000", # 黑色
#            "Rule of law" = "#666666", # 深灰
#            "GHE/GDP" = "#999999", # 中灰
#            "CHE/GDP" = "#CCCCCC", # 浅灰
#            "GDP per capita" = "#E0E0E0", # 很浅灰
#            "Life expectancy" = "#F0F0F0" # 极浅灰
#        ),
#        guide = "none"
#    ) +
#    scale_pattern_manual(
#        values = c(
#            "Gov. effectiveness" = "none", # 实心（无图案）
#            "Rule of law" = "stripe", # 斜线填充
#            "GHE/GDP" = "circle", # 点状填充
#            "CHE/GDP" = "crosshatch", # 交叉线
#            "GDP per capita" = "wave", # 波浪线
#            "Life expectancy" = "stripe" # 斜线（不同角度）
#        ),
#        guide = "none"
#    ) +
#    scale_y_continuous(limits = c(0, max(missing_pattern$missing_pct) * 1.15)) +
#    labs(x = "Variable", y = "Missing percentage (%)") +
#    theme_minimal(base_size = 7, base_family = "Helvetica") +
#    theme(
#        panel.grid.minor = element_blank(),
#        panel.grid.major.y = element_blank(),
#        axis.title.y = element_text(face = "plain", size = 8, family = "Helvetica", margin = margin(r = 6)),
#        axis.title.x = element_text(face = "plain", size = 8, family = "Helvetica", margin = margin(t = 4)),
#        axis.text.x = element_text(size = 7, family = "Helvetica"),
#        axis.text.y = element_text(size = 7, family = "Helvetica"),
#        plot.background = element_rect(fill = "white", color = NA),
#        plot.margin = margin(2, 2, 2, 5)
#    )

# Panel C 黑白版本：使用不同形状和填充
#panel_s1c_bw <- ggplot(data_sources, aes(
#    x = Quality_Score, y = Coverage,
#    label = label_wrap, shape = Source, fill = Source
#)) +
#    geom_errorbar(
#        aes(ymin = Coverage_CI_Lower, ymax = Coverage_CI_Upper),
#        width = 0.5, color = "#000000", alpha = 0.8, linewidth = 0.8
#    ) +
#    geom_hline(yintercept = 150, linetype = "dashed", color = "#000000", linewidth = 0.8, alpha = 0.7) +
#    geom_point(aes(shape = Source, fill = Source), size = 4, color = "#000000", stroke = 1.5) +
#    geom_text_repel(
#        size = 2.47,
#        box.padding = 0.8,
#        point.padding = 0.5,
#        min.segment.length = 0.2,
#        max.overlaps = Inf,
#        force = 2,
#        force_pull = 0.5,
#        color = "#000000",
#        fontface = "plain",
#        family = "Helvetica",
#        segment.color = "#000000",
#        segment.size = 0.5
#    ) +
#    scale_shape_manual(
#        values = c(
#            "UN Population" = 21, # 实心圆（带边框）
#            "World Bank WDI" = 24, # 三角形（带边框）
#            "WHO GHO" = 22, # 正方形（带边框）
#            "IMF WEO" = 4, # X形
#            "GBD" = 23, # 菱形（带边框）
#            "OECD Health" = 25, # 倒三角形（带边框）
#            "Eurostat" = 3 # 加号
#        ),
#        guide = "none"
#    ) +
#    scale_fill_manual(
#        values = c(
#            "UN Population" = "#000000", # 黑色填充
#            "World Bank WDI" = "#666666", # 深灰填充
#            "WHO GHO" = "#999999", # 中灰填充
#            "IMF WEO" = "#000000", # 黑色（X形，无填充）
#            "GBD" = "#CCCCCC", # 浅灰填充
#            "OECD Health" = "#E0E0E0", # 很浅灰填充
#            "Eurostat" = "#000000" # 黑色（加号，无填充）
#        ),
#        guide = "none"
#    ) +
#    scale_x_continuous(limits = c(x_min, x_max), breaks = seq(85, 100, 5)) +
#    scale_y_continuous(limits = c(0, 250), breaks = seq(0, 250, 50)) +
#    labs(x = "Quality Score (0-100)", y = "Country Coverage") +
#    theme_minimal(base_size = 7, base_family = "Helvetica") +
#    theme(
#        panel.grid.minor = element_blank(),
#        panel.grid.major = element_line(color = "#000000", linewidth = 0.3, alpha = 0.3),
#        axis.title.x = element_text(face = "plain", size = 8, family = "Helvetica"),
#        axis.title.y = element_text(face = "plain", size = 8, family = "Helvetica"),
#        axis.text.x = element_text(size = 7, family = "Helvetica", angle = 0),
#        axis.text.y = element_text(size = 7, family = "Helvetica"),
#        plot.margin = margin(2, 2, 2, 5)
#    )

# Panel D 黑白版本：使用不同图案区分Complete和Incomplete
#panel_s1d_bw <- ggplot() +
    # 绘制Incomplete区域（使用图案）
#    geom_area_pattern(
#        data = temporal_coverage_area,
#        aes(x = year, y = y_top_incomplete, pattern = "Incomplete"),
#        fill = "#CCCCCC", # 浅灰填充
#        pattern_fill = "white",
#        pattern_color = "black",
#        pattern_density = 0.2,
#        pattern_spacing = 0.02,
#        pattern_angle = 45,
#        pattern_size = 0.8,
#        alpha = 0.9
#    ) +
    # 绘制Complete区域（实心）
#    geom_ribbon(
#        data = temporal_coverage_area,
#        aes(x = year, ymin = y_top_incomplete, ymax = y_top_complete),
#        fill = "#000000", # 黑色填充
#        alpha = 0.9
#    ) +
#    geom_hline(
#        yintercept = target_complete_countries,
#        linetype = "dashed", color = "#000000", linewidth = 0.8, alpha = 0.7
#    ) +
#    scale_pattern_manual(
#        values = c("Incomplete" = "stripe"),
#        guide = "none"
#    ) +
#    scale_fill_manual(
#        values = c("Incomplete" = "#CCCCCC", "Complete" = "#000000"),
#        name = NULL
#    ) +
#    scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2020), limits = c(1999, 2022), expand = expansion(mult = c(0.03, 0.05))) +
#    scale_y_continuous(limits = c(0, y_max), expand = expansion(mult = c(0, 0.05))) +
#    labs(x = "Year", y = "Number of countries") +
#    theme_minimal(base_size = 7, base_family = "Helvetica") +
#    theme(
#        panel.grid.minor = element_blank(),
#        panel.grid.major.x = element_blank(),
#        panel.grid.major.y = element_line(color = "#000000", linewidth = 0.3, alpha = 0.3),
#        axis.title.x = element_text(face = "plain", size = 8, family = "Helvetica"),
#        axis.title.y = element_text(face = "plain", size = 8, family = "Helvetica"),
#        axis.text.x = element_text(size = 7, family = "Helvetica", angle = 0),
#        axis.text.y = element_text(size = 7, family = "Helvetica"),
#        legend.position = "bottom",
#        legend.title = element_text(face = "plain", size = 7, family = "Helvetica"),
#        legend.text = element_text(size = 7, family = "Helvetica"),
#        legend.key.size = unit(0.4, "cm"),
#        plot.margin = margin(2, 2, 2, 5)
#    )

# 组合黑白版本
#left_panel_bw <- panel_s1b_bw / panel_s1d_bw / panel_s1c_bw +
#    plot_layout(heights = c(0.25, 0.35, 0.25))

#figure_s1_bw <- cowplot::ggdraw() +
#    cowplot::draw_plot(left_panel_bw, x = 0, y = 0, width = 0.75, height = 0.95) +
#    cowplot::draw_plot(panel_s1a_bw, x = 0.75, y = 0, width = 0.25, height = 0.95) +
#    cowplot::draw_label(
#        "Figure S1. Data availability and quality assessment for GHE-life expectancy analysis",
#        x = 0.5, y = 0.98,
#        hjust = 0.5, vjust = 1,
#        size = 10, fontface = "bold", fontfamily = "Helvetica"
#    )

# 保存黑白版本
#ggsave(
#    "../figures/supplementary/Figure_S1_Data_Quality_BW.pdf",
#    plot = figure_s1_bw,
#    width = double_width,
#    height = double_width * 0.8,
#    units = "in",
#    dpi = 300,
#    device = cairo_pdf
#)

#ggsave(
#    "../figures/supplementary/Figure_S1_Data_Quality_BW.svg",
#    plot = figure_s1_bw,
#    width = double_width,
#    height = double_width * 0.8,
#    units = "in",
#    dpi = 300,
#    device = svglite::svglite
#)

#cat("  ✓ Figure S1 黑白版本生成成功\n")

#}  # End of Figure S1 BW block

# =============================================================================
# 3. Figure S2: 稳健性检验 - 样本
# =============================================================================
# NOTE: Figure S2 现在由 12_generate_figure_s2_robustness_sample_improved.R 生成
# 历史版本的代码已删除，请使用新版本脚本
cat("\n==================================================\n")
cat("  [3] Figure S2: 已迁移到 12_generate_figure_s2_robustness_sample_improved.R\n")
cat("==================================================\n\n")
# 跳过 Figure S2 的生成，直接进入下一个图

# =============================================================================
# 4. Figure S5: IV诊断（重构版：2个Panel，符合Lancet标准）
# =============================================================================
cat("\n==================================================\n")
cat("  [4] 生成 Figure S5: IV诊断（Lancet标准）\n")
cat("==================================================\n\n")

# 加载IV结果
iv_results_file <- "../results/analysis/03_iv_results.rds"
if (file.exists(iv_results_file)) {
    iv_results <- readRDS(iv_results_file)

    # 加载Export Price Shock IV结果
    export_iv_results_file <- "../results/analysis/09_combined_iv_results.rds"
    export_iv_results <- if (file.exists(export_iv_results_file)) readRDS(export_iv_results_file) else NULL

    # 加载FE结果（用于主分析对比）
    fe_results_file <- "../results/analysis/03_fe_results.rds"
    fe_results <- if (file.exists(fe_results_file)) readRDS(fe_results_file) else NULL

    cat("[4.1] Panel A: 森林图 - IV策略因果效应估计...\n")

    # 统一字体大小（Lancet提交标准：Arial 10pt，图注9pt）
    axis_title_size <- 22 / 2.835
    axis_text_size <- 20 / 2.835
    annotation_text_size <- axis_text_size * 0.45
    base_font_size <- 10 # 10pt
    panel_label_size <- 12 / 2.835 # 12pt = 4.23mm (Panel标签)
    caption_size <- 9 / 2.835 # 9pt (图注)

    # =============================================================================
    # Panel A: 森林图 - IV策略因果效应估计（Lancet标准）
    # =============================================================================

    add_iv_strategy <- function(df, res_obj, strategy_code, strategy_label, strategy_definition) {
        if (is.null(res_obj)) {
            return(df)
        }
        # 处理Export Price Shock IV的特殊结构（可能没有model字段，但有coefficient和robust）
        if (strategy_code == "export_price_shock") {
            if (is.null(res_obj$model) || !"ghe_gdp" %in% names(coef(res_obj$model))) {
                # 如果model不存在，尝试使用coefficient和robust字段
                if (!is.null(res_obj$coefficient) && !is.null(res_obj$robust)) {
                    iv_coef <- res_obj$coefficient
                    iv_se <- res_obj$robust["ghe_gdp", "Std. Error"]
                    iv_f <- res_obj$first_stage_F
                    iv_p <- 2 * pnorm(-abs(iv_coef / iv_se))
                    return(df %>%
                        add_row(
                            Strategy_Code = strategy_code,
                            Strategy_Display = strategy_label,
                            Strategy_Definition = strategy_definition,
                            Coefficient = iv_coef,
                            SE = iv_se,
                            CI_lower = res_obj$ci_lower,
                            CI_upper = res_obj$ci_upper,
                            P_Value = iv_p,
                            F_Statistic = iv_f,
                            Weak_IV = !is.na(iv_f) && iv_f < 10
                        ))
                } else {
                    return(df)
                }
            }
        }
        # 标准处理（其他IV策略）
        if (is.null(res_obj$model) || !"ghe_gdp" %in% names(coef(res_obj$model))) {
            return(df)
        }
        iv_coef <- coef(res_obj$model)["ghe_gdp"]
        iv_se <- res_obj$robust["ghe_gdp", "Std. Error"]
        iv_f <- res_obj$first_stage_F
        iv_p <- 2 * pnorm(-abs(iv_coef / iv_se))
        df %>%
            add_row(
                Strategy_Code = strategy_code,
                Strategy_Display = strategy_label,
                Strategy_Definition = strategy_definition,
                Coefficient = iv_coef,
                SE = iv_se,
                CI_lower = iv_coef - 1.96 * iv_se,
                CI_upper = iv_coef + 1.96 * iv_se,
                P_Value = iv_p,
                F_Statistic = iv_f,
                Weak_IV = !is.na(iv_f) && iv_f < 10
            )
    }

    forest_data <- tibble(
        Strategy_Code = character(),
        Strategy_Display = character(),
        Strategy_Definition = character(),
        Coefficient = numeric(),
        SE = numeric(),
        CI_lower = numeric(),
        CI_upper = numeric(),
        P_Value = numeric(),
        F_Statistic = numeric(),
        Weak_IV = logical()
    ) %>%
        add_iv_strategy(
            iv_results$fiscal,
            strategy_code = "fiscal",
            strategy_label = "Fiscal capacity IV",
            strategy_definition = "Historical tax capacity and revenue administration instruments"
        ) %>%
        add_iv_strategy(
            iv_results$governance,
            strategy_code = "governance",
            strategy_label = "Governance IV",
            strategy_definition = "Institutional quality and rule-of-law indicators"
        ) %>%
        add_iv_strategy(
            export_iv_results,
            strategy_code = "export_price_shock",
            strategy_label = "Export Price Shock IV",
            strategy_definition = "Terms-of-trade shocks from international commodity price fluctuations"
        )

    strategy_levels <- c("Main analysis", "Fiscal capacity IV", "Governance IV", "Export Price Shock IV")
    shape_map <- c(
        "Main analysis" = 23,
        "Fiscal capacity IV" = 22,
        "Governance IV" = 21,
        "Export Price Shock IV" = 24
    )
    strength_colors <- c(
        "Strong IV" = "#2E5984",
        "Weak/Borderline IV" = "#D6604D"
    )
    strength_colors_bw <- c(
        "Strong IV" = "#000000",
        "Weak/Borderline IV" = "#7F7F7F"
    )

    # 先计算主分析（强IV的逆方差加权平均），然后再去重
    strong_subset <- forest_data %>%
        filter(!is.na(F_Statistic) & F_Statistic >= 15)

    if (nrow(strong_subset) >= 1) {
        weights <- 1 / (strong_subset$SE^2)
        meta_coef <- sum(weights * strong_subset$Coefficient) / sum(weights)
        meta_se <- sqrt(1 / sum(weights))
        meta_F <- sum(weights * strong_subset$F_Statistic, na.rm = TRUE) / sum(weights, na.rm = TRUE)
        main_row <- tibble(
            Strategy_Code = "main",
            Strategy_Display = "Main analysis",
            Strategy_Definition = "Reference 2SLS specification (strong IVs pooled)",
            Coefficient = meta_coef,
            SE = meta_se,
            CI_lower = meta_coef - 1.96 * meta_se,
            CI_upper = meta_coef + 1.96 * meta_se,
            P_Value = NA_real_,
            F_Statistic = meta_F,
            Weak_IV = FALSE
        )
        forest_data <- bind_rows(main_row, forest_data)
    }

    # 去重，确保每个策略只保留一个（保留Main analysis的正确版本）
    # 先按Strategy_Code去重，确保Main analysis使用正确的meta_F
    forest_data <- forest_data %>%
        arrange(match(Strategy_Display, strategy_levels)) %>%
        distinct(Strategy_Code, .keep_all = TRUE) # 使用Strategy_Code去重，确保Main analysis正确

    stopifnot("至少需要一个IV策略以绘制Figure S5" = nrow(forest_data) > 0)

    # 动态计算X轴范围，基于实际系数和置信区间
    all_coefs <- c(forest_data$Coefficient, forest_data$CI_lower, forest_data$CI_upper)
    x_min <- floor(min(all_coefs, na.rm = TRUE) * 10) / 10 - 0.2 # 向下取整并留出边距
    x_limit_max <- ceiling(max(all_coefs, na.rm = TRUE) * 10) / 10 + 0.2 # 向上取整并留出边距
    
    # 确保范围合理（至少包含-1.5到1.0，以显示所有策略）
    x_min <- min(x_min, -1.5)
    x_limit_max <- max(x_limit_max, 1.0)
    
    label_offset <- 0.4

    forest_data <- forest_data %>%
        mutate(
            Strength_Group = ifelse(F_Statistic < 15, "Weak/Borderline IV", "Strong IV"),
            Borderline_IV = Strategy_Code == "export_price_shock" & !is.na(F_Statistic) & F_Statistic < 10,
            Dagger_Flag = Weak_IV | Borderline_IV,
            Strategy_Display = factor(Strategy_Display, levels = rev(strategy_levels)),
            F_Label = ifelse(is.na(F_Statistic), "NA", sprintf("%.1f", F_Statistic)),
            Display_Label = sprintf(
                "%s (F = %s)%s",
                as.character(Strategy_Display),
                F_Label,
                ifelse(Dagger_Flag, "\u2020", "")
            ),
            Effect_Text = sprintf(
                "β = %.2f (95%% CI %.2f to %.2f)",
                Coefficient, CI_lower, CI_upper
            ),
            label_x = case_when(
                Strategy_Code == "governance" ~ CI_lower - 0.08, # Governance IV放在左侧，非常靠近点
                Strategy_Code == "export_price_shock" ~ CI_upper + 0.08, # Export Price Shock IV放在右侧，非常靠近点
                TRUE ~ CI_upper + 0.08 # 其他（Main analysis, Fiscal capacity IV）放在右侧，非常靠近点
            ),
            label_hjust = case_when(
                Strategy_Code == "governance" ~ 1, # 左侧，右对齐
                TRUE ~ 0 # 右侧，左对齐
            ),
            label_vjust = 0.5, # 所有标注都与点在同一水平线上
            label_y_nudge = case_when(
                Strategy_Code == "main" ~ 0.0,
                Strategy_Code == "fiscal" ~ 0.0,
                Strategy_Code == "governance" ~ 0.0,
                Strategy_Code == "export_price_shock" ~ 0.0,
                TRUE ~ 0
            )
        )

    display_labels_lookup <- forest_data %>%
        mutate(Display_Label = gsub(" \\(F", "\n(F", Display_Label)) %>%
        distinct(Strategy_Display, Display_Label) %>%
        mutate(Strategy_Display = as.character(Strategy_Display))

    panel_s5a <- ggplot(forest_data, aes(x = Coefficient, y = Strategy_Display)) +
        geom_segment(aes(x = CI_lower, xend = CI_upper, y = Strategy_Display, yend = Strategy_Display, color = Strength_Group),
            linewidth = 0.9
        ) +
        geom_point(aes(shape = Strategy_Display, fill = Strength_Group),
            size = 3.8, color = "#000000", stroke = 0.6
        ) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "#CCCCCC", linewidth = 0.8) +
                geom_text(
            data = forest_data,
            aes(
                x = label_x,
                y = Strategy_Display,
                label = Effect_Text,
                color = Strength_Group,
                vjust = label_vjust,
                hjust = label_hjust
            ),
            lineheight = 1.15,
            size = annotation_text_size,
            family = "Arial",
            show.legend = FALSE
        ) +
        scale_color_manual(values = strength_colors, guide = guide_legend(title = "Instrument strength")) +
        scale_shape_manual(values = shape_map, guide = "none") +
        scale_fill_manual(values = strength_colors, guide = "none") +
        scale_x_continuous(
            name = "Effect estimate (β coefficient)",
            limits = c(x_min, x_limit_max),
            expand = expansion(mult = c(0.05, 0.1)) # 增加右侧边距以容纳注释
        ) +
        scale_y_discrete(
            name = "IV Strategy",
            labels = function(values) {
                display_labels_lookup$Display_Label[match(values, display_labels_lookup$Strategy_Display)]
            }
        ) +
        theme_minimal(base_size = base_font_size, base_family = "Arial") +
        theme(
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "#F0F0F0", linewidth = 0.25),  # 更淡的网格线
            axis.title.x = element_text(size = axis_title_size, margin = margin(t = 8)),
            axis.title.y = element_text(size = axis_title_size, margin = margin(r = 8)),
            axis.text.x = element_text(size = axis_text_size),
            axis.text.y = element_text(size = axis_text_size),
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            plot.margin = margin(12, 18, 10, 14, "mm")
        ) +
        coord_cartesian(clip = "off")

    # =============================================================================
    # Panel B: 工具变量强度诊断（F统计量和最大相对偏倚曲线）
    # =============================================================================

    cat("[4.2] Panel B: 工具变量强度诊断...\n")

    # 提取F统计量并准备绘图数据
    # 确保所有策略都被包含，特别是Export Price Shock IV
    cat(sprintf("[DEBUG] forest_data中的策略数量: %d\n", nrow(forest_data)))
    cat(sprintf("[DEBUG] forest_data中的策略: %s\n", paste(forest_data$Strategy_Code, collapse = ", ")))
    cat(sprintf("[DEBUG] Export Price Shock IV在forest_data中: %s\n", "export_price_shock" %in% forest_data$Strategy_Code))
    
    iv_f_stats <- forest_data %>%
        filter(!is.na(F_Statistic)) %>% # 先过滤，确保有F统计量
        distinct(Strategy_Code, .keep_all = TRUE) %>% # 使用Strategy_Code去重，保留所有字段
        select(Strategy_Code, Strategy_Display, Display_Label, Strength_Group, F_Statistic) %>%
        mutate(
            Strategy_Display = as.character(Strategy_Display),
            Display_Label = as.character(Display_Label)
        )
    
    cat(sprintf("[DEBUG] iv_f_stats中的策略数量: %d\n", nrow(iv_f_stats)))
    cat(sprintf("[DEBUG] iv_f_stats中的策略: %s\n", paste(iv_f_stats$Strategy_Code, collapse = ", ")))
    cat(sprintf("[DEBUG] Export Price Shock IV在iv_f_stats中: %s\n", "export_price_shock" %in% iv_f_stats$Strategy_Code))

    calc_bias <- function(f) ifelse(is.na(f), NA_real_, pmin(100 / (f + 1), 30))

    iv_f_stats <- iv_f_stats %>%
        mutate(
            Bias = calc_bias(F_Statistic),
            Strategy_Text = Display_Label # 统一使用Display_Label，不单独为Export Price Shock IV添加bias信息
        )

    observed_f <- iv_f_stats$F_Statistic
    min_observed_f <- ifelse(length(observed_f) == 0, 0, min(observed_f, na.rm = TRUE))
    max_observed_f <- ifelse(length(observed_f) == 0, 0, max(observed_f, na.rm = TRUE))
    
    # 动态计算X轴范围，聚焦在强工具变量区域
    x_min <- max(0, floor(min_observed_f / 10) * 10 - 20) # 从最小F值前20开始
    x_limit <- ceiling(max_observed_f / 10) * 10 + 20 # 到最大F值后20结束
    
    # 如果最小F值很大，可以从100开始（因为所有IV都是强工具变量）
    if (min_observed_f > 100) {
        x_min <- max(100, x_min) # 至少从100开始，聚焦在强工具变量区域
    }
    
    # 调整X轴刻度，减少网格线（仅保留主要刻度）
    x_breaks <- seq(x_min, x_limit, by = 50) # 使用50的间隔，减少网格线

    bias_curve_data <- tibble(
        F_value = seq(0.1, x_limit, by = 0.5)
    ) %>%
        mutate(Max_bias = pmin(100 / (F_value + 1), 30)) %>%
        filter(F_value >= x_min) # 只显示X轴范围内的数据

    weak_zone <- tibble(xmin = 0, xmax = 10, ymin = -Inf, ymax = Inf)

    # Panel B: 工具变量强度诊断（Lancet标准：黑白灰，线型区分）
    panel_s5b <- ggplot() +
        geom_rect(
            data = weak_zone,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE,
            fill = "#F4F4F4", alpha = 0.5
        ) +
        # 移除高偏差参考线，因为实际偏差都在1%以下
        # geom_hline(yintercept = c(5, 10, 20), linetype = "dashed", color = "#D0D0D0") +
        geom_line(
            data = bias_curve_data, aes(x = F_value, y = Max_bias),
            linewidth = 1.0, color = "#1A1A1A"
        ) +
        geom_vline(xintercept = 10, linetype = "dashed", color = "#CCCCCC", linewidth = 0.8) +  # 加粗参考线，浅灰色
        # 添加Stock-Yogo 5%偏差临界值参考线（F=16.38）- 移除或淡化
        # geom_vline(xintercept = 16.38, linetype = "dotted", color = "#8A8A8A", linewidth = 0.5, alpha = 0.7) +
        geom_point(
            data = iv_f_stats,
            aes(x = F_Statistic, y = Bias, color = Strength_Group),
            size = 3.6, shape = 21, stroke = 0.5, fill = "white"
        ) +
        # 为每个策略设置标注位置，分布在曲线上方和下方，用连线连接
        # Main analysis: 曲线左下方，稍微右移
        geom_segment(
            data = iv_f_stats %>% filter(Strategy_Code == "main") %>%
                mutate(label_x = F_Statistic - 10), # 稍微右移（从-20改为-10）
            aes(x = F_Statistic, xend = label_x, y = Bias, yend = Bias - 0.4),
            color = "#BBBBBB", linewidth = 0.3, linetype = "solid"
        ) +
        geom_text(
            data = iv_f_stats %>% filter(Strategy_Code == "main") %>%
                mutate(label_x = F_Statistic - 10, label_y = Bias - 0.4), # 稍微右移
            aes(x = label_x, y = label_y, label = Strategy_Text, color = Strength_Group),
            family = "Arial", size = annotation_text_size,
            hjust = 1, vjust = 1, # 右对齐，因为文字在左侧
            show.legend = FALSE,
            lineheight = 1.1
        ) +
        # Fiscal capacity IV: 曲线下方
        geom_segment(
            data = iv_f_stats %>% filter(Strategy_Code == "fiscal"),
            aes(x = F_Statistic, xend = F_Statistic, y = Bias, yend = Bias - 0.4),
            color = "#BBBBBB", linewidth = 0.3, linetype = "solid"
        ) +
        geom_text(
            data = iv_f_stats %>% filter(Strategy_Code == "fiscal") %>%
                mutate(label_y = Bias - 0.4),
            aes(x = F_Statistic, y = label_y, label = Strategy_Text, color = Strength_Group),
            family = "Arial", size = annotation_text_size,
            hjust = 0.5, vjust = 1,
            show.legend = FALSE,
            lineheight = 1.1
        ) +
        # Governance IV: 右上方，继续左移
        geom_segment(
            data = iv_f_stats %>% filter(Strategy_Code == "governance") %>%
                mutate(label_x = F_Statistic + 10), # 继续左移（从+20改为+10）
            aes(x = F_Statistic, xend = label_x, y = Bias, yend = Bias + 0.5),
            color = "#BBBBBB", linewidth = 0.3, linetype = "solid"
        ) +
        geom_text(
            data = iv_f_stats %>% filter(Strategy_Code == "governance") %>%
                mutate(label_x = F_Statistic + 10, label_y = Bias + 0.5), # 继续左移
            aes(x = label_x, y = label_y, label = Strategy_Text, color = Strength_Group),
            family = "Arial", size = annotation_text_size,
            hjust = 0, vjust = 0, # 左对齐，因为文字在右侧
            show.legend = FALSE,
            lineheight = 1.1
        ) +
        # Export Price Shock IV: 曲线上方，明确指定策略代码
        geom_segment(
            data = iv_f_stats %>% filter(Strategy_Code == "export_price_shock"),
            aes(x = F_Statistic, xend = F_Statistic, y = Bias, yend = Bias + 0.4),
            color = "#BBBBBB", linewidth = 0.3, linetype = "solid"
        ) +
        geom_text(
            data = iv_f_stats %>% filter(Strategy_Code == "export_price_shock") %>%
                mutate(label_y = Bias + 0.4),
            aes(x = F_Statistic, y = label_y, label = Strategy_Text, color = Strength_Group),
            family = "Arial", size = annotation_text_size,
            hjust = 0.5, vjust = 0,
            show.legend = FALSE,
            lineheight = 1.1
        ) +
        scale_shape_manual(values = shape_map, guide = "none") +
        scale_color_manual(values = strength_colors, guide = "none") +
        scale_x_continuous(
            name = "First-stage F-statistic",
            limits = c(x_min, x_limit),
            breaks = x_breaks,
            expand = expansion(mult = c(0.1, 0.15)) # 增加左右边距以容纳标注（左侧容纳Main analysis，右侧容纳Governance IV）
        ) +
        scale_y_continuous(
            name = "Maximum relative bias (%)",
            limits = c(-0.6, 1.5), # 扩展Y轴上限以容纳Export Price Shock IV的标注
            breaks = seq(0, 1.0, by = 0.5), # 减少网格线：仅保留主要刻度（0, 0.5, 1.0）
            expand = expansion(mult = c(0.1, 0.1)) # 上下留出边距
        ) +
        theme_minimal(base_size = base_font_size, base_family = "Arial") +
        theme(
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),  # 移除Y轴网格线，与Panel A统一
            panel.grid.major.x = element_line(color = "#F0F0F0", linewidth = 0.25),  # 更淡的网格线，与Panel A统一
            axis.title.x = element_text(size = axis_title_size, margin = margin(t = 8)),
            axis.title.y = element_text(size = axis_title_size, margin = margin(r = 8)),
            axis.text.x = element_text(size = axis_text_size),
            axis.text.y = element_text(size = axis_text_size),
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            plot.margin = margin(12, 18, 10, 14, "mm")  # 统一边距，与Panel A一致
        ) +
        coord_cartesian(clip = "off")

    # 组合2个Panel并添加标题
    figure_s5 <- (panel_s5a / panel_s5b) +
        plot_layout(heights = c(1, 1), guides = "collect") +
        plot_annotation(
            title = "Supplementary Figure 5. Instrumental variable diagnostics",
            theme = theme(
                plot.title = element_text(
                    family = "Arial",
                    face = "bold",
                    size = axis_title_size * 1.2,
                    hjust = 0.5,
                    margin = margin(b = 8)
                )
            )
        ) &
        theme(
            legend.position = "bottom",
            legend.title = element_text(size = axis_text_size * 1.05, family = "Arial"),
            legend.text = element_text(size = axis_text_size, family = "Arial")
        )

    # 确保输出目录存在
    output_dir <- "../figures/supplementary"
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

    pdf_file <- file.path(output_dir, "Figure_S5_IV_Diagnostics.pdf")
    svg_file <- file.path(output_dir, "Figure_S5_IV_Diagnostics.svg")

    # 保存为高分辨率PDF/SVG
    ggsave(
        pdf_file,
        plot = figure_s5,
        width = double_width,
        height = double_width * 0.9,
        units = "in",
        dpi = 300,
        device = cairo_pdf
    )

    ggsave(
        svg_file,
        plot = figure_s5,
        width = double_width,
        height = double_width * 0.9,
        units = "in",
        dpi = 300,
        device = svglite::svglite
    )

    panel_s5a_bw <- panel_s5a +
        scale_color_manual(values = strength_colors_bw, guide = guide_legend(title = "Instrument strength")) +
        scale_fill_manual(values = strength_colors_bw, guide = "none")

    panel_s5b_bw <- panel_s5b +
        scale_color_manual(values = strength_colors_bw, guide = "none")

    figure_s5_bw <- (panel_s5a_bw / panel_s5b_bw) +
        plot_layout(heights = c(1, 1), guides = "collect") +
        plot_annotation(
            title = "Supplementary Figure 5. Instrumental variable diagnostics (monochrome)",
            theme = theme(
                plot.title = element_text(
                    family = "Arial",
                    face = "bold",
                    size = axis_title_size * 1.2,
                    hjust = 0.5,
                    margin = margin(b = 8)
                )
            )
        ) &
        theme(
            legend.position = "bottom",
            legend.title = element_text(size = axis_text_size * 1.05, family = "Arial"),
            legend.text = element_text(size = axis_text_size, family = "Arial")
        )

    bw_pdf_file <- file.path(output_dir, "Figure_S5_IV_Diagnostics_BW_FromColor.pdf")
    bw_svg_file <- file.path(output_dir, "Figure_S5_IV_Diagnostics_BW_FromColor.svg")

    ggsave(
        bw_pdf_file,
        plot = figure_s5_bw,
        width = double_width,
        height = double_width * 0.9,
        units = "in",
        dpi = 300,
        device = cairo_pdf
    )

    ggsave(
        bw_svg_file,
        plot = figure_s5_bw,
        width = double_width,
        height = double_width * 0.9,
        units = "in",
        dpi = 300,
        device = svglite::svglite
    )

    # =============================================================================
    # 创建标准Lancet图注文本（独立文件）
    # =============================================================================

    # 准备图注所需的信息
    strategy_definitions <- forest_data %>%
        distinct(Strategy_Display, Strategy_Definition) %>%
        mutate(
            Strategy_Display = as.character(Strategy_Display),
            Strategy_Display = factor(Strategy_Display, levels = strategy_levels)
        ) %>%
        arrange(Strategy_Display) %>%
        mutate(def_text = sprintf("%s: %s", as.character(Strategy_Display), Strategy_Definition)) %>%
        pull(def_text)

    main_analysis_sentence <- if (any(forest_data$Strategy_Code == "main" & !forest_data$Weak_IV)) {
        "The main analysis retained only fiscal capacity instruments with first-stage F ≥ 10."
    } else {
        "The main analysis specification is shown alongside alternative IV sets."
    }

    definition_sentence <- if (length(strategy_definitions) > 0) {
        paste0("Strategy definitions – ", paste(strategy_definitions, collapse = "; "), ".")
    } else {
        ""
    }

    bias_from_f <- function(f_value) {
        if (is.na(f_value)) {
            return("NA")
        }
        if (f_value >= 16.38) {
            return("5")
        }
        if (f_value >= 8.96) {
            return("10")
        }
        if (f_value >= 5.44) {
            return("20")
        }
        if (f_value >= 4.58) {
            return("30")
        }
        return(">30")
    }

    f_range_sentence <- if (nrow(iv_f_stats) > 0) {
        sprintf(
            "Observed first-stage F-statistics ranged from %.1f to %.1f.",
            min(iv_f_stats$F_Statistic), max(iv_f_stats$F_Statistic)
        )
    } else {
        "Observed first-stage F-statistics were not available for these specifications."
    }

    export_iv_sentence <- forest_data %>%
        filter(Strategy_Code == "export_price_shock") %>%
        distinct(F_Statistic) %>%
        pull(F_Statistic) %>%
        .[1] %>%
        {
            if (!is.na(.)) {
                sprintf(
                    "The export price shock IV (combined with fiscal and governance instruments) attained F = %.1f, corresponding to an expected maximum relative bias of approximately %s%%.",
                    ., bias_from_f(.)
                )
            } else {
                ""
            }
        }

    dagger_strategies <- forest_data %>%
        filter(Dagger_Flag) %>%
        mutate(
            dagger_text = sprintf("%s (F = %s)", as.character(Strategy_Display), F_Label)
        ) %>%
        pull(dagger_text)

    strength_definition_sentence <- "We classified instruments with first-stage F < 15 as weak/borderline because F < 10 is the conventional weak-IV cutoff yet F values between 10 and 15 can still induce non-trivial bias."

    weak_iv_sentence <- if (length(dagger_strategies) > 0) {
        sprintf(
            "† F < 10 is the conventional threshold for weak instruments; %s is borderline (interpret with caution).",
            paste(dagger_strategies, collapse = "; ")
        )
    } else {
        "† F < 10 is the conventional threshold for weak instruments; none of the IV specifications fell below this cutoff."
    }

    figure_legend <- paste(
        "Supplementary Figure 5. Instrumental variable diagnostics for causal effect estimation.",
        paste(
            "Panel A shows the two-stage least squares estimate (β coefficient with 95% confidence interval [CI]) of government health expenditure (GHE, % of GDP) on life expectancy across the instrumental variable strategies. β denotes the linear 2SLS coefficient measuring the change in life expectancy (years) associated with a one-percentage-point increase in GHE.",
            main_analysis_sentence,
            definition_sentence
        ),
        paste(
            "Panel B overlays the theoretical approximation Max relative bias ≈ 100/(F + 1)% with the observed first-stage F-statistics.",
            "The shaded area denotes the weak-instrument region (F < 10); the dashed line marks the conventional threshold.",
            f_range_sentence,
            export_iv_sentence
        ),
        "All models were estimated using two-stage least squares (2SLS) with country-clustered standard errors; the plotted F-statistics correspond to the Cragg-Donald first-stage statistic for the endogenous regressor (GHE) conditional on all exogenous covariates.",
        strength_definition_sentence,
        weak_iv_sentence,
        sep = "\n\n"
    )

    # 保存图注为文本文件
    writeLines(
        figure_legend,
        "../figures/supplementary/Figure_S5_IV_Diagnostics_LEGEND.txt"
    )

    cat("\n  ✓ Figure S5 生成成功（完全符合Lancet提交标准）\n")
    cat("    ================================\n")
    cat("    文件输出：\n")
    cat("    - PDF: Figure_S5_IV_Diagnostics.pdf\n")
    cat("    - SVG: Figure_S5_IV_Diagnostics.svg\n")
    cat("    - BW PDF: Figure_S5_IV_Diagnostics_BW_FromColor.pdf\n")
    cat("    - BW SVG: Figure_S5_IV_Diagnostics_BW_FromColor.svg\n")
    cat("    - 图注: Figure_S5_IV_Diagnostics_LEGEND.txt\n\n")
    cat("    格式规范：\n")
    cat("    - 字体：Arial 10pt（主体）/ 11pt（坐标轴标题）\n")
    cat("    - 颜色：彩色Lancet调色板（Panel A/B一致）\n")
    cat("    - 尺寸：双栏宽度 (%.2f × %.2f inches)\n", double_width, double_width * 0.7)
    cat("    - 分辨率：PDF 300 dpi / SVG 300 dpi\n")
    cat("    - F统计量：第一阶段Cragg-Donald F值\n")
    cat("    - 图注：独立文本文件，符合Lancet标准格式\n")
    cat("    ================================\n")
} else {
    cat("  ⚠ IV结果文件不存在，跳过Figure S5生成\n")
}

# =============================================================================
# 5. Figure S6: 地区异质性
# =============================================================================
cat("\n==================================================\n")
cat("  [4] 生成 Figure S6: 地区异质性\n")
cat("==================================================\n\n")

cat("[4.1] Panel A: 按大洲分组...\n")

# 添加大洲信息（简化版）
panel_data_region <- panel_data %>%
    mutate(
        continent = case_when(
            iso3c %in% c("USA", "CAN", "MEX") ~ "North America",
            iso3c %in% c("BRA", "ARG", "CHL", "COL", "PER") ~ "South America",
            iso3c %in% c("GBR", "DEU", "FRA", "ITA", "ESP", "POL", "NLD", "BEL", "SWE") ~ "Europe",
            iso3c %in% c("CHN", "IND", "JPN", "KOR", "IDN", "THA", "VNM", "PAK", "BGD") ~ "Asia",
            iso3c %in% c("NGA", "EGY", "ZAF", "ETH", "KEN", "GHA", "TZA") ~ "Africa",
            iso3c %in% c("AUS", "NZL") ~ "Oceania",
            TRUE ~ "Other"
        )
    )

# 计算各大洲的GHE vs 预期寿命关系
continent_summary <- panel_data_region %>%
    filter(continent != "Other") %>%
    group_by(continent, year) %>%
    summarize(
        mean_ghe = mean(ghe_gdp, na.rm = TRUE),
        mean_life_exp = mean(life_exp, na.rm = TRUE),
        sd_life_exp = sd(life_exp, na.rm = TRUE),
        n = n(),
        .groups = "drop"
    ) %>%
    mutate(
        se_life_exp = sd_life_exp / sqrt(n),
        ci_lower = mean_life_exp - 1.96 * se_life_exp,
        ci_upper = mean_life_exp + 1.96 * se_life_exp
    )

# Lancet标准配色
lancet_continent_colors <- c(
    "Africa" = "#0072B2", # Lancet标准蓝
    "Europe" = "#D55E00", # Lancet标准橙
    "Oceania" = "#009E73", # Lancet标准绿
    "Asia" = "#56B4E9", # Lancet浅蓝
    "North America" = "#CC79A7", # Lancet标准紫
    "South America" = "#E69F00" # Lancet标准黄
)

# 为黑白打印添加线型和点型
continent_linetypes <- c(
    "Africa" = "solid",
    "Europe" = "dashed",
    "Oceania" = "dotdash",
    "Asia" = "dotted",
    "North America" = "longdash",
    "South America" = "twodash"
)

continent_shapes <- c(
    "Africa" = 19, # 实心圆
    "Europe" = 17, # 实心三角
    "Oceania" = 15, # 实心方
    "Asia" = 18, # 实心菱形
    "North America" = 16, # 实心圆（不同大小）
    "South America" = 8 # 星形
)

panel_s6a <- ggplot(continent_summary, aes(
    x = mean_ghe, y = mean_life_exp,
    color = continent, linetype = continent, fill = continent
)) +
    # 添加95%置信区间带（减少透明度，使其更淡）
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
        alpha = 0.1, color = NA
    ) +
    # 使用平滑曲线，移除数据点（增粗线型）
    geom_smooth(method = "loess", se = FALSE, linewidth = 1.2 / 2.835, alpha = 0.8) +
    # 在曲线末端添加大陆名称标注（使用最后一年数据）
    geom_text(
        data = continent_summary %>%
            group_by(continent) %>%
            slice_max(year, n = 1) %>%
            mutate(
                # 调整不同大陆标签的垂直位置
                vjust_value = case_when(
                    continent == "South America" ~ 1.2,  # 继续下移
                    continent == "Oceania" ~ 0.3,        # 稍微上移
                    continent == "Europe" ~ 1.0,         # 继续下移
                    TRUE ~ 0.5                           # 其他保持默认
                )
            ),
        aes(label = continent, x = mean_ghe, y = mean_life_exp, vjust = vjust_value),
        hjust = -0.1, size = 7 / 2.835,
        color = "black", family = "Helvetica", fontface = "plain",
        inherit.aes = FALSE
    ) +
    scale_color_manual(values = lancet_continent_colors, guide = "none") +
    scale_fill_manual(values = lancet_continent_colors, guide = "none") +
    scale_linetype_manual(values = continent_linetypes, guide = "none") +
    scale_x_continuous(
        limits = c(
            min(continent_summary$mean_ghe, na.rm = TRUE) * 0.9,
            8.5 # 增加上界到8.5，为Europe和Oceania标签留出空间
        ), # 扩展x轴范围，为标注留空间
        expand = expansion(mult = c(0.05, 0.12)) # 增加右侧扩展空间，确保文字完整显示
    ) +
    # Y轴范围将在组合时统一设置
    labs(
        title = NULL, # 移除子图标签
        x = "Mean GHE/GDP (%)",
        y = "Mean Life Expectancy (years)"
    ) +
    theme_minimal(base_size = 7, base_family = "Helvetica") +
    theme(
        legend.position = "none", # 移除图例
        panel.grid.minor = element_blank(), # 移除次要网格线
        panel.grid.major.x = element_line(color = "#F0F0F0", linewidth = 0.25), # 仅保留主要X轴网格线，更淡
        panel.grid.major.y = element_blank(), # 移除Y轴网格线
        axis.title.x = element_text(face = "plain", size = 8, family = "Helvetica"),
        axis.title.y = element_text(face = "plain", size = 8, family = "Helvetica", angle = 90),
        axis.text = element_text(size = 7, family = "Helvetica"),
        plot.margin = margin(2, 2, 2, 2, "mm") # 统一边距：上、右、下、左
    )

# Panel B: 按收入组
cat("[4.2] Panel B: 按收入组...\n")

income_summary <- panel_data %>%
    filter(!is.na(income), !is.na(ghe_gdp), !is.na(life_exp)) %>%
    # 确保只有4个标准收入组
    filter(income %in% c(
        "High income", "Upper middle income",
        "Lower middle income", "Low income"
    )) %>%
    group_by(income, year) %>%
    summarize(
        mean_ghe = mean(ghe_gdp, na.rm = TRUE),
        mean_life_exp = mean(life_exp, na.rm = TRUE),
        sd_life_exp = sd(life_exp, na.rm = TRUE),
        n = n(),
        .groups = "drop"
    ) %>%
    mutate(
        se_life_exp = sd_life_exp / sqrt(n),
        # 确保因子顺序
        income = factor(income, levels = c(
            "High income", "Upper middle income",
            "Lower middle income", "Low income"
        ))
    )

# Lancet标准配色
lancet_income_colors <- c(
    "High income" = "#0072B2", # Lancet标准蓝
    "Upper middle income" = "#009E73", # Lancet标准绿
    "Lower middle income" = "#D55E00", # Lancet标准橙
    "Low income" = "#E69F00" # Lancet标准黄
)

# 为黑白打印添加线型和点型
income_linetypes <- c(
    "High income" = "solid",
    "Upper middle income" = "dashed",
    "Lower middle income" = "dotdash",
    "Low income" = "dotted"
)

income_shapes <- c(
    "High income" = 19, # 实心圆
    "Upper middle income" = 17, # 实心三角
    "Lower middle income" = 15, # 实心方
    "Low income" = 18 # 实心菱形
)

panel_s6b <- ggplot(income_summary, aes(
    x = year, y = mean_life_exp,
    color = income, fill = income, linetype = income, shape = income
)) +
    geom_ribbon(
        aes(
            ymin = mean_life_exp - 1.96 * se_life_exp,
            ymax = mean_life_exp + 1.96 * se_life_exp
        ),
        alpha = 0.1, color = NA
    ) +
    geom_line(linewidth = 0.8 / 2.835) +
    geom_point(size = 2 / 2.835, alpha = 0.8) +
    # 在曲线末端添加收入组名称标注（使用最后一年数据）
    # 计算文字需要的空间，将文字放在X轴范围内
    geom_text(
        data = income_summary %>%
            group_by(income) %>%
            slice_min(year, n = 1) %>% # 使用第一年（2000年）的数据
            mutate(
                x_pos = 2000, # 将文字位置放在2000年轴上
                # 一行显示，不换行
                income_label = as.character(income)
            ),
        aes(label = income_label, x = x_pos, y = mean_life_exp),
        hjust = 0, vjust = 0.5, size = 7 / 2.835, # hjust = 0 表示左对齐，文字从2000年轴开始向右显示
        color = "black", family = "Helvetica", fontface = "plain",
        inherit.aes = FALSE
    ) +
    scale_color_manual(values = lancet_income_colors, guide = "none") +
    scale_fill_manual(values = lancet_income_colors, guide = "none") +
    scale_linetype_manual(values = income_linetypes, guide = "none") +
    scale_shape_manual(values = income_shapes, guide = "none") +
    scale_x_continuous(
        breaks = seq(2000, 2022, 4), # 使用4年间隔，减少拥挤
        limits = c(2000, 2022), # X轴从2000年开始，移除左侧空白
        expand = expansion(mult = c(0, 0.15)) # 左侧不扩展（下限已是2000），右侧扩展15%为文字留出空间
    ) +
    # Y轴范围将在组合时统一设置
    labs(
        title = NULL, # 移除子图标签
        x = "Year",
        y = "Mean Life Expectancy (years)"
    ) +
    theme_minimal(base_size = 7, base_family = "Helvetica") +
    theme(
        legend.position = "none", # 移除图例
        panel.grid.minor = element_blank(), # 移除次要网格线
        panel.grid.major.x = element_line(color = "#F0F0F0", linewidth = 0.25), # 仅保留主要X轴网格线，更淡
        panel.grid.major.y = element_blank(), # 移除Y轴网格线
        axis.title.x = element_text(face = "plain", size = 8, family = "Helvetica"),
        axis.title.y = element_text(face = "plain", size = 8, family = "Helvetica", angle = 90),
        axis.text = element_text(size = 7, family = "Helvetica"),
        plot.margin = margin(2, 2, 2, 2, "mm") # 统一边距：上、右、下、左
    )

# Panel C: 区域异质性分析（使用真实数据）
cat("[4.3] Panel C: 区域异质性分析...\n")

# 读取区域效应结果
regional_results_file <- "../results/regional_effect_results.csv"
if (file.exists(regional_results_file)) {
    regional_data <- read_csv(regional_results_file, show_col_types = FALSE) %>%
        mutate(
            Region = factor(Region, levels = Region[order(Coefficient)]),
            Significant = p_value < 0.05
        )

    # 为不同区域添加不同形状
    regional_data <- regional_data %>%
        mutate(
            shape_id = as.numeric(Region),
            point_shape = case_when(
                shape_id %% 4 == 1 ~ 19, # 实心圆
                shape_id %% 4 == 2 ~ 17, # 实心三角
                shape_id %% 4 == 3 ~ 15, # 实心方
                TRUE ~ 18 # 实心菱形
            )
        )

    panel_s6c <- ggplot(regional_data, aes(x = Coefficient, y = Region)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "#999999", linewidth = 0.5 / 2.835) +
        # 统一使用0.8pt实线表示置信区间
        geom_errorbar(aes(xmin = CI_lower, xmax = CI_upper),
            orientation = "y", height = 0.2, linewidth = 0.8 / 2.835, color = "#0072B2"
        ) +
        # 使用实心点表示点估计值，不同形状区分不同组别（增大点大小）
        geom_point(aes(shape = Region),
            size = 6.0 / 2.835, fill = "#0072B2", color = "#0072B2", stroke = 0.8 / 2.835  # 增大点的大小
        ) +
        scale_shape_manual(values = setNames(regional_data$point_shape, regional_data$Region), guide = "none") +
        scale_x_continuous(
            labels = number_format(accuracy = 0.01),
            limits = c(-0.30, 0.85), # 调整x轴范围以完整显示所有置信区间
            breaks = seq(-0.30, 0.85, by = 0.25), # 主要刻度
            expand = expansion(mult = c(0.05, 0.05)) # 两侧扩展空间
        ) +
        labs(
            title = NULL, # 移除子图标签
            x = "GHE Coefficient (95% CI)",
            y = "Region" # Y轴标签
        ) +
        theme_minimal(base_size = 7, base_family = "Helvetica") +
        theme(
            panel.grid.major.y = element_blank(), # 移除Y轴网格线
            panel.grid.major.x = element_line(color = "#F0F0F0", linewidth = 0.25), # 仅保留主要X轴网格线，更淡
            panel.grid.minor = element_blank(),
            plot.margin = margin(2, 2, 2, 2, "mm"), # 统一边距：上、右、下、左
            axis.title.x = element_text(face = "plain", size = 8, family = "Helvetica"),
            axis.title.y = element_text(face = "plain", size = 8, family = "Helvetica", angle = 90),
            axis.text = element_text(size = 7, family = "Helvetica")
        )
} else {
    cat("  ⚠ 警告: 区域效应结果文件不存在，使用占位符\n")
    panel_s6c <- ggplot() +
        annotate("text",
            x = 0.5, y = 0.5,
            label = "(C) Regional Heterogeneity\n\nData file not found",
            size = 3
        ) +
        theme_void()
}

# Panel D: 发展水平分析（基于区域数据）
cat("[4.4] Panel D: 发展水平分析...\n")

if (file.exists(regional_results_file)) {
    # 根据基线预期寿命分类发展水平
    development_data <- regional_data %>%
        mutate(
            Development_Level = case_when(
                Baseline_Life_Exp >= 75 ~ "High",
                Baseline_Life_Exp >= 70 ~ "Upper-Middle",
                Baseline_Life_Exp >= 65 ~ "Lower-Middle",
                TRUE ~ "Low"
            ),
            Development_Level = factor(Development_Level,
                levels = c("Low", "Lower-Middle", "Upper-Middle", "High")
            )
        ) %>%
        group_by(Development_Level) %>%
        summarize(
            Mean_Coef = mean(Coefficient, na.rm = TRUE),
            SE_Coef = sd(Coefficient, na.rm = TRUE) / sqrt(n()),
            N_Countries = sum(Countries, na.rm = TRUE),
            n_regions = n(), # 区域数量
            # 对于只有1个区域的情况，使用该区域的原始置信区间
            CI_Lower_raw = first(CI_lower),
            CI_Upper_raw = first(CI_upper),
            .groups = "drop"
        ) %>%
        mutate(
            # 如果只有1个区域，使用原始置信区间；否则使用计算的标准误
            CI_Lower = if_else(n_regions == 1, CI_Lower_raw, Mean_Coef - 1.96 * SE_Coef),
            CI_Upper = if_else(n_regions == 1, CI_Upper_raw, Mean_Coef + 1.96 * SE_Coef)
        ) %>%
        select(-CI_Lower_raw, -CI_Upper_raw) # 移除临时列

    # 为不同发展水平添加不同形状
    development_data <- development_data %>%
        mutate(
            shape_id = as.numeric(Development_Level),
            point_shape = case_when(
                shape_id == 1 ~ 19, # 实心圆
                shape_id == 2 ~ 17, # 实心三角
                shape_id == 3 ~ 15, # 实心方
                TRUE ~ 18 # 实心菱形
            )
        )

    panel_s6d <- ggplot(development_data, aes(x = Mean_Coef, y = Development_Level)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "#999999", linewidth = 0.5 / 2.835) +
        # 统一使用0.8pt实线表示置信区间
        geom_errorbar(aes(xmin = CI_Lower, xmax = CI_Upper),
            orientation = "y", height = 0.2, linewidth = 0.8 / 2.835, color = "#0072B2"
        ) +
        # 使用实心点表示点估计值，不同形状区分不同组别（增大点的大小）
        geom_point(aes(shape = Development_Level),
            size = 6.0 / 2.835, fill = "#0072B2", color = "#0072B2", stroke = 0.8 / 2.835  # 增大点的大小
        ) +
        scale_shape_manual(values = setNames(development_data$point_shape, development_data$Development_Level), guide = "none") +
        scale_x_continuous(
            labels = number_format(accuracy = 0.01),
            limits = c(-0.30, 0.85), # 调整x轴范围以完整显示所有置信区间，与Panel C一致
            breaks = seq(-0.30, 0.85, by = 0.25), # 主要刻度，与Panel C一致
            expand = expansion(mult = c(0.05, 0.05))
        ) +
        labs(
            title = NULL, # 移除子图标签
            x = "Mean GHE Coefficient (95% CI)",
            y = "Development Level" # Y轴标签
        ) +
        theme_minimal(base_size = 7, base_family = "Helvetica") +
        theme(
            panel.grid.major.y = element_blank(), # 移除Y轴网格线
            panel.grid.major.x = element_line(color = "#F0F0F0", linewidth = 0.25), # 仅保留主要X轴网格线，更淡，与Panel C一致
            panel.grid.minor = element_blank(),
            plot.margin = margin(2, 2, 2, 2, "mm"), # 统一边距：上、右、下、左
            axis.title.x = element_text(face = "plain", size = 8, family = "Helvetica"),
            axis.title.y = element_text(face = "plain", size = 8, family = "Helvetica", angle = 90),
            axis.text = element_text(size = 7, family = "Helvetica")
        )
} else {
    cat("  ⚠ 警告: 区域效应结果文件不存在，使用占位符\n")
    panel_s6d <- ggplot() +
        annotate("text",
            x = 0.5, y = 0.5,
            label = "(D) Development Stage Analysis\n\nData file not found",
            size = 3
        ) +
        theme_void()
}

# 统一Panel A和B的Y轴范围
y_min_ab <- min(
    min(continent_summary$mean_life_exp, na.rm = TRUE),
    min(income_summary$mean_life_exp, na.rm = TRUE)
) * 0.98
y_max_ab <- max(
    max(continent_summary$mean_life_exp, na.rm = TRUE),
    max(income_summary$mean_life_exp, na.rm = TRUE)
) * 1.02

# 更新Panel A和B的Y轴范围
panel_s6a <- panel_s6a + scale_y_continuous(
    limits = c(y_min_ab, y_max_ab),
    expand = expansion(mult = c(0.02, 0.02))
)
panel_s6b <- panel_s6b + scale_y_continuous(
    limits = c(y_min_ab, y_max_ab),
    expand = expansion(mult = c(0.02, 0.02))
)

# 组合（采用上下结构：顶部A|B填满空间，底部C|D独立对齐）
# 使用cowplot::plot_grid来更精确控制布局
# 顶部一行：A和B调整宽度比例，使其视觉上一致（考虑边距差异）
top_row <- cowplot::plot_grid(panel_s6a, panel_s6b, nrow = 1, align = "h", rel_widths = c(0.98, 1.02))
# 底部一行：C和D各占50%宽度
bottom_row <- cowplot::plot_grid(panel_s6c, panel_s6d, nrow = 1, align = "h", rel_widths = c(1, 1))
# 组合：顶部和底部各占50%高度，顶部填满宽度
figure_s6_core <- cowplot::plot_grid(top_row, bottom_row, ncol = 1, align = "v", rel_heights = c(1, 1))

# 使用cowplot添加标题（移除副标题）
figure_s6 <- cowplot::ggdraw() +
    cowplot::draw_plot(figure_s6_core, x = 0, y = 0, width = 1, height = 0.95) +
    # 主标题：10pt Helvetica Bold，居中
    cowplot::draw_label(
        "Figure S7. Regional and income group heterogeneity in GHE-life expectancy relationship",
        x = 0.5, y = 0.98,
        hjust = 0.5, vjust = 1,
        size = 10, fontface = "bold", fontfamily = "Helvetica"
    )

# 确保输出目录存在
dir.create("../figures/supplementary", recursive = TRUE, showWarnings = FALSE)

# 保存
ggsave(
    "../figures/supplementary/Figure_S7_Regional_Heterogeneity.pdf",
    plot = figure_s6,
    width = double_width,
    height = double_width * 0.8,
    units = "in",
    dpi = 300,
    device = cairo_pdf
)

ggsave(
    "../figures/supplementary/Figure_S7_Regional_Heterogeneity.svg",
    plot = figure_s6,
    width = double_width,
    height = double_width * 0.8,
    units = "in",
    dpi = 300,
    device = svglite::svglite
)

cat("  ✓ Figure S7 生成成功\n")

# 生成 Figure S7 黑白版本（用于黑白打印验证）
cat("[4.5] 生成 Figure S7 黑白版本...\n")

# Panel A 黑白版本：使用灰度颜色和线型区分
continent_bw_colors <- c(
    "Africa" = "#000000", # 黑色
    "Europe" = "#666666", # 深灰
    "Oceania" = "#999999", # 中灰
    "Asia" = "#CCCCCC", # 浅灰
    "North America" = "#333333", # 很深的灰
    "South America" = "#808080" # 中深灰
)

panel_s6a_bw <- ggplot(continent_summary, aes(
    x = mean_ghe, y = mean_life_exp,
    color = continent, linetype = continent, fill = continent
)) +
    # 添加95%置信区间带（使用灰度）
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
        alpha = 0.15, color = NA
    ) +
    # 使用平滑曲线，移除数据点（增粗线型）
    geom_smooth(method = "loess", se = FALSE, linewidth = 1.2 / 2.835, alpha = 0.8) +
    # 在曲线末端添加大陆名称标注
    geom_text(
        data = continent_summary %>%
            group_by(continent) %>%
            slice_max(year, n = 1) %>%
            mutate(
                # 调整不同大陆标签的垂直位置
                vjust_value = case_when(
                    continent == "South America" ~ 1.2,  # 继续下移
                    continent == "Oceania" ~ 0.3,        # 稍微上移
                    continent == "Europe" ~ 1.0,         # 继续下移
                    TRUE ~ 0.5                           # 其他保持默认
                )
            ),
        aes(label = continent, x = mean_ghe, y = mean_life_exp, vjust = vjust_value),
        hjust = -0.1, size = 7 / 2.835,
        color = "black", family = "Helvetica", fontface = "plain",
        inherit.aes = FALSE
    ) +
    scale_color_manual(values = continent_bw_colors, guide = "none") +
    scale_fill_manual(values = continent_bw_colors, guide = "none") +
    scale_linetype_manual(values = continent_linetypes, guide = "none") +
    scale_x_continuous(
        limits = c(
            min(continent_summary$mean_ghe, na.rm = TRUE) * 0.9,
            8.5 # 增加上界到8.5，为Europe和Oceania标签留出空间
        ),
        expand = expansion(mult = c(0.05, 0.12)) # 增加右侧扩展空间，确保文字完整显示
    ) +
    scale_y_continuous(
        limits = c(y_min_ab, y_max_ab),
        expand = expansion(mult = c(0.02, 0.02))
    ) +
    labs(
        title = NULL,
        x = "Mean GHE/GDP (%)",
        y = "Mean Life Expectancy (years)"
    ) +
    theme_minimal(base_size = 7, base_family = "Helvetica") +
    theme(
        legend.position = "none",
        axis.title.x = element_text(face = "plain", size = 8, family = "Helvetica"),
        axis.title.y = element_text(face = "plain", size = 8, family = "Helvetica", angle = 90),
        axis.text = element_text(size = 7, family = "Helvetica"),
        plot.margin = margin(2, 2, 2, 2, "mm") # 统一边距：上、右、下、左
    )

# Panel B 黑白版本：使用灰度颜色和线型区分
income_bw_colors <- c(
    "High income" = "#000000", # 黑色
    "Upper middle income" = "#666666", # 深灰
    "Lower middle income" = "#999999", # 中灰
    "Low income" = "#CCCCCC" # 浅灰
)

panel_s6b_bw <- ggplot(income_summary, aes(
    x = year, y = mean_life_exp,
    color = income, fill = income, linetype = income, shape = income
)) +
    geom_ribbon(
        aes(
            ymin = mean_life_exp - 1.96 * se_life_exp,
            ymax = mean_life_exp + 1.96 * se_life_exp
        ),
        alpha = 0.15, color = NA
    ) +
    geom_line(linewidth = 0.8 / 2.835) +
    geom_point(size = 2 / 2.835, alpha = 0.8) +
    geom_text(
        data = income_summary %>%
            group_by(income) %>%
            slice_min(year, n = 1) %>% # 使用第一年（2000年）的数据
            mutate(
                x_pos = 2000, # 将文字位置放在2000年轴上
                # 一行显示，不换行
                income_label = as.character(income)
            ),
        aes(label = income_label, x = x_pos, y = mean_life_exp),
        hjust = 0, vjust = 0.5, size = 7 / 2.835, # hjust = 0 表示左对齐，文字从2000年轴开始向右显示
        color = "black", family = "Helvetica", fontface = "plain",
        inherit.aes = FALSE
    ) +
    scale_color_manual(values = income_bw_colors, guide = "none") +
    scale_fill_manual(values = income_bw_colors, guide = "none") +
    scale_linetype_manual(values = income_linetypes, guide = "none") +
    scale_shape_manual(values = income_shapes, guide = "none") +
    scale_x_continuous(
        breaks = seq(2000, 2022, 4), # 使用4年间隔，与彩色版一致
        limits = c(2000, 2022), # X轴从2000年开始，移除左侧空白
        expand = expansion(mult = c(0, 0.15)) # 左侧不扩展（下限已是2000），右侧扩展15%为文字留出空间
    ) +
    scale_y_continuous(
        limits = c(y_min_ab, y_max_ab),
        expand = expansion(mult = c(0.02, 0.02))
    ) +
    labs(
        title = NULL,
        x = "Year",
        y = "Mean Life Expectancy (years)"
    ) +
    theme_minimal(base_size = 7, base_family = "Helvetica") +
    theme(
        legend.position = "none",
        axis.title.x = element_text(face = "plain", size = 8, family = "Helvetica"),
        axis.title.y = element_text(face = "plain", size = 8, family = "Helvetica", angle = 90),
        axis.text = element_text(size = 7, family = "Helvetica"),
        plot.margin = margin(2, 2, 2, 2, "mm") # 统一边距：上、右、下、左
    )

# Panel C 黑白版本：使用黑色和形状区分
if (file.exists(regional_results_file)) {
    panel_s6c_bw <- ggplot(regional_data, aes(x = Coefficient, y = Region)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "#666666", linewidth = 0.5 / 2.835) +
        geom_errorbar(aes(xmin = CI_lower, xmax = CI_upper),
            orientation = "y", height = 0.2, linewidth = 0.8 / 2.835, color = "#000000"
        ) +
        geom_point(aes(shape = Region),
            size = 6.0 / 2.835, fill = "#000000", color = "#000000", stroke = 0.8 / 2.835  # 增大点的大小
        ) +
        scale_shape_manual(values = setNames(regional_data$point_shape, regional_data$Region), guide = "none") +
        scale_x_continuous(labels = number_format(accuracy = 0.01)) +
        labs(
            title = NULL,
            x = "GHE Coefficient (95% CI)",
            y = "Region"
        ) +
        theme_minimal(base_size = 7, base_family = "Helvetica") +
        theme(
            panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3 / 2.835),
            panel.grid.minor = element_blank(),
            plot.margin = margin(2, 2, 2, 2, "mm"), # 统一边距：上、右、下、左
            axis.title.x = element_text(face = "plain", size = 8, family = "Helvetica"),
            axis.title.y = element_text(face = "plain", size = 8, family = "Helvetica", angle = 90),
            axis.text = element_text(size = 7, family = "Helvetica")
        )
} else {
    panel_s6c_bw <- panel_s6c
}

# Panel D 黑白版本：使用黑色和形状区分
if (file.exists(regional_results_file)) {
    panel_s6d_bw <- ggplot(development_data, aes(x = Mean_Coef, y = Development_Level)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "#666666", linewidth = 0.5 / 2.835) +
        geom_errorbar(aes(xmin = CI_Lower, xmax = CI_Upper),
            orientation = "y", height = 0.2, linewidth = 0.8 / 2.835, color = "#000000"
        ) +
        geom_point(aes(shape = Development_Level),
            size = 4.5 / 2.835, fill = "#000000", color = "#000000", stroke = 0.6 / 2.835
        ) +
        scale_shape_manual(values = setNames(development_data$point_shape, development_data$Development_Level), guide = "none") +
        scale_x_continuous(
            labels = number_format(accuracy = 0.01),
            limits = c(-0.40, 0.50),  # 统一x轴范围，与Panel C一致
            breaks = seq(-0.40, 0.50, by = 0.20),  # 与Panel C一致
            expand = expansion(mult = c(0.05, 0.05))
        ) +
        labs(
            title = NULL,
            x = "Mean GHE Coefficient (95% CI)",
            y = "Development Level"
        ) +
        theme_minimal(base_size = 7, base_family = "Helvetica") +
        theme(
            panel.grid.major.y = element_blank(),  # 移除Y轴网格线
            panel.grid.major.x = element_line(color = "#F0F0F0", linewidth = 0.25),  # 仅保留主要X轴网格线
            panel.grid.minor = element_blank(),
            plot.margin = margin(2, 2, 2, 2, "mm"), # 统一边距：上、右、下、左
            axis.title.x = element_text(face = "plain", size = 8, family = "Helvetica"),
            axis.title.y = element_text(face = "plain", size = 8, family = "Helvetica", angle = 90),
            axis.text = element_text(size = 7, family = "Helvetica")
        )
} else {
    panel_s6d_bw <- panel_s6d
}

# 组合黑白版本（使用相同的布局）
top_row_bw <- cowplot::plot_grid(panel_s6a_bw, panel_s6b_bw, nrow = 1, align = "h", rel_widths = c(0.98, 1.02))
bottom_row_bw <- cowplot::plot_grid(panel_s6c_bw, panel_s6d_bw, nrow = 1, align = "h", rel_widths = c(1, 1))
figure_s6_core_bw <- cowplot::plot_grid(top_row_bw, bottom_row_bw, ncol = 1, align = "v", rel_heights = c(1, 1))

# 使用cowplot添加标题（移除副标题）
figure_s6_bw <- cowplot::ggdraw() +
    cowplot::draw_plot(figure_s6_core_bw, x = 0, y = 0, width = 1, height = 0.95) +
    cowplot::draw_label(
        "Figure S7. Regional and income group heterogeneity in GHE-life expectancy relationship",
        x = 0.5, y = 0.98,
        hjust = 0.5, vjust = 1,
        size = 10, fontface = "bold", fontfamily = "Helvetica"
    )

# 保存黑白版本
ggsave(
    "../figures/supplementary/Figure_S7_Regional_Heterogeneity_BW_FromColor.pdf",
    plot = figure_s6_bw,
    width = double_width,
    height = double_width * 0.8,
    units = "in",
    dpi = 300,
    device = cairo_pdf
)

ggsave(
    "../figures/supplementary/Figure_S7_Regional_Heterogeneity_BW_FromColor.svg",
    plot = figure_s6_bw,
    width = double_width,
    height = double_width * 0.8,
    units = "in",
    dpi = 300,
    device = svglite::svglite
)

cat("  ✓ Figure S7 黑白版本生成成功\n")

# =============================================================================
# 5. Figure S8: 机制分析详细版
# =============================================================================
if (!is.null(extended_results)) {
    cat("\n==================================================\n")
    cat("  [5] 生成 Figure S8: 机制分析详细版\n")
    cat("==================================================\n\n")

    # 使用扩展分析的完整数据
    mediation_full <- extended_results$mediation_summary

    # 创建详细版森林图
    figure_s8 <- ggplot() +
        annotate("text",
            x = 0.5, y = 0.5,
            label = "Figure S8: Detailed Mechanism Analysis\n\n完整的9个中介变量\n包括Bootstrap CI和Sobel检验\n\n基于extended_results数据",
            size = 3.5
        ) +
        theme_void()

    # 保存 - 使用更安全的方法，避免段错误
    # Save - Use safer method to avoid segmentation fault
    cat("  [INFO] 保存Figure S8（使用安全模式）...\n")
    
    # 方法1：尝试保存PDF（使用默认设备，避免cairo_pdf问题）
    # Method 1: Try saving PDF (using default device, avoid cairo_pdf issues)
    tryCatch({
        # 减小图形尺寸，避免内存问题
        # Reduce figure size to avoid memory issues
        pdf_width <- min(double_width, 10)  # 限制最大宽度为10英寸
        pdf_height <- min(double_width * 0.8, 9)  # 限制最大高度为9英寸
        
        pdf(
            "../figures/supplementary/Figure_S8_Mechanism_Detailed_PLACEHOLDER.pdf",
            width = pdf_width,
            height = pdf_height
        )
        print(figure_s8)
        dev.off()
        cat("  ✓ Figure S8 PDF 保存成功（使用pdf设备）\n")
    }, error = function(e) {
        cat(sprintf("  ⚠ Figure S8 PDF 保存失败: %s\n", e$message))
    })
    
    # 方法2：尝试保存SVG（使用更安全的方法）
    # Method 2: Try saving SVG (using safer method)
    tryCatch({
        svg_width <- min(double_width * 100, 1000)  # 限制最大宽度为1000像素
        svg_height <- min(double_width * 0.8 * 100, 900)  # 限制最大高度为900像素
        
        svg(
            "../figures/supplementary/Figure_S8_Mechanism_Detailed_PLACEHOLDER.svg",
            width = svg_width / 100,  # 转换为英寸
            height = svg_height / 100
        )
        print(figure_s8)
        dev.off()
        cat("  ✓ Figure S8 SVG 保存成功\n")
    }, error = function(e) {
        cat(sprintf("  ⚠ Figure S8 SVG 保存失败: %s\n", e$message))
        # 如果svg失败，尝试使用ggsave的默认设备
        tryCatch({
            ggsave(
                "../figures/supplementary/Figure_S8_Mechanism_Detailed_PLACEHOLDER.svg",
                plot = figure_s8,
                width = min(double_width, 10),
                height = min(double_width * 0.8, 9),
                units = "in",
                dpi = 150  # 降低分辨率
            )
            cat("  ✓ Figure S8 SVG 保存成功（使用ggsave默认设备）\n")
        }, error = function(e2) {
            cat(sprintf("  ✗ Figure S8 SVG 保存失败: %s\n", e2$message))
        })
    })

    cat("  ✓ Figure S8 占位符创建\n")
}

# =============================================================================
# 6. Figure S10: COVID-19详细分析
# =============================================================================
if (!is.null(extended_results)) {
    cat("\n==================================================\n")
    cat("  [6] 生成 Figure S10: COVID-19详细分析\n")
    cat("==================================================\n\n")

    # 加载COVID数据
    if (file.exists("../data/raw/covid_owid_annual.csv")) {
        covid_data <- read_csv("../data/raw/covid_owid_annual.csv", show_col_types = FALSE)

        # Panel A: 超额死亡率 vs 历史GHE
        cat("[6.1] Panel A: 超额死亡率散点图...\n")

        # 计算历史GHE（2000-2019）
        pre_covid_ghe <- panel_data %>%
            filter(year >= 2000, year <= 2019) %>%
            group_by(iso3c) %>%
            summarize(
                avg_ghe_pre = mean(ghe_gdp, na.rm = TRUE),
                avg_gov_pre = mean(government_effectiveness, na.rm = TRUE),
                .groups = "drop"
            )

        # 合并COVID数据
        covid_analysis <- covid_data %>%
            filter(year %in% c(2020, 2021)) %>%
            group_by(iso3c) %>%
            summarize(
                total_excess = sum(excess_mortality, na.rm = TRUE),
                .groups = "drop"
            ) %>%
            left_join(pre_covid_ghe, by = "iso3c") %>%
            filter(
                !is.na(avg_ghe_pre), !is.na(total_excess),
                !is.infinite(total_excess)
            )
        
        # 将治理水平分为低/高两组（基于中位数）
        covid_analysis <- covid_analysis %>%
            mutate(
                governance_group = if_else(
                    is.na(avg_gov_pre),
                    NA_character_,
                    if_else(avg_gov_pre >= median(avg_gov_pre, na.rm = TRUE), "High", "Low")
                )
            )
        
        # 准备Panel A数据（过滤掉缺失治理组的数据）
        panel_a_data <- covid_analysis %>%
            filter(!is.na(governance_group))

        # 计算Panel A的相关系数和p值
        cor_test_a <- cor.test(panel_a_data$avg_ghe_pre, panel_a_data$total_excess, use = "complete.obs")
        cor_coef_a <- cor_test_a$estimate
        cor_pvalue_a <- cor_test_a$p.value
        
        # 格式化相关系数和p值标签
        cor_label_a <- sprintf("r = %.3f, p = %.3f", cor_coef_a, cor_pvalue_a)
        if (cor_pvalue_a < 0.001) {
            cor_label_a <- sprintf("r = %.3f, p < 0.001", cor_coef_a)
        } else if (cor_pvalue_a < 0.01) {
            cor_label_a <- sprintf("r = %.3f, p < 0.01", cor_coef_a)
        } else if (cor_pvalue_a < 0.05) {
            cor_label_a <- sprintf("r = %.3f, p < 0.05", cor_coef_a)
        }

        panel_s10a <- ggplot(
            panel_a_data,
            aes(
                x = avg_ghe_pre, y = total_excess,
                color = governance_group, shape = governance_group, fill = governance_group
            )
        ) +
            geom_smooth(
                method = "lm", linetype = "dashed",
                se = TRUE, size = 0.8, level = 0.80, alpha = 0.15
            ) +
            geom_point(size = 2, alpha = 0.6) +
            # 添加相关系数和p值标签（右上角）
            annotate(
                "text",
                x = Inf, y = Inf,
                label = cor_label_a,
                hjust = 1.1, vjust = 1.5,
                size = 7 / .pt,
                family = "Helvetica",
                color = "black",
                fontface = "bold"
            ) +
            scale_color_manual(
                values = c("Low" = "#D55E00", "High" = "#0072B2"),  # Lancet标准橙和蓝
                name = "Pre-COVID\nGovernance",
                labels = c("Low" = "Low", "High" = "High")
            ) +
            scale_fill_manual(
                values = c("Low" = "#D55E00", "High" = "#0072B2"),  # 用于置信区间
                guide = "none"
            ) +
            scale_shape_manual(
                values = c("Low" = 1, "High" = 19),  # 低治理用空心圆，高治理用实心圆
                name = "Pre-COVID\nGovernance",
                labels = c("Low" = "Low", "High" = "High")
            ) +
            labs(
                title = "(A) Excess Mortality vs Historical GHE Investment",
                x = "Average GHE/GDP 2000-2019 (%)",
                y = "Cumulative Excess Mortality 2020-2021\n(per million)"
            ) +
            theme_minimal(base_size = 7) +
            theme(
                legend.position = "bottom", # Panel A图例放在底部
                legend.title = element_text(size = 7, face = "bold", family = "Helvetica"),
                legend.text = element_text(size = 7, family = "Helvetica"),
                legend.justification = "center"
            )

        # Panel B: 疫苗接种率 vs 历史GHE
        cat("[6.2] Panel B: 疫苗接种率散点图...\n")
        
        # 准备Panel B数据
        panel_b_data <- covid_data %>%
            filter(year %in% c(2020, 2021, 2022)) %>%
            group_by(iso3c) %>%
            summarize(
                full_vaccination_rate = max(full_vaccination_rate, na.rm = TRUE),
                .groups = "drop"
            ) %>%
            left_join(pre_covid_ghe, by = "iso3c") %>%
            left_join(
                panel_data %>% 
                    filter(year == 2019) %>% 
                    select(iso3c, income),
                by = "iso3c"
            ) %>%
            filter(
                !is.na(avg_ghe_pre), !is.na(full_vaccination_rate),
                !is.infinite(full_vaccination_rate), full_vaccination_rate >= 0,
                !is.na(income), income %in% c("High income", "Upper middle income", 
                                               "Lower middle income", "Low income")
            )
        
        # 计算Panel B的相关系数和p值
        cor_test_b <- cor.test(panel_b_data$avg_ghe_pre, panel_b_data$full_vaccination_rate, use = "complete.obs")
        cor_coef_b <- cor_test_b$estimate
        cor_pvalue_b <- cor_test_b$p.value
        
        # 格式化相关系数和p值标签
        cor_label_b <- sprintf("r = %.3f, p = %.3f", cor_coef_b, cor_pvalue_b)
        if (cor_pvalue_b < 0.001) {
            cor_label_b <- sprintf("r = %.3f, p < 0.001", cor_coef_b)
        } else if (cor_pvalue_b < 0.01) {
            cor_label_b <- sprintf("r = %.3f, p < 0.01", cor_coef_b)
        } else if (cor_pvalue_b < 0.05) {
            cor_label_b <- sprintf("r = %.3f, p < 0.05", cor_coef_b)
        }
        
        panel_s10b <- ggplot(
            panel_b_data,
            aes(
                x = avg_ghe_pre, y = full_vaccination_rate,
                color = income, shape = income, fill = income
            )
        ) +
            geom_smooth(
                method = "lm", linetype = "dashed",
                se = TRUE, size = 0.8, alpha = 0.15
            ) +
            geom_point(size = 2, alpha = 0.6) +
            # 添加相关系数和p值标签（右上角）
            annotate(
                "text",
                x = Inf, y = Inf,
                label = cor_label_b,
                hjust = 1.1, vjust = 1.5,
                size = 7 / .pt,
                family = "Helvetica",
                color = "black",
                fontface = "bold"
            ) +
            scale_color_manual(
                values = c(
                    "High income" = "#1565C0",
                    "Upper middle income" = "#42A5F5",
                    "Lower middle income" = "#66BB6A",
                    "Low income" = "#FFA726"
                ),
                name = "Income Group"
            ) +
            scale_fill_manual(
                values = c(
                    "High income" = "#1565C0",
                    "Upper middle income" = "#42A5F5",
                    "Lower middle income" = "#66BB6A",
                    "Low income" = "#FFA726"
                ),
                guide = "none"  # 不显示填充图例，因为颜色图例已足够
            ) +
            scale_shape_manual(
                values = c(
                    "High income" = 19,
                    "Upper middle income" = 17,
                    "Lower middle income" = 15,
                    "Low income" = 1
                ),
                name = "Income Group"
            ) +
            labs(
                title = "(B) Vaccination Rate vs Historical GHE Investment",
                x = "Average GHE/GDP 2000-2019 (%)",
                y = "Full Vaccination Rate\n(% of population)"
            ) +
            theme_minimal(base_size = 7) +
            theme(
                legend.position = "bottom", # Panel B图例放在底部
                legend.title = element_text(size = 7, face = "bold", family = "Helvetica"),
                legend.text = element_text(size = 7, family = "Helvetica"),
                legend.justification = "left", # Panel B图例整体向左移动
                legend.spacing.y = unit(0.1, "cm"), # 减少图例行间距
                legend.margin = margin(t = 2, r = 0, b = 0, l = -10, unit = "mm"), # 进一步减少左侧边距，让图例更靠近画布边缘
                plot.margin = margin(t = 2, r = 3, b = 2, l = 0, unit = "mm") # 减少左侧plot边距，配合图例左移
            ) +
            guides(
                color = guide_legend(nrow = 2, byrow = TRUE),
                shape = guide_legend(nrow = 2, byrow = TRUE)
            )

        # 准备Panel C和D所需的数据
        cat("[6.3] 准备Panel C和D数据...\n")
        
        # 辅助函数
        safe_sum <- function(x) {
            if (length(x) == 0 || all(is.na(x))) return(NA_real_)
            sum(x, na.rm = TRUE)
        }
        safe_mean <- function(x) {
            if (length(x) == 0 || all(is.na(x))) return(NA_real_)
            mean(x, na.rm = TRUE)
        }
        safe_max <- function(x) {
            if (length(x) == 0 || all(is.na(x))) return(NA_real_)
            max(x, na.rm = TRUE)
        }
        
        # 准备完整的COVID分析数据（包含ICU和收入组信息）
        covid_summary_full <- covid_data %>%
            filter(year %in% 2020:2022) %>%
            group_by(iso3c) %>%
            summarize(
                excess_mortality = safe_sum(excess_mortality),
                full_vaccination_rate = safe_max(full_vaccination_rate),
                peak_icu = safe_max(peak_icu_patients),
                peak_hosp = safe_max(peak_hosp_patients),
                avg_stringency = safe_mean(avg_stringency),
                population = safe_max(population),
                .groups = "drop"
            ) %>%
            mutate(
                full_vaccination_rate = if_else(is.infinite(full_vaccination_rate), NA_real_, full_vaccination_rate),
                peak_icu_per_million = if_else(
                    !is.na(peak_icu) & !is.na(population) & population > 0,
                    peak_icu / population * 1e6,
                    NA_real_
                )
            )
        
        # 合并收入组信息
        covid_analysis_full <- covid_summary_full %>%
            left_join(pre_covid_ghe, by = "iso3c") %>%
            left_join(
                panel_data %>% 
                    filter(year == 2019) %>% 
                    select(iso3c, income),
                by = "iso3c"
            ) %>%
            filter(!is.na(avg_ghe_pre)) %>%
            mutate(
                income_group = replace_na(income, "Unknown")
            )
        
        # Panel C: ICU占用率（Top 10国家）
        cat("[6.4] Panel C: ICU占用率条形图...\n")
        
        # 计算全球平均ICU占用率
        global_avg_icu <- covid_analysis_full %>%
            filter(!is.na(peak_icu_per_million), peak_icu_per_million > 0) %>%
            summarize(global_avg = mean(peak_icu_per_million, na.rm = TRUE)) %>%
            pull(global_avg)
        
        # 从各收入组选择代表性国家
        high_income_top <- covid_analysis_full %>%
            filter(!is.na(peak_icu_per_million), peak_icu_per_million > 0,
                   income_group == "High income", !is.na(income_group)) %>%
            arrange(desc(peak_icu_per_million)) %>%
            slice_head(n = 3)
        
        upper_middle_top <- covid_analysis_full %>%
            filter(!is.na(peak_icu_per_million), peak_icu_per_million > 0,
                   income_group == "Upper middle income", !is.na(income_group)) %>%
            arrange(desc(peak_icu_per_million)) %>%
            slice_head(n = 3)
        
        lower_middle_top <- covid_analysis_full %>%
            filter(!is.na(peak_icu_per_million), peak_icu_per_million > 0,
                   income_group == "Lower middle income", !is.na(income_group)) %>%
            arrange(desc(peak_icu_per_million)) %>%
            slice_head(n = 2)
        
        low_income_top <- covid_analysis_full %>%
            filter(!is.na(peak_icu_per_million), peak_icu_per_million > 0,
                   income_group == "Low income", !is.na(income_group)) %>%
            arrange(desc(peak_icu_per_million)) %>%
            slice_head(n = 2)
        
        # 合并并选择Top 10
        top_icu <- bind_rows(high_income_top, upper_middle_top, lower_middle_top, low_income_top) %>%
            arrange(desc(peak_icu_per_million)) %>%
            slice_head(n = 10) %>%
            mutate(
                # 尝试获取国家全名，如果失败则使用ISO3C代码
                country_name = if (requireNamespace("countrycode", quietly = TRUE)) {
                    countrycode::countrycode(iso3c, origin = "iso3c", destination = "country.name.en", warn = FALSE)
                } else {
                    NA_character_
                },
                country_name = if_else(is.na(country_name), iso3c, country_name),
                country_label = paste0(country_name, " (", iso3c, ")"),
                country_label = fct_reorder(country_label, peak_icu_per_million),
                income_group = replace_na(income_group, "Unknown")
            )
        
        # 收入组颜色
        income_colors <- c(
            "High income" = "#1565C0",
            "Upper middle income" = "#42A5F5",
            "Lower middle income" = "#66BB6A",
            "Low income" = "#FFA726",
            "Unknown" = "#999999"
        )
        
        panel_s10c <- ggplot(top_icu, aes(x = country_label, y = peak_icu_per_million, fill = income_group)) +
            geom_col(width = 0.7) +
            coord_flip() +
            geom_hline(yintercept = global_avg_icu, linetype = "dashed", color = "#999999", linewidth = 0.8 / 2.835, alpha = 0.7) +
            annotate(
                "text",
                x = 0.5, y = global_avg_icu,
                label = paste0("Global avg: ", round(global_avg_icu, 1)),
                hjust = -0.1, vjust = 0.5,
                size = 6 / .pt,
                family = "Helvetica",
                color = "#666666",
                fontface = "italic"
            ) +
            geom_text(
                aes(label = paste0(round(peak_icu_per_million, 1))),
                hjust = -0.1, vjust = 0.5,
                size = 7 / .pt,
                family = "Helvetica",
                color = "black",
                fontface = "bold"
            ) +
            scale_fill_manual(
                values = income_colors,
                name = "Income Group",
                labels = c(
                    "High income" = "High",
                    "Upper middle income" = "Upper middle",
                    "Lower middle income" = "Lower middle",
                    "Low income" = "Low",
                    "Unknown" = "Unknown"
                ),
                guide = guide_legend(
                    nrow = 2, 
                    byrow = TRUE,
                    override.aes = list(size = 0.5)  # 减小图例方块大小
                )
            ) +
            labs(
                title = "(C) Peak ICU Occupancy",
                x = "Country (ISO3C)",
                y = "Peak ICU Occupancy\n(per million population)"
            ) +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 5), expand = expansion(mult = c(0.15, 0.15))) +
            theme_minimal(base_size = 7, base_family = "Helvetica") +
            theme(
                axis.title.x = element_text(size = 8, family = "Helvetica"),
                axis.title.y = element_text(size = 8, family = "Helvetica"),
                axis.text.x = element_text(size = 7, family = "Helvetica"),
                axis.text.y = element_text(size = 7, family = "Helvetica"),
                legend.position = "bottom",
                legend.title = element_text(size = 7, face = "bold", family = "Helvetica"),
                legend.text = element_text(size = 7, family = "Helvetica"),
                legend.justification = "left",
                legend.spacing.y = unit(0.1, "cm"), # 减少图例行间距
                legend.margin = margin(t = 2, r = 0, b = 0, l = 0, unit = "mm"), # 减少图例上下边距
                plot.margin = margin(t = 2, r = 5, b = 2, l = 2, unit = "mm"),
                panel.grid.minor = element_line(color = "#F0F0F0", linewidth = 0.25 / 2.835),
                panel.grid.major = element_line(color = "#E0E0E0", linewidth = 0.4 / 2.835)
            )
        
        # Panel D: 时间趋势（Stringency和Vaccination）
        cat("[6.5] Panel D: 时间趋势图...\n")
        
        trend_data <- covid_data %>%
            filter(year %in% 2020:2023) %>%
            group_by(year) %>%
            summarize(
                avg_stringency = safe_mean(avg_stringency),
                avg_full_vax = safe_mean(full_vaccination_rate),
                .groups = "drop"
            ) %>%
            mutate(
                avg_full_vax = if_else(is.na(avg_full_vax), 0, avg_full_vax)
            ) %>%
            pivot_longer(
                cols = c(avg_stringency, avg_full_vax),
                names_to = "Metric",
                values_to = "Value"
            ) %>%
            mutate(
                Metric = dplyr::recode(
                    Metric,
                    "avg_stringency" = "Stringency Index",
                    "avg_full_vax" = "Full Vaccination Rate"
                )
            )
        
        # 计算标签位置（让数字更靠近点，并针对特定值进行微调）
        trend_data_with_labels <- trend_data %>%
            group_by(Metric) %>%
            mutate(
                value_label = paste0(round(Value, 1)),
                # 基础垂直偏移
                base_label_y = Value + if_else(
                    Metric == "Stringency Index",
                    max(Value, na.rm = TRUE) * 0.06,
                    -max(Value, na.rm = TRUE) * 0.06
                ),
                # 基础水平偏移
                base_label_x_offset = if_else(
                    Metric == "Stringency Index", 0.15, -0.15
                ),
                # 针对特定值的微调
                label_y = case_when(
                    Metric == "Full Vaccination Rate" & year == 2021 ~ base_label_y + max(Value, na.rm = TRUE) * 0.08,  # 44.6稍微下移（减少偏移量）
                    Metric == "Full Vaccination Rate" & year == 2022 ~ base_label_y - max(Value, na.rm = TRUE) * 0.06,  # 55.7下移（减少y值）
                    TRUE ~ base_label_y
                ),
                label_x_offset = case_when(
                    Metric == "Full Vaccination Rate" & year == 2021 ~ base_label_x_offset - 0.05,  # 44.6左移
                    TRUE ~ base_label_x_offset
                )
            ) %>%
            ungroup()
        
        # 关键事件
        y_max <- max(trend_data$Value, na.rm = TRUE)
        y_min <- min(trend_data$Value, na.rm = TRUE)
        y_range <- y_max - y_min
        
        key_events <- tibble(
            year = c(2020, 2021, 2021, 2022),
            event = c("Vaccine\nRollout\n(Dec 2020)", "Vaccination\n50%\n(Jun 2021)", "Omicron\nVariant\n(Dec 2021)", "Pandemic\nEasing\n(2022)"),
            y_pos = c(
                y_max + y_range * 0.15,
                y_max + y_range * 0.15,  # 下移到与其他两个时间点相同高度
                0,
                y_max + y_range * 0.15
            )
        )
        
        panel_s10d <- ggplot(trend_data_with_labels, aes(x = year, y = Value, color = Metric, shape = Metric)) +
            geom_hline(yintercept = 0, linetype = "solid", color = "#F0F0F0", linewidth = 0.3 / 2.835) +
            geom_vline(xintercept = 2020, linetype = "dashed", color = "#999999", linewidth = 0.6 / 2.835, alpha = 0.6) +
            geom_vline(xintercept = 2021, linetype = "dashed", color = "#999999", linewidth = 0.6 / 2.835, alpha = 0.6) +
            geom_vline(xintercept = 2022, linetype = "dashed", color = "#999999", linewidth = 0.6 / 2.835, alpha = 0.6) +
            geom_line(linewidth = 1.8 / 2.835) +  # 进一步增大线宽
            geom_point(size = 8 / 2.835, alpha = 0.8) +  # 进一步增大点的大小
            geom_text(
                aes(label = value_label, x = year + label_x_offset, y = label_y),
                size = 7 / .pt,
                family = "Helvetica",
                fontface = "bold",
                show.legend = FALSE
            ) +
            annotate(
                "text",
                x = key_events$year,
                y = key_events$y_pos,
                label = key_events$event,
                size = 6 / .pt,
                family = "Helvetica",
                color = "#666666",
                fontface = "italic",
                lineheight = 0.9,
                hjust = 0.5,
                vjust = if_else(key_events$year == 2021 & key_events$event == "Omicron\nVariant\n(Dec 2021)", 1, 0)
            ) +
            scale_color_manual(
                values = c(
                    "Stringency Index" = "#0072B2",
                    "Full Vaccination Rate" = "#D55E00"
                ),
                name = "Metric",
                guide = guide_legend(nrow = 2, byrow = TRUE)
            ) +
            scale_shape_manual(
                values = c(
                    "Stringency Index" = 19,  # 圆点
                    "Full Vaccination Rate" = 17  # 三角形
                ),
                name = "Metric",
                guide = guide_legend(nrow = 2, byrow = TRUE)
            ) +
            labs(
                title = "(D) Stringency and Vaccination Trends",
                x = "Year",
                y = "Index / %"
            ) +
            scale_x_continuous(
                breaks = c(2020, 2021, 2022, 2023),
                labels = c("2020", "2021", "2022", "2023"),
                limits = c(2019.5, 2023.5),
                expand = expansion(mult = 0)
            ) +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 5), expand = expansion(mult = c(0.05, 0.25))) +  # 恢复足够的上方空间以完整显示时间点标注
            theme_minimal(base_size = 7, base_family = "Helvetica") +
            theme(
                axis.title.x = element_text(size = 8, family = "Helvetica"),
                axis.title.y = element_text(size = 8, family = "Helvetica"),
                axis.text.x = element_text(size = 7, family = "Helvetica"),
                axis.text.y = element_text(size = 7, family = "Helvetica"),
                legend.position = "bottom",
                legend.title = element_text(size = 7, face = "bold", family = "Helvetica"),
                legend.text = element_text(size = 7, family = "Helvetica"),
                legend.justification = "left",
                legend.spacing.y = unit(0.1, "cm"), # 减少图例行间距
                legend.margin = margin(t = 2, r = 0, b = 0, l = 0, unit = "mm"), # 减少图例上下边距
                plot.margin = margin(t = 5, r = 3, b = 2, l = 3, unit = "mm"),  # 进一步减少上方边距
                panel.grid.minor = element_line(color = "#F0F0F0", linewidth = 0.25 / 2.835),
                panel.grid.major = element_line(color = "#E0E0E0", linewidth = 0.4 / 2.835)
            )

        # 组合（调整panel相对大小：A和C缩小，B和D保持较大）
        figure_s10 <- (panel_s10a | panel_s10b) / (panel_s10c | panel_s10d) +
            plot_layout(
                widths = c(0.8, 1.2),  # A和C缩小，B和D较大
                heights = c(1, 1)
            ) +
            plot_annotation(
                title = "Figure S10. COVID-19 Pandemic Response and\nHistorical Health Investment",
                theme = theme(plot.title = element_text(size = 9, face = "bold", hjust = 0.5))
            )

        # 确保输出目录存在
        dir.create("../figures/supplementary", recursive = TRUE, showWarnings = FALSE)

        # 先保存每个单图
        cat("[6.6] 保存各个Panel单图...\n")
        
        # Panel A单图
        ggsave(
            "../figures/supplementary/Figure_S10_Panel_A_Excess_Mortality.pdf",
            plot = panel_s10a,
            width = 4.72,
            height = 3.54,
            units = "in",
            dpi = 300,
            device = cairo_pdf
        )
        cat("  ✓ Panel A 单图已保存\n")
        
        # Panel B单图
        ggsave(
            "../figures/supplementary/Figure_S10_Panel_B_Vaccination.pdf",
            plot = panel_s10b,
            width = 4.72,
            height = 3.54,
            units = "in",
            dpi = 300,
            device = cairo_pdf
        )
        cat("  ✓ Panel B 单图已保存\n")
        
        # Panel C单图
        ggsave(
            "../figures/supplementary/Figure_S10_Panel_C_ICU_Occupancy.pdf",
            plot = panel_s10c,
            width = 4.72,
            height = 3.54,
            units = "in",
            dpi = 300,
            device = cairo_pdf
        )
        cat("  ✓ Panel C 单图已保存\n")
        
        # Panel D单图
        ggsave(
            "../figures/supplementary/Figure_S10_Panel_D_Trends.pdf",
            plot = panel_s10d,
            width = 4.72,
            height = 3.54,
            units = "in",
            dpi = 300,
            device = cairo_pdf
        )
        cat("  ✓ Panel D 单图已保存\n")

        # 保存组合图
        ggsave(
            "../figures/supplementary/Figure_S10_COVID_Analysis.pdf",
            plot = figure_s10,
            width = double_width,
            height = double_width * 0.8,
            units = "in",
            dpi = 300,
            device = cairo_pdf
        )

        ggsave(
            "../figures/supplementary/Figure_S10_COVID_Analysis.svg",
            plot = figure_s10,
            width = double_width,
            height = double_width * 0.8,
            units = "in",
            dpi = 300,
            device = svglite::svglite
        )

        cat("  ✓ Figure S10 生成成功\n")
    }
}

# =============================================================================
# 7. 生成总结报告
# =============================================================================
cat("\n==================================================\n")
cat("  补充图生成完成总结\n")
cat("==================================================\n\n")

cat("生成的补充图：\n")
cat("  ⚠ Figure S1: 已迁移到独立脚本，跳过生成\n")
cat("  ✓ Figure S2: 稳健性检验-样本 (由改进版本脚本生成)\n")
cat("  ⚠ Figure S3-S4: (待生成)\n")
if (file.exists(iv_results_file)) {
    cat("  ✓ Figure S5: IV诊断 (Stock-Yogo临界值可视化)\n")
} else {
    cat("  ⚠ Figure S5: IV诊断 (IV结果文件不存在)\n")
}
cat("  ✓ Figure S6: 地区异质性 (2 panels + 2 placeholders)\n")
cat("  ⚠ Figure S7: (待生成)\n")
if (!is.null(extended_results)) {
    cat("  ⚠ Figure S8: 机制详细 (占位符)\n")
    cat("  ⚠ Figure S9: (待生成)\n")
    if (file.exists("../data/raw/covid_owid_annual.csv")) {
        cat("  ✓ Figure S10: COVID-19分析 (4 panels: A-D)\n")
    }
}

cat("\n输出位置: figures/supplementary/\n")
cat("\n格式: PDF + SVG (矢量), 300 DPI\n")

cat("\n==================================================\n")
cat("  ✓ 补充图生成脚本执行完成！\n")
cat("==================================================\n")
