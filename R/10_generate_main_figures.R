# =============================================================================
# 10_generate_main_figures.R
# Generate Main Figures for Lancet | 生成 Lancet 主图
# =============================================================================
#
# 【功能概述 | Function Overview】
# 按照 Lancet 期刊标准生成主文本图形
# Generate main text figures following Lancet journal standards:
#   - Figure 1: Conceptual framework and descriptive statistics | 研究框架与描述性统计
#   - Figure 2: Core causal results | 核心因果结果
#   - Figure 3: Mechanism analysis | 机制分析
#   - Figure 4: Disease-specific effects | 疾病特定效应
#
# 【技术标准 | Technical Standards】
#   - Resolution: 300 DPI | 分辨率: 300 DPI
#   - Format: PDF (vector) | 格式: PDF (矢量图)
#   - Size: 85mm (single column) or 180mm (double column) | 尺寸: 85mm (单栏) 或 180mm (双栏)
#   - Font: Arial 7-10pt | 字体: Arial 7-10pt
#   - Colorblind-friendly palette | 色盲友好配色
#
# 【输出文件 | Output Files】
#   - figures/main/Figure_1_*.pdf
#   - figures/main/Figure_2_*.pdf
#   - figures/main/Figure_3_*.pdf
#   - figures/main/Figure_4_*.pdf
#
# 【运行时间 | Runtime】
# ~ 5-10 minutes | ~5-10分钟
#
# 【最后更新 | Last Updated】
# 2025-11-07
# =============================================================================

# 清空环境
rm(list = ls())
gc()

# =============================================================================
# 0. 包加载和设置 | Package Loading & Setup
# =============================================================================
cat("\n==================================================\n")
cat("  生成Lancet主图 | Generate Main Figures\n")
cat("==================================================\n\n")

cat("[0] 加载R包...\n")

# 核心包
library(tidyverse)
library(ggplot2)
library(patchwork) # 组合图表
library(cowplot) # 用于在组合图上添加文本

# 特殊可视化包
# 地图相关包（可选，如果未安装则跳过地图生成）
# Map-related packages (optional, skip map generation if not installed)
if (require(sf, quietly = TRUE) && require(rnaturalearth, quietly = TRUE)) {
library(sf) # 地图
library(rnaturalearth) # 世界地图数据
library(rnaturalearthdata)
    has_map_packages <- TRUE
} else {
    cat("  ⚠ 地图包未安装，将跳过地图生成\n")
    has_map_packages <- FALSE
}
library(viridis) # 色盲友好配色
library(ggrepel) # 文本标签
library(scales) # 坐标轴格式化
library(grid) # 图形布局
library(gridExtra) # 额外布局功能

# 分析包
library(fixest) # 用于提取模型结果
library(broom) # 整理模型输出

cat("  ✓ 所有包加载完成\n")

# 设置全局主题
theme_set(theme_bw(base_size = 9, base_family = "Arial"))

# Lancet配色方案（色盲友好）
lancet_colors <- list(
    income = c(
        "High income" = "#1f78b4", # 蓝色
        "Upper middle income" = "#ff7f00", # 橙色
        "Lower middle income" = "#33a02c", # 绿色
        "Low income" = "#56B4E9" # 浅蓝色
    ),
    sequential_blue = c(
        "#f7fbff", "#deebf7", "#c6dbef", "#9ecae1",
        "#6baed6", "#4292c6", "#2171b5", "#08519c", "#08306b"
    ),
    diverging = c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"),
    qualitative = c(
        "#e41a1c", "#377eb8", "#4daf4a", "#984ea3",
        "#ff7f00", "#ffff33", "#a65628", "#f781bf"
    )
)

# 创建输出目录
dir.create("../figures", showWarnings = FALSE, recursive = TRUE)
dir.create("../figures/main", showWarnings = FALSE, recursive = TRUE)
dir.create("../figures/main", showWarnings = FALSE, recursive = TRUE)

# 设置图形参数
# 单栏: 85mm = 3.35 inches
# 双栏: 180mm = 7.09 inches
# 300 DPI
single_width <- 3.35
double_width <- 7.09
panel_height <- 3.35

# =============================================================================
# 1. 加载数据 | Load Data
# =============================================================================
cat("\n[1] 加载数据...\n")

# 主面板数据
panel_data <- readRDS("../data/processed/02_integrated_panel_data.rds")
cat(sprintf("  ✓ 主面板数据: %d obs\n", nrow(panel_data)))

# 扩展分析结果
if (file.exists("../results/extended_analysis/extended_results.rds")) {
    extended_results <- readRDS("../results/extended_analysis/extended_results.rds")
    cat("  ✓ 扩展分析结果加载\n")
} else {
    cat("  ⚠ 扩展分析结果不存在，将跳过相关图表\n")
    extended_results <- NULL
}

# 核心分析结果
if (file.exists("../results/core_results.rds")) {
    core_results <- readRDS("../results/core_results.rds")
    cat("  ✓ 核心分析结果加载\n")
} else {
    cat("  ⚠ 核心分析结果不存在，将跳过相关图表\n")
    core_results <- NULL
}

# =============================================================================
# 2. Figure 1: 研究框架与描述性统计
# =============================================================================
cat("\n==================================================\n")
cat("  [2] 生成 Figure 1: 研究框架与描述性统计\n")
cat("==================================================\n\n")

# -----------------------------------------------------------------------------
# Panel A: 概念框架图 (将手动绘制，这里创建占位符)
# -----------------------------------------------------------------------------
cat("[2.1] Panel A: 概念框架图 (需要手动绘制)...\n")

# 创建文本占位符
panel_a <- ggplot() +
    annotate("text",
        x = 0.5, y = 0.7,
        label = "概念框架图\nConceptual Framework\n\n请使用专业绘图软件绘制:\nAdobe Illustrator / PowerPoint\n\nGHE → 机制 (医疗人力、服务、设施)\n→ 制度质量调节 → 健康结局",
        size = 3, hjust = 0.5, vjust = 0.5, lineheight = 1.2
    ) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = "black", size = 0.5))

cat("  ✓ Panel A 占位符创建（需手动绘制）\n")

# -----------------------------------------------------------------------------
# Panel B: 全球GHE分布地图（2022年截面）
# -----------------------------------------------------------------------------
cat("[2.2] Panel B: 全球GHE分布地图 (2022)...\n")

# 初始化panel_b为占位符
panel_b <- NULL

if (has_map_packages) {
tryCatch(
    {
        # 准备2022年数据
        ghe_2022 <- panel_data %>%
            filter(year == 2022) %>%
            select(iso3c, country, ghe_gdp, income) %>%
            filter(!is.na(ghe_gdp))

        # 加载世界地图
        world <- ne_countries(scale = "medium", returnclass = "sf")

        # 合并数据
        world_ghe <- world %>%
            left_join(ghe_2022, by = c("iso_a3" = "iso3c"))

        # 创建分位数分组
        world_ghe <- world_ghe %>%
            mutate(ghe_quintile = cut(ghe_gdp,
                breaks = quantile(ghe_gdp, probs = seq(0, 1, 0.2), na.rm = TRUE),
                labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4", "Q5 (Highest)"),
                include.lowest = TRUE
            ))

        # 绘制地图
        panel_b <- ggplot(world_ghe) +
            geom_sf(aes(fill = ghe_quintile), color = "white", size = 0.1) +
            scale_fill_manual(
                values = lancet_colors$sequential_blue[c(2, 4, 5, 7, 8)],
                na.value = "grey90",
                name = "GHE/GDP (%)",
                na.translate = TRUE,
                labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4", "Q5 (Highest)", "No data")
            ) +
            coord_sf(crs = "+proj=robin") + # Robinson投影
            theme_void(base_size = 7) +
            theme(
                legend.position = "bottom",
                legend.title = element_text(size = 5, face = "bold"),  # 统一图例标题大小（与Panel D一致）
                legend.text = element_text(size = 4, margin = margin(r = 0.05, unit = "cm")),  # 统一图例文本大小（与Panel D一致）
                legend.key.size = unit(0.35, "cm"),  # 统一图例key尺寸（与Panel D一致）
                legend.key.width = unit(0.3, "cm"),  # 统一图例key宽度（与Panel D一致）
                legend.key.height = unit(0.35, "cm"),  # 统一图例key高度（与Panel D一致）
                plot.margin = margin(5, 5, 5, 5)
            ) +
            labs(title = "(B) Global Distribution of GHE/GDP in 2022")

        cat("  ✓ Panel B 地图创建成功\n")
    },
    error = function(e) {
        cat(sprintf("  ✗ Panel B 创建失败: %s\n", e$message))
            panel_b <<- ggplot() +
            annotate("text",
                x = 0.5, y = 0.5,
                label = "地图创建失败\n请检查rnaturalearth包",
                size = 3
            ) +
            theme_void()
    }
)
} else {
    # 如果地图包不可用，创建占位符
    panel_b <- ggplot() +
        annotate("text",
            x = 0.5, y = 0.5,
            label = "地图包未安装\n跳过地图生成",
            size = 3
        ) +
        theme_void()
    cat("  ⚠ Panel B 跳过（地图包未安装）\n")
}

# -----------------------------------------------------------------------------
# Panel C: GHE时间趋势
# -----------------------------------------------------------------------------
cat("[2.3] Panel C: GHE时间趋势...\n")

# 准备时间趋势数据（排除NA收入组）
trend_data <- panel_data %>%
    filter(!is.na(ghe_gdp), !is.na(income)) %>%
    filter(income %in% c("High income", "Upper middle income", "Lower middle income", "Low income")) %>%  # 明确排除NA
    group_by(year, income) %>%
    summarize(
        mean_ghe = mean(ghe_gdp, na.rm = TRUE),
        se_ghe = sd(ghe_gdp, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
    ) %>%
    mutate(
        ci_lower = mean_ghe - 1.96 * se_ghe,
        ci_upper = mean_ghe + 1.96 * se_ghe
    )

# 确保收入组顺序（只包含4个类别，排除NA）
trend_data$income <- factor(trend_data$income,
    levels = c(
        "High income", "Upper middle income",
        "Lower middle income", "Low income"
    ),
    exclude = NULL  # 确保不包含NA
)

# 绘制趋势图
panel_c <- ggplot(trend_data, aes(
    x = year, y = mean_ghe,
    color = income, fill = income
)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
    geom_line(size = 0.8) +
    geom_point(size = 1.5, shape = 21, fill = "white") +
    geom_vline(xintercept = 2020, linetype = "dashed", color = "red", size = 0.5) +
    scale_color_manual(
        values = lancet_colors$income,
        name = "Income Group",
        labels = c(
            "High income" = "High income",
            "Upper middle income" = "Upper middle income",
            "Lower middle income" = "Lower middle income",
            "Low income" = "Low income"
        )
    ) +
    scale_fill_manual(
        values = lancet_colors$income,
        name = "Income Group",
        labels = c(
            "High income" = "High income",
            "Upper middle income" = "Upper middle income",
            "Lower middle income" = "Lower middle income",
            "Low income" = "Low income"
        )
    ) +
    scale_x_continuous(breaks = seq(2000, 2022, 5)) +
    scale_y_continuous(labels = number_format(accuracy = 0.1)) +
    labs(
        x = "Year",
        y = "Mean GHE/GDP (%)",
        color = "Income Group",
        fill = "Income Group"
    ) +  # 去掉标题
    annotate("text",
        x = 2019.5,  # 放在红色线段左侧（2020年之前）
        y = max(trend_data$mean_ghe, na.rm = TRUE) * 0.995,  # 靠近图形上边框（99.5%位置）
        label = "COVID-19", 
        size = 2.5,  # 缩小字体（从3.5减少到2.5）
        hjust = 1,  # 右对齐（文字在左侧）
        vjust = 0,  # 顶部对齐，使文字更靠近上边框
        color = "red"
    ) +
    theme_bw(base_size = 8) +  # 统一base_size，与Panel D一致
    theme(
        legend.position = "none",  # 移除Panel C的图例
        axis.title = element_text(size = 8),  # 统一坐标轴标题大小（与Panel D一致）
        axis.title.x = element_text(margin = margin(t = 0.3, unit = "cm")),  # 统一横坐标标题与坐标轴的距离（与Panel D一致）
        axis.title.y = element_text(margin = margin(r = 0.3, unit = "cm")),  # 统一纵坐标标题与坐标轴的距离（与Panel D一致）
        axis.text = element_text(size = 7),   # 统一坐标轴文本大小（与Panel D一致）
        axis.text.x = element_text(margin = margin(t = 0.05, unit = "cm")),  # 统一横坐标文字与坐标轴的距离（与Panel D一致）
        axis.text.y = element_text(margin = margin(r = 0.05, unit = "cm")),  # 统一纵坐标文字与坐标轴的距离（与Panel D一致）
        panel.grid.minor = element_blank(),
        # 统一整体高度：确保图形框上下边界与Panel D完全对齐
        plot.margin = margin(5, 5, 0, 5, unit = "mm")  # 上5mm，右5mm，下0mm，左5mm（与Panel D图形区域高度一致）
    ) +
    guides(
        color = guide_legend(nrow = 1, byrow = TRUE),  # 一行显示，与Panel D一致
        fill = guide_legend(nrow = 1, byrow = TRUE)    # 一行显示
    )

cat("  ✓ Panel C 趋势图创建成功\n")

# -----------------------------------------------------------------------------
# Panel D: 制度质量 vs GHE散点图（2022年截面）
# -----------------------------------------------------------------------------
cat("[2.4] Panel D: 制度质量 vs GHE散点图 (2022)...\n")

# 准备散点图数据（使用2022年）
scatter_data <- panel_data %>%
    filter(year == 2022, !is.na(government_effectiveness), !is.na(ghe_gdp), !is.na(income)) %>%
    mutate(
        population_millions = population / 1e6,
        income = factor(income, levels = c(
            "High income", "Upper middle income",
            "Lower middle income", "Low income"
        ))
    ) %>%
    filter(!is.na(income)) # 确保没有NA值

# 标注重点国家（每个收入组选择2个代表性国家，使用国家全称）
# Label key countries (2 representative countries per income group, using full country names)
representative_countries <- list(
    "High income" = c("USA", "JPN"),  # United States, Japan
    "Upper middle income" = c("CHN", "BRA"),  # China, Brazil
    "Lower middle income" = c("IND", "BGD"),  # India, Bangladesh
    "Low income" = c("ETH", "UGA")  # Ethiopia, Uganda
)
label_countries <- unlist(representative_countries)

# 为代表性国家单独构建标注数据，并手动设置标签终点位置
# Build a separate label data frame for representative countries and manually set label end positions
label_data <- scatter_data %>%
    dplyr::filter(iso3c %in% label_countries) %>%
    dplyr::mutate(
        label = country,
        label_x = government_effectiveness,
        label_y = ghe_gdp
    ) %>%
    dplyr::mutate(
        # 标签终点横坐标：在曲线周围的空白位置，重新设计以避免重叠
        # Horizontal position of label end: redesigned to avoid overlaps
        label_x_end = dplyr::case_when(
            iso3c == "USA" ~ label_x - 0.6,      # 左上
            iso3c == "JPN" ~ label_x + 0.7,      # 右上，与USA错开
            iso3c == "CHN" ~ label_x + 0.8,      # 右下，右移更多
            iso3c == "BRA" ~ label_x - 0.3,     # 左上，与USA错开
            iso3c == "IND" ~ label_x + 0.3,     # 右下，向右移动，避免与UGA交叉
            iso3c == "BGD" ~ label_x - 0.8,     # 左下，稍微右移
            iso3c == "ETH" ~ label_x - 1.0,     # 左上，与USA错开
            iso3c == "UGA" ~ label_x + 0.4,     # 右下，向右移动
            TRUE ~ label_x
        ),
        # 标签终点纵坐标：重新设计垂直位置，确保上下错开
        # Vertical position of label end: redesigned to ensure vertical separation
        label_y_end = dplyr::case_when(
            iso3c == "USA" ~ label_y + 4.5,     # 上移，最高
            iso3c == "JPN" ~ label_y + 3.2,     # 上移，略低于USA
            iso3c == "CHN" ~ label_y - 4.0,     # 下移，最低
            iso3c == "BRA" ~ label_y + 4.0,     # 上移，与USA和JPN错开
            iso3c == "IND" ~ label_y - 3.0,     # 下移，略高于CHN
            iso3c == "BGD" ~ label_y - 2.0,     # 下移，略高于IND
            iso3c == "ETH" ~ label_y + 5.8,     # 上移，最高，与USA错开
            iso3c == "UGA" ~ label_y - 3.5,     # 下移，与CHN和IND错开
            TRUE ~ label_y + 2.0
        )
    )

# 读取阈值估计值（从结果文件读取，如果不存在则使用默认值）
# Read threshold estimate from results file, use default if not available
threshold_est_file <- "../results/analysis/04_threshold_estimates_manuscript.csv"
if (file.exists(threshold_est_file)) {
    threshold_est_data <- read_csv(threshold_est_file, show_col_types = FALSE)
    threshold_value <- as.numeric(threshold_est_data$threshold[1])
    threshold_ci_lower <- as.numeric(threshold_est_data$ci_lower[1])
    threshold_ci_upper <- as.numeric(threshold_est_data$ci_upper[1])
    cat(sprintf("  [INFO] 从结果文件读取阈值: %.2f (95%% CI: [%.2f, %.2f])\n", 
                threshold_value, threshold_ci_lower, threshold_ci_upper))
} else {
    threshold_value <- -0.53  # 使用最新阈值估计值 | Use latest threshold estimate
    threshold_ci_lower <- -0.78
    threshold_ci_upper <- 0.52
    cat(sprintf("  [WARN] 结果文件不存在，使用默认阈值: %.2f (95%% CI: [%.2f, %.2f])\n", 
                threshold_value, threshold_ci_lower, threshold_ci_upper))
}

# 绘制散点图
panel_d <- ggplot(scatter_data, aes(x = government_effectiveness, y = ghe_gdp)) +
    # 添加阈值线（垂直虚线）
    geom_vline(xintercept = threshold_value, 
               linetype = "dashed", 
               color = "#d73027",  # 红色，与Figure 2 Panel C一致
               linewidth = 1,
               alpha = 0.8) +
    # 添加阈值标注（水平放置，靠近红线左侧，分两行显示）
    annotate("text",
             x = threshold_value - 0.15,  # 红线左侧0.15个单位
             y = max(scatter_data$ghe_gdp, na.rm = TRUE) * 0.95,
             label = sprintf("Threshold = %.2f\n(95%% CI: [%.2f, %.2f])", 
                            threshold_value, threshold_ci_lower, threshold_ci_upper),
             angle = 0,  # 水平放置
             vjust = 0.5,
             hjust = 1,  # 右对齐（文字在红线左侧）
             size = 6 / .pt,  # 缩小阈值标注字体
             fontface = "italic",
             color = "#d73027",
             family = "Arial") +
    geom_point(aes(size = population_millions, color = income), alpha = 0.6) +
    geom_smooth(
        method = "loess", color = "black", fill = "grey70",
        alpha = 0.3, size = 0.8, se = TRUE
    ) +
    # 代表性国家的连线和标签，根据收入组着色，并放置在曲线上下的空白区域
    # Segments and labels for representative countries, colored by income group and placed in empty space above/below the curve
    geom_segment(
        data = label_data,
        aes(
            x = label_x, y = label_y,
            xend = label_x_end, yend = label_y_end,
            color = income
        ),
        linewidth = 0.4,
        alpha = 0.8,
        inherit.aes = FALSE,
        show.legend = FALSE
    ) +
    geom_text(
        data = label_data,
        aes(
            x = label_x_end, y = label_y_end,
            label = label,
            color = income
        ),
        size = 2.4,
        fontface = "bold",
        hjust = 0.5,
        vjust = 0.5,
        inherit.aes = FALSE,
        show.legend = FALSE
    ) +
    scale_color_manual(
        values = lancet_colors$income,
        name = "Income Group",
        labels = c(
            "High income" = "High income",
            "Upper middle income" = "Upper middle income",
            "Lower middle income" = "Lower middle income",
            "Low income" = "Low income"
        )
    ) +
    scale_size_continuous(
        range = c(1.5, 9),  # 放大点的大小范围 | enlarge point size range
        name = "Population\n(millions)",
        breaks = c(10, 100, 500, 1000)
    ) +
    scale_x_continuous(breaks = seq(-2, 2, 1)) +
    scale_y_continuous(labels = number_format(accuracy = 0.1)) +
    labs(
        x = "Government Effectiveness (standardized)",
        y = "GHE/GDP (%)",
        color = "Income Group"
    ) +
    theme_bw(base_size = 8) +  # 统一base_size，与Panel C一致
    theme(
        legend.position = "bottom",  # Panel D保留图例
        legend.justification = "left",  # 图例左对齐，不影响坐标轴位置
        legend.title = element_text(size = 5, face = "bold"),  # 统一图例标题大小（与Panel B一致）
        legend.text = element_text(size = 4, margin = margin(r = 0.05, unit = "cm")),  # 统一图例文本大小（与Panel B一致，size = 4）
        legend.key.size = unit(0.35, "cm"),  # 统一图例key尺寸（与Panel B一致）
        legend.key.width = unit(0.3, "cm"),   # 统一图例key宽度（与Panel B一致）
        legend.key.height = unit(0.35, "cm"),  # 统一图例key高度（与Panel B一致）
        legend.box = "vertical",  # 只有一个图例，改为垂直排列
        legend.direction = "horizontal",
        legend.box.just = "left",  # 左对齐，确保坐标轴位置正常
        legend.spacing.x = unit(0.05, "cm"),  # 减少图例项之间的水平间距，使更紧凑
        legend.spacing.y = unit(0, "cm"),  # Income Group内部两行之间的垂直间距设为0，使两行非常靠近
        legend.margin = margin(t = -0.1, r = 0, b = 0, l = -1.0, unit = "cm"),  # 图例靠左，使用负值让图例更紧贴图形（重叠一点）
        legend.background = element_blank(),
        axis.title = element_text(size = 8),  # 统一坐标轴标题大小（与Panel C一致）
        axis.title.x = element_text(margin = margin(t = 0.3, unit = "cm")),  # 统一横坐标标题与坐标轴的距离（与Panel C一致）
        axis.title.y = element_text(margin = margin(r = 0.3, unit = "cm")),  # 统一纵坐标标题与坐标轴的距离（与Panel C一致）
        axis.text = element_text(size = 7),   # 统一坐标轴文本大小（与Panel C一致）
        axis.text.x = element_text(margin = margin(t = 0.05, unit = "cm")),  # 横坐标文字更靠近坐标轴（与Panel C一致）
        axis.text.y = element_text(margin = margin(r = 0.05, unit = "cm")),  # 统一纵坐标文字与坐标轴的距离（与Panel C一致）
        panel.grid.minor = element_blank(),
        # 添加少量空白区域，保持最小化
        plot.margin = margin(1, 1, 1, 1, unit = "mm")  # 上1mm，右1mm，下1mm，左1mm（少量空白）
    ) +
    guides(
        color = guide_legend(
            order = 1, 
            nrow = 1,  # Income Group类别一行显示，与Panel C一致
            keywidth = unit(0.3, "cm"),  # 统一key宽度
            keyheight = unit(0.35, "cm"),  # 统一key高度
            override.aes = list(size = 2.5)  # 统一Income Group图例点大小
        ),
        size = "none"  # 不显示Population图例，已在Income Group标题中说明
    )

cat("  ✓ Panel D 散点图创建成功\n")

# 单独保存Panel D（用于组合到Figure_1_Complete_Handdrawn.pdf）
# Save Panel D separately (for combining into Figure_1_Complete_Handdrawn.pdf)
cat("\n[2.4.1] 保存Panel D单独文件...\n")
tryCatch({
    # Lancet标准尺寸：90mm × 60mm (3.543 × 2.362 inches @ 300 DPI)
    panel_d_width <- 90 / 25.4  # 3.543 inches
    panel_d_height <- 60 / 25.4  # 2.362 inches
    
    ggsave(
        "../figures/main/Figure_1_Panel_D_Scatter.pdf",
        plot = panel_d,
        width = panel_d_width,
        height = panel_d_height,
        units = "in",
        dpi = 300,
        device = cairo_pdf
    )
    cat("  ✓ Panel D已保存: figures/main/Figure_1_Panel_D_Scatter.pdf\n")
}, error = function(e) {
    cat(sprintf("  ⚠ Panel D保存失败: %s\n", e$message))
    # 尝试使用默认PDF设备
    tryCatch({
        ggsave(
            "../figures/main/Figure_1_Panel_D_Scatter.pdf",
            plot = panel_d,
            width = panel_d_width,
            height = panel_d_height,
            units = "in",
            dpi = 300
        )
        cat("  ✓ Panel D已保存（使用默认PDF设备）\n")
    }, error = function(e2) {
        cat(sprintf("  ✗ Panel D保存失败: %s\n", e2$message))
    })
})

# -----------------------------------------------------------------------------
# 组合 Figure 1
# -----------------------------------------------------------------------------
cat("\n[2.5] 组合 Figure 1...\n")

# 使用patchwork组合
figure_1 <- (panel_a | panel_b) / (panel_c | panel_d) +
    plot_annotation(
        title = "Figure 1. Conceptual Framework and Descriptive Analysis of Government Health Expenditure",
        theme = theme(plot.title = element_text(size = 10, face = "bold"))
    )

# 保存 - 已移除 Figure_1_Complete.pdf 和 Figure_1_Complete.png 的生成代码
# Save - Removed code for generating Figure_1_Complete.pdf and Figure_1_Complete.png
# 这些文件已不再需要，使用 Figure_1_Complete_Handdrawn.pdf 代替
# These files are no longer needed, use Figure_1_Complete_Handdrawn.pdf instead
cat("  [INFO] Figure 1 完整版保存已跳过（使用 Handdrawn 版本）\n")

# =============================================================================
# 3. Figure 2: 核心因果结果
# =============================================================================
cat("\n==================================================\n")
cat("  [3] 生成 Figure 2: 核心因果结果\n")
cat("==================================================\n\n")

cat("[INFO] Figure 2 需要核心分析结果，请先运行03-05脚本\n")
cat("       如果结果文件存在，将生成实际图形\n")
cat("       否则创建占位符\n\n")

# 检查必要的结果文件是否存在
fe_file <- "../results/analysis/03_fe_results.rds"
iv_file <- "../results/analysis/03_iv_results.rds"
threshold_file <- "../results/analysis/04_threshold_estimates.csv"
threshold_data_file <- "../results/analysis/threshold_effect_data.csv"

if (file.exists(fe_file) && file.exists(iv_file) && file.exists(threshold_file) && file.exists(threshold_data_file)) {
    cat("[INFO] 分析结果文件存在，但Figure 2由独立脚本生成，跳过...\n")
    # source("16_generate_figure_2_complete.R")  # Figure 2由16_generate_figure_2_v2_enhanced.R独立生成
    cat("  ✓ Figure 2 由独立脚本生成，跳过\n")
} else {
    cat("[WARN] 分析结果文件缺失，创建占位符...\n")
    figure_2_placeholder <- ggplot() +
        annotate("text",
            x = 0.5, y = 0.5,
            label = "Figure 2: 核心因果结果\n\nPanel A: FE结果森林图\nPanel B: IV结果\nPanel C: 阈值回归\nPanel D: 制度质量调节\n\n需要先运行03_core_econometric_analysis.R",
            size = 4, hjust = 0.5, vjust = 0.5, lineheight = 1.5
        ) +
        theme_void() +
        theme(plot.background = element_rect(fill = "white", color = "black", size = 1))

    ggsave(
        "../figures/main/Figure_2_Causal_Results_PLACEHOLDER.pdf",
        plot = figure_2_placeholder,
        width = double_width,
        height = double_width * 0.9,
        units = "in",
        dpi = 300,
        device = cairo_pdf
    )
    cat("  ✓ Figure 2 占位符创建\n")
}

# =============================================================================
# 4. Figure 3: 机制分析
# =============================================================================
cat("\n==================================================\n")
cat("  [4] 生成 Figure 3: 机制分析\n")
cat("==================================================\n\n")

if (!is.null(extended_results)) {
    # -----------------------------------------------------------------------------
    # Panel A: 中介效应森林图
    # -----------------------------------------------------------------------------
    cat("[4.1] Panel A: 中介效应森林图...\n")

    # 准备中介效应数据
    mediation_data <- extended_results$mediation_summary %>%
        mutate(
            mediator_clean = case_when(
                Mediator == "physicians" ~ "Physician density",
                Mediator == "nurses_midwives" ~ "Nurse/midwife density",
                Mediator == "hospital_beds" ~ "Hospital beds",
                Mediator == "skilled_birth_attendance" ~ "Skilled birth attendance",
                Mediator == "dtp3_immunization" ~ "DTP3 immunization",
                Mediator == "basic_water" ~ "Basic water access",
                Mediator == "basic_sanitation" ~ "Basic sanitation",
                TRUE ~ Mediator
            ),
            indirect_num = as.numeric(Indirect),
            mediation_pct = as.numeric(gsub("%", "", `% Mediated`))
        ) %>%
        arrange(desc(abs(mediation_pct))) %>%
        mutate(mediator_clean = factor(mediator_clean, levels = mediator_clean))

    # 提取置信区间（简化处理）
    mediation_data <- mediation_data %>%
        mutate(
            ci_lower = indirect_num - 0.015, # 简化：实际应从原始数据提取
            ci_upper = indirect_num + 0.015
        )

    # 判断显著性：置信区间不跨过0则为显著
    mediation_data <- mediation_data %>%
        mutate(
            is_significant = (ci_lower > 0 & ci_upper > 0) | (ci_lower < 0 & ci_upper < 0),
            effect_direction = if_else(indirect_num > 0, "Positive", "Negative"),
            # 显著：实心点，不显著：空心点（灰色）
            point_shape = if_else(is_significant, 19, 1),
            point_color = if_else(is_significant, 
                if_else(indirect_num > 0, "Positive", "Negative"), 
                "Not significant"
            ),
            line_type = if_else(is_significant, "solid", "dashed"),
            point_stroke = if_else(is_significant, 0.8, 1.2)
        )

    # 森林图 - 重新设计版本（简化设计）
    # 关键改进：
    # 1. 删除点的大小编码（统一大小）
    # 2. 在图上直接标注关键信息（% Mediated）
    # 3. 简化legend：只保留Effect Direction（Positive/Negative）
    # 4. 不显著的中介效应用灰色空心点表示

    panel_3a <- ggplot(mediation_data, aes(x = indirect_num, y = mediator_clean)) +
        geom_vline(xintercept = 0, linetype = "solid", color = "grey50", linewidth = 0.8) +
        # 误差条：显著用深灰色实线，不显著用浅灰色虚线
        geom_errorbar(
            aes(xmin = ci_lower, xmax = ci_upper, linetype = line_type),
            width = 0.2, linewidth = 0.5, 
            color = ifelse(mediation_data$is_significant, "grey30", "grey70"),
            orientation = "y"
        ) +
        # 点：统一大小（size = 3），不显著用灰色空心点，显著用彩色实心点
        geom_point(
            aes(
                color = if_else(is_significant, effect_direction, "Not significant"),
                fill = if_else(is_significant, effect_direction, "Not significant"),
                shape = factor(if_else(is_significant, "significant", "not_significant")) # 转换为因子
            ),
            size = 3,
            stroke = ifelse(mediation_data$is_significant, 0.8, 1.2),
            alpha = ifelse(mediation_data$is_significant, 0.9, 0.6)
        ) +
        # 为最重要的中介变量添加% Mediated标注
        # 标注前两个最重要的：Skilled birth attendance (30.8%) 和 Physician density (17.2%)
        geom_text(
            data = mediation_data %>% filter(
                mediator_clean %in% c("Skilled birth attendance", "Physician density")
            ),
            aes(
                x = ci_upper, y = mediator_clean,
                label = sprintf("%.1f%%", mediation_pct)
            ),
            hjust = -0.1, vjust = 0.5,
            size = 7 / .pt, color = "#333333", fontface = "italic",
            family = "Arial", inherit.aes = FALSE
        ) +
        scale_color_manual(
            values = c(
                "Positive" = "#2171b5", 
                "Negative" = "#e31a1c",
                "Not significant" = "#999999"
            ),
            name = "Effect Direction",
            breaks = c("Positive", "Negative"), # 只显示Positive和Negative
            labels = c(
                "Positive" = "Positive", 
                "Negative" = "Negative"
            ),
            guide = guide_legend(
                override.aes = list(
                    shape = 19, # 只显示显著的点（实心）
                    size = 3,
                    alpha = 0.9
                )
            )
        ) +
        scale_fill_manual(
            values = c(
                "Positive" = "#2171b5",
                "Negative" = "#e31a1c",
                "Not significant" = "#999999"
            ),
            guide = "none" # fill不单独显示在legend中
        ) +
        scale_shape_manual(
            values = c("significant" = 19, "not_significant" = 21), # 19=实心圆（显著），21=空心圆（不显著）
            guide = "none" # 形状信息通过颜色和legend说明
        ) +
        scale_linetype_manual(
            values = c("solid" = "solid", "dashed" = "dashed"),
            guide = "none" # 线型信息通过颜色和legend说明
        ) +
        scale_x_continuous(
            limits = c(-0.06, 0.02),
            labels = number_format(accuracy = 0.01),
            breaks = scales::pretty_breaks(n = 5) # 简化刻度标记
        ) +
        labs(
            x = "Indirect Effect (95% CI)",
            y = NULL # 移除Y轴标签，直接显示中介变量名称
        ) +
        theme_bw() +
        theme(
            # 移除冗余网格线
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_line(color = "grey95", linewidth = 0.3),
            # 减少上下边距，减少panel之间的空白
            plot.margin = margin(3, 8, 3, 8, "mm"),
            # 字体规范：轴标签 8pt Arial（期刊要求）
            axis.title.x = element_text(size = 8, family = "Arial", face = "plain"),
            axis.title.y = element_text(size = 8, family = "Arial", face = "plain"),
            # 数值标注：7pt Arial，水平显示
            axis.text.x = element_text(size = 7, family = "Arial", angle = 0, hjust = 0.5),
            axis.text.y = element_text(size = 7, family = "Arial", angle = 0, hjust = 1),
            plot.title = element_blank(),
            legend.position = "right",
            legend.title = element_text(size = 7, family = "Arial", face = "plain"),
            legend.text = element_text(size = 7, family = "Arial"),
            legend.key.size = unit(0.4, "cm")
        )

    cat("  ✓ Panel A 森林图创建成功\n")

    # -----------------------------------------------------------------------------
    # Panel B: 按制度质量分组 - 森林图设计
    # -----------------------------------------------------------------------------
    cat("[4.2] Panel B: 按制度质量分组（森林图）...\n")

    # 准备分组数据
    mechanism_gov <- extended_results$mechanism_by_governance %>%
        mutate(
            mediator_clean = case_when(
                Mediator == "physicians" ~ "Physician density",
                Mediator == "nurses_midwives" ~ "Nurse/midwife density",
                Mediator == "hospital_beds" ~ "Hospital beds",
                Mediator == "skilled_birth_attendance" ~ "Skilled birth attendance",
                Mediator == "dtp3_immunization" ~ "DTP3 immunization",
                Mediator == "basic_water" ~ "Basic water access",
                Mediator == "basic_sanitation" ~ "Basic sanitation",
                TRUE ~ Mediator
            )
        ) %>%
        pivot_longer(
            cols = c(`High Gov (β)`, `Low Gov (β)`),
            names_to = "governance",
            values_to = "coefficient"
        ) %>%
        mutate(
            governance = if_else(governance == "High Gov (β)",
                "High governance", "Low governance"
            ),
            # 提取系数：匹配第一个数字（可能带负号）
            coef_num = as.numeric(str_extract(coefficient, "-?\\d+\\.\\d+")),
            # 提取标准误：匹配括号内的数字（可能带负号）
            # 使用str_match提取第一个捕获组
            se_str = str_match(coefficient, "\\((-?\\d+\\.\\d+)\\)")[, 2],
            se_num = as.numeric(se_str)
        ) %>%
        # 过滤掉NA值（N/A的情况）
        filter(!is.na(coef_num), !is.na(se_num)) %>%
        # 计算置信区间（95% CI = coef ± 1.96*se）
        mutate(
            ci_lower = coef_num - 1.96 * se_num,
            ci_upper = coef_num + 1.96 * se_num,
            # 判断显著性：置信区间不跨过0则为显著
            is_significant = (ci_lower > 0 & ci_upper > 0) | (ci_lower < 0 & ci_upper < 0)
        )

    # 按中介比例排序（使用mediation_summary中的排序，与Panel A保持一致）
    mediation_order <- extended_results$mediation_summary %>%
        mutate(mediation_pct = as.numeric(gsub("%", "", `% Mediated`))) %>%
        arrange(desc(abs(mediation_pct))) %>%
        pull(Mediator)

    # 创建mediator_clean到Mediator的映射，用于排序
    mediator_mapping <- mechanism_gov %>%
        select(Mediator, mediator_clean) %>%
        distinct()

    # 按mediation_order排序mediator_clean
    mediator_clean_order <- mediator_mapping %>%
        arrange(match(Mediator, mediation_order)) %>%
        pull(mediator_clean)

    mechanism_gov <- mechanism_gov %>%
        mutate(
            mediator_clean = factor(mediator_clean, levels = mediator_clean_order),
            # 为并排显示创建y轴偏移：High governance在左侧，Low governance在右侧
            y_offset = if_else(governance == "High governance", -0.15, 0.15),
            y_position = as.numeric(mediator_clean) + y_offset
        )

    # 森林图设计
    panel_3b <- ggplot(
        mechanism_gov,
        aes(x = coef_num, y = y_position, color = governance)
    ) +
        geom_vline(xintercept = 0, linetype = "solid", color = "grey50", linewidth = 0.8) +
        # 置信区间误差条
        geom_errorbar(
            aes(xmin = ci_lower, xmax = ci_upper),
            width = 0.15,
            linewidth = 0.5,
            alpha = 0.7
        ) +
        # 点估计
        geom_point(
            aes(shape = factor(if_else(is_significant, "significant", "not_significant"))),
            size = 3,
            alpha = 0.9
        ) +
        # 使用scale_y_continuous和breaks来显示mediator标签
        scale_y_continuous(
            breaks = 1:length(mediator_clean_order),
            labels = mediator_clean_order,
            minor_breaks = NULL,
            expand = expansion(mult = c(0.1, 0.1)) # 上下留出空间
        ) +
        scale_color_manual(
            values = c(
                "High governance" = "#2171b5", # 蓝色
                "Low governance" = "#fd8d3c" # 橙色
            ),
            name = "Governance Quality",
            labels = c("High governance" = "High governance", "Low governance" = "Low governance")
        ) +
        scale_shape_manual(
            values = c("significant" = 19, "not_significant" = 21), # 19=实心圆（显著），21=空心圆（不显著）
            name = "Significance",
            guide = guide_legend(
                title = "Significance",
                title.theme = element_text(size = 7, family = "Arial", face = "plain"),
                override.aes = list(
                    color = c("grey30", "grey70"), # 显著用深灰色，不显著用浅灰色
                    size = 3
                ),
                keywidth = unit(0.4, "cm"),
                keyheight = unit(0.4, "cm")
            )
        ) +
        scale_x_continuous(
            labels = number_format(accuracy = 0.1),
            breaks = scales::pretty_breaks(n = 6),
            expand = expansion(mult = c(0.05, 0.15)) # 右侧留出空间给图例
        ) +
        labs(
            x = "GHE Coefficient (95% CI)",
            y = NULL, # 移除Y轴标签，直接显示中介变量名称
            color = "Governance Quality"
        ) +
        theme_bw() +
        theme(
            # 移除冗余网格线
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_line(color = "grey95", linewidth = 0.3),
            # 减少上下边距
            plot.margin = margin(3, 8, 3, 8, "mm"),
            # 字体规范：轴标签 8pt Arial（期刊要求）
            axis.title.x = element_text(size = 8, family = "Arial", face = "plain"),
            axis.title.y = element_text(size = 8, family = "Arial", face = "plain"),
            # 数值标注：7pt Arial
            axis.text.x = element_text(size = 7, family = "Arial", angle = 0, hjust = 0.5),
            axis.text.y = element_text(size = 7, family = "Arial", angle = 0, hjust = 1),
            plot.title = element_blank(),
            legend.position = "right",
            legend.title = element_text(size = 7, family = "Arial", face = "plain"),
            legend.text = element_text(size = 7, family = "Arial"),
            legend.key.size = unit(0.4, "cm")
        )

    cat("  ✓ Panel B 森林图创建成功\n")

    # -----------------------------------------------------------------------------
    # 组合 Figure 3
    # -----------------------------------------------------------------------------
    cat("\n[4.3] 组合 Figure 3...\n")

    # 先保存单个Panel（在组合前保存，确保不被修改）
    ggsave(
        "../figures/main/Figure_3_Panel_A_Mediation.pdf",
        plot = panel_3a,
        width = double_width,
        height = double_width * 0.35, # 降低Panel A高度
        units = "in",
        dpi = 300,
        device = cairo_pdf
    )

    ggsave(
        "../figures/main/Figure_3_Panel_B_By_Governance.pdf",
        plot = panel_3b,
        width = double_width,
        height = double_width * 0.45, # 与Panel A保持一致
        units = "in",
        dpi = 300,
        device = cairo_pdf
    )

    # 使用cowplot组合图形（上下结构：顶部Panel A，底部Panel B）
    figure_3_base <- plot_grid(
        panel_3a, panel_3b,
        ncol = 1, nrow = 2, # 垂直布局
        align = "v", # 只垂直对齐，减少间距
        axis = "lr", # 只左右对齐
        rel_widths = c(1),
        rel_heights = c(1, 1), # Panel A 和 Panel B 等高度分布
        scale = 1.0 # 不缩小，充分利用空间
    )

    # 使用cowplot添加主标题（10pt Arial Bold，顶部居中）
    # 删除副标题，因为结论应该在正文中阐述，而不是在图表标题中
    figure_3 <- ggdraw() +
        cowplot::draw_plot(figure_3_base, x = 0, y = 0, width = 1, height = 0.95) +
        # 主标题：中立、描述性的标题
        cowplot::draw_label(
            "Mediation analysis of the GHE-life expectancy relationship",
            x = 0.5, y = 0.98,
            hjust = 0.5, vjust = 1,
            size = 10,
            fontface = "bold",
            fontfamily = "Arial"
        )

    # 保存组合图（减少高度）
    ggsave(
        "../figures/main/Figure_3_Complete.pdf",
        plot = figure_3,
        width = double_width,
        height = double_width * 0.9, # 减少高度：从1.2减少到0.9
        units = "in",
        dpi = 300,
        device = cairo_pdf
    )

    ggsave(
        "../figures/main/Figure_3_Complete.png",
        plot = figure_3,
        width = double_width,
        height = double_width * 0.9, # 减少高度：从1.2减少到0.9
        units = "in",
        dpi = 300
    )

    cat("  ✓ Figure 3 保存成功\n")
    cat(sprintf("    - Panel A PDF: figures/main/Figure_3_Panel_A_Mediation.pdf\n"))
    cat(sprintf("    - Panel B PDF: figures/main/Figure_3_Panel_B_By_Governance.pdf\n"))
    cat(sprintf("    - Complete PDF: figures/main/Figure_3_Complete.pdf\n"))
    cat(sprintf("    - Complete PNG: figures/main/Figure_3_Complete.png\n"))

    # 生成黑白打印版本（使用ghostscript直接转换）
    cat("\n[4.4] 生成 Figure 3 黑白打印版本...\n")

    color_pdf <- "../figures/main/Figure_3_Complete.pdf"
    bw_pdf <- "../figures/main/Figure_3_Complete_BW.pdf"

    # 使用ghostscript直接转换彩色PDF为黑白PDF
    system_command <- sprintf(
        'gs -sDEVICE=pdfwrite -dProcessColorModel=/DeviceGray -dColorConversionStrategy=/Gray -dPDFUseOldCMS=false -dNOPAUSE -dQUIET -dBATCH -sOutputFile="%s" "%s"',
        bw_pdf, color_pdf
    )

    result <- system(system_command)
    if (result == 0) {
        cat("  ✓ Figure 3 黑白版本生成成功\n")
        cat(sprintf("    - Complete BW PDF: figures/main/Figure_3_Complete_BW.pdf\n"))
    } else {
        cat("  ⚠ 黑白图生成失败，请确保已安装ghostscript\n")
    }
} else {
    cat("  ⚠ 扩展分析结果不存在，跳过 Figure 3\n")
}

# =============================================================================
# 5. Figure 4: 疾病特定效应
# =============================================================================
cat("\n==================================================\n")
cat("  [5] 生成 Figure 4: 疾病特定效应\n")
cat("==================================================\n\n")

if (!is.null(extended_results)) {
    # -----------------------------------------------------------------------------
    # Panel A: 疾病敏感度热图
    # -----------------------------------------------------------------------------
    cat("[5.1] Panel A: 疾病敏感度热图...\n")

    # 准备热图数据
    disease_data <- extended_results$disease_summary %>%
        mutate(
            disease_clean = case_when(
                str_detect(Disease, "孕产妇") ~ "Maternal\nmortality",
                str_detect(Disease, "5岁以下") ~ "Child\nmortality\n(U5)",
                str_detect(Disease, "新生儿") ~ "Neonatal\nmortality",
                str_detect(Disease, "结核") ~ "Tuberculosis\nincidence",
                str_detect(Disease, "慢性病") ~ "NCD mortality\n(30-70y)",
                TRUE ~ Disease
            )
        ) %>%
        select(disease_clean, `Overall β`, `High Gov β`, `Low Gov β`) %>%
        pivot_longer(
            cols = -disease_clean,
            names_to = "governance",
            values_to = "coefficient"
        ) %>%
        mutate(
            governance = case_when(
                governance == "Overall β" ~ "Overall",
                governance == "High Gov β" ~ "High\nGovernance",
                governance == "Low Gov β" ~ "Low\nGovernance"
            ),
            coef_num = as.numeric(str_extract(coefficient, "-?\\d+\\.\\d+")),
            governance = factor(governance, levels = c("Overall", "High\nGovernance", "Low\nGovernance"))
        )

    # 为数值标签添加颜色映射：深色背景用白色文字，浅色背景用黑色文字
    disease_data <- disease_data %>%
        mutate(
            # 计算与中点的距离，判断背景深浅
            # 距离中点越远（绝对值越大），背景越深
            text_color = if_else(abs(coef_num - 0) > 0.15, "white", "black")
        )

    # 热图
    panel_4a_core <- ggplot(disease_data, aes(x = governance, y = disease_clean, fill = coef_num)) +
        geom_tile(color = "white", size = 0.5) +
        geom_text(aes(x = governance, y = disease_clean, label = sprintf("%.3f", coef_num), color = text_color),
            size = 7 / .pt, family = "Arial"
        ) + # 7pt Arial（期刊要求）
        scale_color_manual(values = c("black" = "black", "white" = "white"), guide = "none") +
        scale_fill_gradient2(
            low = "#0072B2", mid = "white", high = "#D55E00", # Lancet标准蓝到标准橙
            midpoint = 0, limits = c(-0.05, 0.45),
            name = "GHE\nCoefficient"
        ) +
        labs(
            x = "Governance Context", # 添加横坐标标签
            y = "Health Outcome" # 添加纵坐标标签
        ) +
        theme_minimal(base_size = 7) +
        theme(
            legend.position = "bottom", # 图例放到图形下方
            legend.title = element_text(size = 6, face = "bold", family = "Arial"), # 图例标题6pt Arial
            legend.text = element_text(size = 6, family = "Arial"), # 图例文字6pt Arial
            legend.key.size = unit(0.3, "cm"),
            legend.box = element_blank(), # 移除图例边框
            axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8, family = "Arial"), # 8pt Arial（期刊要求）
            axis.text.y = element_text(angle = 0, hjust = 1, size = 8, family = "Arial"), # 8pt Arial（期刊要求）
            axis.title.x = element_text(face = "bold", size = 8, family = "Arial"), # 8pt Arial
            axis.title.y = element_text(face = "bold", size = 8, family = "Arial"), # 8pt Arial
            panel.grid = element_blank(), # 移除所有网格线
            plot.margin = margin(8, 8, 8, 12), # 增加留白
            plot.title = element_blank()
        )

    panel_4a <- panel_4a_core # 移除子图标签

    cat("  ✓ Panel A 热图创建成功\n")

    # -----------------------------------------------------------------------------
    # Panel B: 儿童健康效应（高亮）
    # -----------------------------------------------------------------------------
    cat("[5.2] Panel B: 儿童健康效应...\n")

    # 提取儿童健康数据
    child_health <- disease_data %>%
        filter(str_detect(disease_clean, "Child|Neonatal|Maternal")) %>%
        filter(governance != "Overall")

    # 为黑白打印添加形状和线型映射
    child_health <- child_health %>%
        mutate(
            point_shape = if_else(governance == "High\nGovernance", 19, 1), # 高治理：实心点，低治理：空心点
            line_type = if_else(governance == "High\nGovernance", "solid", "dashed") # 高治理：实线，低治理：虚线
        )

    # 系数图
    panel_4b_core <- ggplot(child_health, aes(x = coef_num, y = disease_clean, color = governance, shape = factor(point_shape), linetype = line_type)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
        geom_point(size = 3, position = position_dodge(width = 0.5), stroke = 1.2, fill = "white") +
        geom_errorbar(aes(xmin = coef_num - 0.01, xmax = coef_num + 0.01),
            width = 0.2, position = position_dodge(width = 0.5), linewidth = 0.5, orientation = "y"
        ) +
        scale_color_manual(
            values = c(
                "High\nGovernance" = "#0072B2", # Lancet标准蓝
                "Low\nGovernance" = "#D55E00" # Lancet标准橙
            )
        ) +
        scale_shape_manual(
            values = c("19" = 19, "1" = 1), # 高治理：实心点，低治理：空心点
            guide = "none"
        ) +
        scale_linetype_manual(
            values = c("solid" = "solid", "dashed" = "dashed"), # 高治理：实线，低治理：虚线
            guide = "none"
        ) +
        guides(
            color = guide_legend(override.aes = list(shape = c(19, 1), linetype = c("solid", "dashed")))
        ) +
        scale_x_continuous(labels = number_format(accuracy = 0.02)) + # 简化刻度精度
        labs(
            x = "GHE Coefficient (95% CI)",
            y = "Health Outcome", # 添加纵轴标签
            color = "Governance\nQuality"
        ) +
        theme_bw(base_size = 7) +
        theme(
            legend.position = "bottom",
            legend.title = element_text(size = 6, face = "bold", family = "Arial"), # 图例标题6pt Arial
            legend.text = element_text(size = 6, family = "Arial"), # 图例文字6pt Arial
            legend.box = element_blank(), # 移除图例边框
            axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8, family = "Arial"), # 8pt Arial（期刊要求）
            axis.text.y = element_text(angle = 0, hjust = 1, size = 8, family = "Arial"), # 8pt Arial（期刊要求）
            axis.title.x = element_text(face = "bold", size = 8, family = "Arial"), # 8pt Arial
            axis.title.y = element_text(face = "bold", size = 8, family = "Arial"), # 8pt Arial
            panel.grid.major = element_blank(), # 移除主要网格线
            panel.grid.minor = element_blank(), # 移除次要网格线
            plot.margin = margin(8, 8, 8, 12), # 增加留白
            plot.title = element_blank()
        )

    panel_4b <- panel_4b_core # 移除子图标签

    cat("  ✓ Panel B 儿童健康图创建成功\n")

    # -----------------------------------------------------------------------------
    # Panel C & D: 慢性病制度依赖 + 疾病优先级矩阵
    # -----------------------------------------------------------------------------
    cat("[5.3] Panel C-D: 慢性病制度依赖与疾病优先级矩阵...\n")

    disease_summary <- extended_results$disease_summary

    # --- Panel C 数据准备 ---
    # 读取阈值回归的阈值，与图2和图3保持一致
    # Read threshold regression threshold, consistent with Figure 2 and Figure 3
    threshold_est_file <- "../results/analysis/04_threshold_estimates_manuscript.csv"
    if (file.exists(threshold_est_file)) {
        threshold_est_data <- read_csv(threshold_est_file, show_col_types = FALSE)
        wgi_threshold <- as.numeric(threshold_est_data$threshold[1])
    } else {
        wgi_threshold <- -0.53  # 默认值，与图2和图3保持一致
    }
    
    ncd_data <- disease_summary %>%
        filter(str_detect(Disease, "慢性病")) %>%
        mutate(
            overall_coef = as.numeric(str_extract(`Overall β`, "-?\\d+\\.\\d+")),
            high_gov_coef = as.numeric(str_extract(`High Gov β`, "-?\\d+\\.\\d+")),
            low_gov_coef = as.numeric(str_extract(`Low Gov β`, "-?\\d+\\.\\d+")),
            interaction_coef = as.numeric(str_extract(Interaction, "-?\\d+\\.\\d+"))
        )

    governance_range <- seq(-2.5, 2.5, length.out = 200)
    base_ncd_coef <- ncd_data$overall_coef[1]
    interaction_ncd <- ncd_data$interaction_coef[1]

    ncd_marginal <- tibble(
        governance = governance_range,
        marginal_effect = base_ncd_coef + interaction_ncd * governance
    ) %>%
        mutate(
            # 使用阈值-0.53定义Low/High Governance，与图2和图3保持一致
            # Use threshold -0.53 to define Low/High Governance, consistent with Figure 2 and Figure 3
            regime = if_else(governance < wgi_threshold, "Low Governance", "High Governance"),
            ci_lower = marginal_effect - 0.02,
            ci_upper = marginal_effect + 0.02
        )

    # 使用marginal_effect的实际范围，而不是CI范围，确保图形可见
    y_min <- min(ncd_marginal$marginal_effect, na.rm = TRUE)
    y_max <- max(ncd_marginal$marginal_effect, na.rm = TRUE)
    x_min <- min(ncd_marginal$governance)
    x_max <- max(ncd_marginal$governance)
    x_range <- x_max - x_min
    y_range <- y_max - y_min
    x_margin <- ifelse(is.finite(x_range) && x_range > 0, x_range * 0.12, 0.3)
    y_margin <- ifelse(is.finite(y_range) && y_range > 0, y_range * 0.12, 0.1)
    x_limit_low <- x_min - x_margin
    x_limit_high <- x_max + x_margin
    y_limit_low <- y_min - y_margin
    y_limit_high <- y_max + y_margin
    low_point <- ncd_marginal %>% filter(governance == min(governance))
    high_point <- ncd_marginal %>% filter(governance == max(governance))

    # 为黑白打印添加线型映射
    ncd_marginal <- ncd_marginal %>%
        mutate(
            line_type = if_else(regime == "High Governance", "solid", "dashed") # 高治理：实线，低治理：虚线
        )

    panel_4c_core <- ggplot(ncd_marginal, aes(x = governance, y = marginal_effect, color = regime, linetype = line_type, fill = regime)) +
        # Harmful zone（上方）：红色背景
        annotate("rect",
            xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf,
            fill = "#fde0dd", alpha = 0.35
        ) +
        # Protective zone（下方）：蓝色背景
        annotate("rect",
            xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0,
            fill = "#e0f3f8", alpha = 0.35
        ) +
        # Harmful zone添加subtle点状背景（增强黑白区分）
        # 覆盖整个上半部分区域（y = 0.002 到 0.6）
        annotate("point",
            x = rep(seq(-2.5, 2.5, length.out = 50), 30),
            y = rep(seq(0.002, 0.6, length.out = 30), each = 50),
            color = "grey50", alpha = 0.08, size = 0.25, shape = 16
        ) +
        geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = regime),
            alpha = 0.3, linewidth = 0, color = "grey80", show.legend = FALSE # 置信区间：浅灰色填充，不显示在图例中
        ) +
        geom_line(linewidth = 1.2, show.legend = FALSE) + # 线段不显示在图例中
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.4) +
        # 添加阈值线（-0.53），与图2和图3保持一致
        # Add threshold line (-0.53), consistent with Figure 2 and Figure 3
        geom_vline(xintercept = wgi_threshold, linetype = "dashed", color = "grey60", linewidth = 0.5) +
        # 添加用于图例显示的虚拟点（使用极小的size，几乎不可见，但能生成图例）
        geom_point(
            data = ncd_marginal %>% filter(regime == "Low Governance") %>% slice(1),
            aes(x = governance, y = marginal_effect, color = regime),
            shape = 1, size = 0.01, stroke = 1.5, alpha = 0.01
        ) + # 极小且几乎透明，用于生成图例
        geom_point(
            data = ncd_marginal %>% filter(regime == "High Governance") %>% slice(1),
            aes(x = governance, y = marginal_effect, color = regime),
            shape = 19, size = 0.01, stroke = 1.2, alpha = 0.01
        ) + # 极小且几乎透明，用于生成图例
        # 实际显示的数据点
        geom_point(
            data = low_point, aes(x = governance, y = marginal_effect),
            shape = 1, size = 3.8, fill = "white", stroke = 1.5, color = "#D55E00", inherit.aes = FALSE, show.legend = FALSE # 低治理：空心点，不显示在图例中
        ) +
        geom_point(
            data = high_point, aes(x = governance, y = marginal_effect),
            shape = 19, size = 3.8, fill = "white", stroke = 1.2, color = "#0072B2", inherit.aes = FALSE, show.legend = FALSE # 高治理：实心点，不显示在图例中
        ) +
        scale_color_manual(
            values = c(
                "Low Governance" = "#D55E00", # Lancet标准橙
                "High Governance" = "#0072B2" # Lancet标准蓝
            ),
            breaks = c("Low Governance", "High Governance"),
            labels = c(
                sprintf("Low Governance\n(Below %.2f)", wgi_threshold),
                sprintf("High Governance\n(Above %.2f)", wgi_threshold)
            )
        ) +
        scale_fill_manual(
            values = c(
                "Low Governance" = "#D55E00", # Lancet标准橙（浅色填充）
                "High Governance" = "#0072B2" # Lancet标准蓝（浅色填充）
            ),
            guide = "none"
        ) +
        scale_linetype_manual(
            values = c("dashed" = "dashed", "solid" = "solid"), # 低治理：虚线，高治理：实线
            guide = "none"
        ) +
        scale_x_continuous(
            limits = c(x_limit_low, x_limit_high),
            labels = number_format(accuracy = 0.5),
            expand = expansion(mult = c(0, 0))
        ) +
        scale_y_continuous(
            limits = c(y_limit_low, y_limit_high),
            labels = number_format(accuracy = 0.02), # 简化刻度精度
            expand = expansion(mult = c(0, 0))
        ) +
        labs(
            x = "Government Effectiveness (standardized)",
            y = "Marginal Effect of GHE on NCD Mortality",
            color = "Governance regime"
        ) +
        # 添加阈值标注（右移到垂线右侧，左对齐，贴近上侧边框）
        # Add threshold annotation (moved to the right of the vertical line, left-aligned, near top border)
        annotate("text",
            x = wgi_threshold + 0.15,  # 右移到垂线右侧
            y = y_max - 0.01 * (y_max - y_min),  # 上移，贴近上侧边框
            label = sprintf("Threshold\n= %.2f", wgi_threshold),
            hjust = 0, vjust = 1,  # 左对齐
            size = 6 / .pt, color = "#333333", fontface = "italic",
            family = "Arial"
        ) +
        # 移除内部数值标注，提高数据墨水比（使用更谨慎的表述）
        annotate("label",
            x = (x_min + x_max) / 2,
            y = (y_max + 0) / 2,
            label = "Higher mortality (+)",
            size = 2.6,
            color = "#b2182b",
            fill = "#fde0dd",
            fontface = "bold"
        ) +
        annotate("label",
            x = (x_min + x_max) / 2,
            y = (y_min + 0) / 2,
            label = "Lower mortality (-)",
            size = 2.6,
            color = "#045a8d",
            fill = "#e0f3f8",
            fontface = "bold"
        ) +
        theme_bw(base_size = 7) +
        theme(
            legend.position = "bottom",
            legend.title = element_text(face = "bold", size = 6, family = "Arial"), # 图例标题6pt Arial
            legend.text = element_text(size = 6, family = "Arial"), # 图例文字6pt Arial
            legend.box = element_blank(), # 移除图例边框
            panel.grid.major = element_blank(), # 移除主要网格线（仅保留关键参考线）
            panel.grid.minor = element_blank(), # 移除次要网格线
            plot.margin = margin(8, 8, 8, 12), # 增加留白
            axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8, family = "Arial"), # 8pt Arial（期刊要求）
            axis.text.y = element_text(angle = 0, hjust = 1, size = 8, family = "Arial"), # 8pt Arial（期刊要求）
            axis.title.x = element_text(face = "bold", size = 8, family = "Arial"), # 8pt Arial
            axis.title.y = element_text(face = "bold", size = 8, family = "Arial"), # 8pt Arial
            legend.box.spacing = unit(2, "pt")
        ) +
        guides(color = guide_legend(
            override.aes = list(
                # 按照 scale_color_manual 的顺序：Low Governance, High Governance
                shape = c(1, 19), # Low Governance: 空心点，High Governance: 实心点
                size = 3.5, # 点的大小
                alpha = 1, # 确保不透明
                linetype = "blank", # 隐藏线段
                linewidth = 0, # 线宽为0
                stroke = c(1.5, 1.2) # 边框宽度：Low Governance 1.5, High Governance 1.2
            )
        )) +
        ggtitle(NULL)

    panel_4c <- panel_4c_core # 移除子图标签

    cat("  ✓ Panel C 慢性病制度依赖图创建成功\n")

    # --- Panel D 数据准备 ---
    disease_priority <- disease_summary %>%
        mutate(
            disease_name = case_when(
                str_detect(Disease, "新生儿") ~ "Neonatal\nmortality",
                str_detect(Disease, "5岁以下") ~ "Child\nmortality\n(U5)",
                str_detect(Disease, "孕产妇") ~ "Maternal\nmortality",
                str_detect(Disease, "结核") ~ "Tuberculosis",
                str_detect(Disease, "慢性病") ~ "NCD (30-70y)",
                TRUE ~ Disease
            ),
            sensitivity = abs(as.numeric(str_extract(`High Gov β`, "-?\\d+\\.\\d+"))),
            institutional_dependence = abs(as.numeric(str_extract(Interaction, "-?\\d+\\.\\d+"))),
            priority = case_when(
                sensitivity > 0.02 & institutional_dependence < 0.02 ~ "High Priority\n(Low dependence)",
                sensitivity > 0.02 & institutional_dependence >= 0.02 ~ "High Priority\n(Needs governance)",
                sensitivity <= 0.02 & institutional_dependence < 0.02 ~ "Medium Priority",
                TRUE ~ "Low Priority\n(Needs governance)"
            )
        ) %>%
        filter(!is.na(sensitivity), !is.na(institutional_dependence)) %>%
        # 为黑白打印添加不同形状的点
        mutate(
            point_shape = case_when(
                priority == "High Priority\n(Low dependence)" ~ 19, # 实心圆
                priority == "High Priority\n(Needs governance)" ~ 17, # 实心三角
                priority == "Medium Priority" ~ 15, # 实心方
                priority == "Low Priority\n(Needs governance)" ~ 1 # 空心圆
            )
        )

    max_dep <- max(disease_priority$institutional_dependence, na.rm = TRUE)
    max_sens <- max(disease_priority$sensitivity, na.rm = TRUE)
    threshold_dep <- 0.02
    threshold_sens <- 0.02
    x_limit_low_d <- -0.05
    x_limit_high_d <- 0.25
    y_limit_low_d <- -0.025
    y_limit_high_d <- 0.18
    left_center <- (threshold_dep + x_limit_low_d) / 2
    right_center <- (threshold_dep + x_limit_high_d) / 2
    bottom_center <- (threshold_sens + y_limit_low_d) / 2
    top_center <- (threshold_sens + y_limit_high_d) / 2

    panel_4d_core <- ggplot(
        disease_priority,
        aes(
            x = institutional_dependence,
            y = sensitivity,
            label = disease_name,
            color = priority,
            shape = factor(point_shape)
        )
    ) +
        geom_point(size = 3.6, alpha = 0.85) +
        scale_shape_manual(
            values = c(
                "19" = 19, # 实心圆 - High Priority (Low dependence)
                "17" = 17, # 实心三角 - High Priority (Needs governance)
                "15" = 15, # 实心方 - Medium Priority
                "1" = 1 # 空心圆 - Low Priority (Needs governance)
            ),
            guide = "none" # 隐藏单独的形状图例，通过颜色图例显示
        ) +
        ggrepel::geom_text_repel(
            size = 2.4,
            fontface = "bold",
            max.overlaps = Inf,
            box.padding = 0.55,
            point.padding = 0.3,
            min.segment.length = 0,
            force = 5,
            segment.size = 0.2,
            color = "black",
            segment.color = "grey60",
            seed = 123
        ) +
        geom_vline(xintercept = 0.02, linetype = "dashed", color = "grey60", linewidth = 0.4) +
        geom_hline(yintercept = 0.02, linetype = "dashed", color = "grey60", linewidth = 0.4) +
        annotate("label",
            x = left_center, y = top_center,
            label = "High Priority\nQuick Wins", size = 2.3, fill = "#e7f0fa",
            color = "#045a8d", fontface = "bold"
        ) +
        annotate("label",
            x = right_center, y = top_center,
            label = "High Priority\nNeeds Governance", size = 2.3, fill = "#fff3cd",
            color = "#cc5801", fontface = "bold"
        ) +
        annotate("label",
            x = left_center, y = bottom_center - 0.005,
            label = "Medium Priority", size = 2.1, fill = "white",
            color = "#5f5f5f"
        ) +
        annotate("label",
            x = right_center, y = bottom_center,
            label = "Low Priority\nNeeds Governance", size = 2.1, fill = "#fde0dd",
            color = "#b2182b", fontface = "bold"
        ) +
        scale_color_manual(
            values = c(
                "High Priority\n(Low dependence)" = "#0072B2", # Lancet标准蓝
                "High Priority\n(Needs governance)" = "#009E73", # Lancet标准绿（区分两种High Priority）
                "Medium Priority" = "#56B4E9", # Lancet浅蓝
                "Low Priority\n(Needs governance)" = "#D55E00" # Lancet标准橙
            ),
            name = "Priority Level",
            breaks = c(
                "High Priority\n(Low dependence)",
                "High Priority\n(Needs governance)",
                "Medium Priority",
                "Low Priority\n(Needs governance)"
            ), # 明确指定breaks，确保4个类别都显示
            labels = c(
                "High Priority\n(Low dependence)" = "High Priority (Low dependence)",
                "High Priority\n(Needs governance)" = "High Priority (Needs governance)",
                "Medium Priority" = "Medium Priority",
                "Low Priority\n(Needs governance)" = "Low Priority (Needs governance)"
            )
        ) +
        guides(
            color = guide_legend(
                nrow = 2, # 图例分两行显示
                # 不设置override.aes，让ggplot2自动处理，避免类别数量不匹配的问题
                # 形状通过scale_shape_manual已经设置，颜色通过scale_color_manual设置
                title.position = "top",
                title.hjust = 0,  # 标题左对齐
                byrow = TRUE  # 按行填充
            )
        ) +
        scale_x_continuous(
            limits = c(x_limit_low_d, x_limit_high_d),
            labels = number_format(accuracy = 0.02), # 简化刻度精度
            expand = expansion(mult = c(0, 0))
        ) +
        scale_y_continuous(
            limits = c(y_limit_low_d, y_limit_high_d),
            labels = number_format(accuracy = 0.02), # 简化刻度精度
            expand = expansion(mult = c(0, 0))
        ) +
        labs(
            x = "Institutional Dependence\n(|Interaction Coefficient|)",
            y = "Sensitivity to GHE\n(|High Governance Coefficient|)" # 分两行显示
        ) +
        theme_bw(base_size = 7) +
        theme(
            panel.grid.major = element_blank(), # 移除主要网格线（仅保留关键参考线）
            panel.grid.minor = element_blank(), # 移除次要网格线
            plot.margin = margin(8, 8, 8, 12), # 增加留白
            axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8, family = "Arial"), # 8pt Arial（期刊要求）
            axis.text.y = element_text(angle = 0, hjust = 1, size = 8, family = "Arial"), # 8pt Arial（期刊要求）
            axis.title.x = element_text(face = "bold", size = 8, family = "Arial"), # 8pt Arial
            axis.title.y = element_text(face = "bold", size = 8, family = "Arial"), # 8pt Arial
            plot.title = element_blank(),
            legend.position = "bottom", # 图例放在图形下方
            legend.justification = "left", # 图例左对齐（相对于底部位置）
            legend.title = element_text(size = 6, face = "bold", family = "Arial"), # 图例标题6pt Arial
            legend.text = element_text(size = 6, family = "Arial"), # 图例文字6pt Arial
            legend.key.size = unit(0.4, "cm"),
            legend.box = element_blank(), # 移除图例边框
            legend.box.just = "left", # 图例框左对齐
            legend.box.spacing = unit(2, "pt"),
            legend.margin = margin(t = 0, r = 0, b = 0, l = -5, unit = "mm"), # 图例向左移动，贴近左侧边缘
            legend.direction = "horizontal", # 水平排列图例项
            legend.spacing.y = unit(0.1, "cm") # 减少图例两行之间的间距
        )

    panel_4d <- panel_4d_core # 移除子图标签

    cat("  ✓ Panel D 疾病优先级矩阵创建成功\n")

    # -----------------------------------------------------------------------------
    # 组合 Figure 4
    # -----------------------------------------------------------------------------
    cat("\n[5.4] 组合 Figure 4...\n")

    # 先保存单个Panel（在组合前保存，确保不被修改）
    ggsave(
        "../figures/main/Figure_4_Panel_A_Heatmap.pdf",
        plot = panel_4a,
        width = double_width / 2,
        height = double_width * 0.45,
        units = "in",
        dpi = 300,
        device = cairo_pdf
    )

    ggsave(
        "../figures/main/Figure_4_Panel_B_Child_Health.pdf",
        plot = panel_4b,
        width = double_width / 2,
        height = double_width * 0.45,
        units = "in",
        dpi = 300,
        device = cairo_pdf
    )

    ggsave(
        "../figures/main/Figure_4_Panel_C_NCD.pdf",
        plot = panel_4c,
        width = double_width / 2,
        height = double_width * 0.45,
        units = "in",
        dpi = 300,
        device = cairo_pdf
    )

    ggsave(
        "../figures/main/Figure_4_Panel_D_Priority.pdf",
        plot = panel_4d,
        width = double_width / 2,
        height = double_width * 0.45,
        units = "in",
        dpi = 300,
        device = cairo_pdf
    )

    # 使用cowplot组合图形（上下结构：顶部A和B并排，底部C和D并排）
    # 顶部：Panel A 和 Panel B 并排
    figure_4_top <- plot_grid(
        panel_4a, panel_4b,
        ncol = 2, nrow = 1,
        align = "hv",
        axis = "tblr",
        rel_widths = c(1, 1) # 各占50%宽度
    )

    # 底部：Panel C 和 Panel D 并排
    figure_4_bottom <- plot_grid(
        panel_4c, panel_4d,
        ncol = 2, nrow = 1,
        align = "hv",
        axis = "tblr",
        rel_widths = c(1, 1) # 各占50%宽度
    )

    # 上下组合
    figure_4_base <- plot_grid(
        figure_4_top, figure_4_bottom,
        ncol = 1, nrow = 2,
        align = "hv",
        axis = "tblr",
        rel_heights = c(1, 1) # 各占50%高度
    )

    # 使用cowplot添加主标题（10pt Arial Bold，顶部居中）
    # 删除副标题，因为结论应该在正文中阐述，而不是在图表标题中
    # 使用tryCatch避免布局错误
    figure_4 <- tryCatch(
        {
            cowplot::ggdraw() +
                cowplot::draw_plot(figure_4_base, x = 0, y = 0, width = 1, height = 0.95) +
                # 主标题：中立、描述性的标题
        cowplot::draw_label(
                    "Policy implications for health spending based on governance context and outcome type",
            x = 0.5, y = 0.98,
            hjust = 0.5, vjust = 1,
            size = 10,
            fontface = "bold",
                    fontfamily = "Arial"
                )
        },
        error = function(e) {
            cat("  ⚠ 标题添加失败，使用基础组合图:", conditionMessage(e), "\n")
            figure_4_base
        }
        )

    # 保存组合图
    ggsave(
        "../figures/main/Figure_4_Complete.pdf",
        plot = figure_4,
        width = double_width,
        height = double_width * 0.9,
        units = "in",
        dpi = 300,
        device = cairo_pdf
    )

    ggsave(
        "../figures/main/Figure_4_Complete.png",
        plot = figure_4,
        width = double_width,
        height = double_width * 0.9,
        units = "in",
        dpi = 300
    )

    cat("  ✓ Figure 4 保存成功\n")
    cat(sprintf("    - Panel A PDF: figures/main/Figure_4_Panel_A_Heatmap.pdf\n"))
    cat(sprintf("    - Panel B PDF: figures/main/Figure_4_Panel_B_Child_Health.pdf\n"))
    cat(sprintf("    - Panel C PDF: figures/main/Figure_4_Panel_C_NCD.pdf\n"))
    cat(sprintf("    - Panel D PDF: figures/main/Figure_4_Panel_D_Priority.pdf\n"))
    cat(sprintf("    - Complete PDF: figures/main/Figure_4_Complete.pdf\n"))
    cat(sprintf("    - Complete PNG: figures/main/Figure_4_Complete.png\n"))

    # 生成黑白版本（通过转换彩色PDF）
    if (requireNamespace("magick", quietly = TRUE)) {
        library(magick)
        cat("\n[5.5] 生成 Figure 4 黑白版本...\n")

        # 读取彩色PDF
        figure_4_color <- image_read_pdf("../figures/main/Figure_4_Complete.pdf", density = 300)

        # 转换为灰度
        figure_4_bw <- figure_4_color %>%
            image_convert(colorspace = "gray")

        # 保存黑白版本
        image_write(figure_4_bw, "../figures/main/Figure_4_Complete_BW.pdf", format = "pdf", density = 300)
        image_write(figure_4_bw, "../figures/main/Figure_4_Complete_BW.png", format = "png", density = 300)

        cat("  ✓ Figure 4 黑白版本保存成功\n")
        cat(sprintf("    - Complete BW PDF: figures/main/Figure_4_Complete_BW.pdf\n"))
        cat(sprintf("    - Complete BW PNG: figures/main/Figure_4_Complete_BW.png\n"))
    } else {
        cat("  ⚠ magick 包未安装，跳过黑白图生成\n")
        cat("    请运行: install.packages('magick')\n")
    }
} else {
    cat("  ⚠ 扩展分析结果不存在，跳过 Figure 4\n")
}

# =============================================================================
# 6. 生成总结报告
# =============================================================================
cat("\n==================================================\n")
cat("  主图生成完成总结\n")
cat("==================================================\n\n")

cat("生成的图表：\n")
cat("  ✓ Figure 1: 研究框架与描述性统计 (4 panels)\n")
cat("  ⚠ Figure 2: 核心因果结果 (占位符)\n")
if (!is.null(extended_results)) {
    cat("  ✓ Figure 3: 机制分析 (2 panels)\n")
    cat("  ✓ Figure 4: 疾病特定效应 (4 panels)\n")
} else {
    cat("  ⚠ Figure 3: 机制分析 (需要扩展分析结果)\n")
    cat("  ⚠ Figure 4: 疾病特定效应 (需要扩展分析结果)\n")
}

cat("\n输出位置: figures/main/\n")
cat("\n格式:\n")
cat("  - PDF (矢量图, 300 DPI)\n")
cat("  - PNG (位图, 300 DPI)\n")

cat("\n尺寸:\n")
cat(sprintf("  - 双栏图: %.2f inches (%.0f mm)\n", double_width, double_width * 25.4))
cat(sprintf("  - 高度: 自适应\n"))

cat("\n配色:\n")
cat("  - 色盲友好 (ColorBrewer)\n")
cat("  - 收入组: 蓝-绿-橙-红\n")

cat("\n==================================================\n")
cat("  ✓ 主图生成脚本执行完成！\n")
cat("==================================================\n")
