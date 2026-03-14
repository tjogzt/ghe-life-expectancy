# =============================================================================
# 21_generate_extended_analysis_figures.R
# Generate Extended Analysis Figures | 生成扩展分析图表
# =============================================================================
#
# 【功能概述 | Function Overview】
# 为所有扩展分析生成可视化图表
# Generate visualization figures for all extended analyses:
# - Multiple threshold comparison | 多阈值模型对比
# - Long-term dynamic effects | 长期动态效应
# - Environmental interactions | 环境交互作用
# - Gender/urban-rural heterogeneity | 性别/城乡异质性
# - HALE analysis | HALE分析
#
# 【输入文件 | Input Files】
# - results/analysis/14_fine_grained_heterogeneity.rds
# - results/analysis/15_dynamic_effects.rds
# - results/analysis/16_nonlinear_threshold.rds
# - results/analysis/17_alternative_outcomes.rds
# - results/analysis/20_environmental_interaction.rds
#
# 【输出文件 | Output Files】
# - figures/extended/Figure_SX1_Multiple_Threshold_Comparison.pdf
# - figures/extended/Figure_SX2_Long_Term_Effects.pdf
# - figures/extended/Figure_SX3_Environmental_Interactions.pdf
# - figures/extended/Figure_SX4_Heterogeneity_Analysis.pdf
# - figures/extended/Figure_SX5_HALE_Analysis.pdf
#
# 【运行时间 | Runtime】
# ~ 5-10 minutes | ~5-10分钟
#
# 【最后更新 | Last Updated】
# 2025-12-22
# =============================================================================

# 运行配置脚本 | Run configuration script
if (!exists("CONFIG")) {
    source("01_setup_and_configuration.R")
}

cat("\n")
cat("=============================================================================\n")
cat("  21 - Generate Extended Analysis Figures\n")
cat("=============================================================================\n\n")

log_message("Starting extended analysis figures generation")

# =============================================================================
# 0. 加载必要的包 | Load Required Packages
# =============================================================================

required_packages <- c("dplyr", "tidyr", "ggplot2", "readr", "gridExtra", 
                       "patchwork", "scales", "viridis")

for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
        install.packages(pkg)
        library(pkg, character.only = TRUE)
    }
}

cat("[OK] Required packages loaded\n")

# 设置Lancet标准主题 | Set Lancet standard theme
lancet_theme <- theme_bw(base_size = 9, base_family = "sans") +
    theme(
        text = element_text(family = "sans", size = 9),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        plot.margin = margin(5, 5, 5, 5, "mm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey90", linewidth = 0.3)
    )

# Lancet配色方案 | Lancet color scheme
lancet_colors <- list(
    sequential_blue = c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1",
                       "#6baed6", "#4292c6", "#2171b5", "#08519c"),
    diverging = c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"),
    qualitative = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3",
                    "#ff7f00", "#ffff33", "#a65628", "#f781bf")
)

# 创建输出目录 | Create output directory
dir.create(file.path("..", "figures", "extended"), 
          showWarnings = FALSE, recursive = TRUE)

# 图形尺寸设置 | Figure dimensions
# 单栏: 85mm = 3.35 inches
# 双栏: 180mm = 7.09 inches
single_width <- 3.35
double_width <- 7.09
panel_height <- 3.35

# =============================================================================
# 1. 多阈值模型对比图 | Multiple Threshold Comparison
# =============================================================================

cat("\n[STEP 1/5] Generating multiple threshold comparison figure...\n")

tryCatch({
    threshold_results <- readRDS(file.path("..", "results", "analysis", 
                                           "16_nonlinear_threshold.rds"))
    
    # 提取多阈值结果 | Extract multiple threshold results
    if (!is.null(threshold_results$multiple_threshold)) {
        mt_results <- threshold_results$multiple_threshold
        
        # 创建对比数据 | Create comparison data
        threshold_comparison <- data.frame(
            Model = c("Single", "Double", "Triple"),
            AIC = c(mt_results$single$aic, 
                   mt_results$double$aic, 
                   mt_results$triple$aic),
            BIC = c(mt_results$single$bic, 
                   mt_results$double$bic, 
                   mt_results$triple$bic),
            SSR = c(mt_results$single$ssr, 
                   mt_results$double$ssr, 
                   mt_results$triple$ssr)
        )
        
        # Panel A: AIC对比 | Panel A: AIC comparison
        p1 <- ggplot(threshold_comparison, aes(x = Model, y = AIC, fill = Model)) +
            geom_bar(stat = "identity", width = 0.6) +
            scale_fill_manual(values = lancet_colors$qualitative[1:3]) +
            labs(x = "Threshold Model", y = "AIC") +
            lancet_theme +
            theme(legend.position = "none") +
            annotate("text", x = 0.5, y = max(threshold_comparison$AIC) * 0.95,
                    label = "A", size = 5, fontface = "bold", hjust = 0)
        
        # Panel B: BIC对比 | Panel B: BIC comparison
        p2 <- ggplot(threshold_comparison, aes(x = Model, y = BIC, fill = Model)) +
            geom_bar(stat = "identity", width = 0.6) +
            scale_fill_manual(values = lancet_colors$qualitative[1:3]) +
            labs(x = "Threshold Model", y = "BIC") +
            lancet_theme +
            theme(legend.position = "none") +
            annotate("text", x = 0.5, y = max(threshold_comparison$BIC) * 0.95,
                    label = "B", size = 5, fontface = "bold", hjust = 0)
        
        # 组合图表 | Combine plots
        fig_threshold <- p1 + p2 + plot_layout(ncol = 2)
        
        # 保存 | Save
        ggsave(
            filename = file.path("..", "figures", "extended", 
                               "Figure_SX1_Multiple_Threshold_Comparison.pdf"),
            plot = fig_threshold,
            width = double_width,
            height = panel_height,
            units = "in",
            dpi = 300
        )
        
        cat("[OK] Multiple threshold comparison figure saved\n")
    }
}, error = function(e) {
    cat(sprintf("[WARN] Failed to generate threshold figure: %s\n", e$message))
})

# =============================================================================
# 2. 长期动态效应图 | Long-Term Dynamic Effects
# =============================================================================

cat("\n[STEP 2/5] Generating long-term dynamic effects figure...\n")

tryCatch({
    # 直接从表格读取累积效应数据 | Read cumulative effects directly from table
    cum_table <- read_csv(file.path("..", "tables", 
                                    "Table_S14_Cumulative_Effects.csv"),
                         show_col_types = FALSE)
    
    if (nrow(cum_table) > 0) {
        cum_data <- cum_table %>%
            mutate(
                CI_lower = Coefficient - 1.96 * SE,
                CI_upper = Coefficient + 1.96 * SE,
                Significant = p_value < 0.05
            )
        
        # 生成图表 | Generate plot
        p_cum <- ggplot(cum_data, aes(x = Years, y = Coefficient)) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
            geom_point(aes(color = Significant), size = 3) +
            geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper, color = Significant),
                         width = 0.5, linewidth = 0.8) +
            scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "#d7191c"),
                             labels = c("FALSE" = "Not significant", 
                                       "TRUE" = "Significant (p<0.05)"),
                             name = "Significance") +
            labs(x = "Cumulative Years", 
                y = "Cumulative Effect of GHE on Life Expectancy",
                title = NULL) +
            lancet_theme +
            annotate("text", x = min(cum_data$Years) - 0.5, 
                    y = max(cum_data$CI_upper) * 0.9,
                    label = "A", size = 5, fontface = "bold", hjust = 0)
        
        # 保存 | Save
        ggsave(
            filename = file.path("..", "figures", "extended", 
                               "Figure_SX2_Long_Term_Effects.pdf"),
            plot = p_cum,
            width = single_width,
            height = panel_height,
            units = "in",
            dpi = 300
        )
        
        cat("[OK] Long-term dynamic effects figure saved\n")
    } else {
        cat("[WARN] No cumulative effects data available\n")
    }
}, error = function(e) {
    cat(sprintf("[WARN] Failed to generate dynamic effects figure: %s\n", e$message))
})

# =============================================================================
# 3. 环境交互作用图 | Environmental Interactions
# =============================================================================

cat("\n[STEP 3/5] Generating environmental interactions figure...\n")

tryCatch({
    env_results <- readRDS(file.path("..", "results", "analysis", 
                                    "20_environmental_interaction.rds"))
    
    # 读取环境交互表格 | Read environmental interaction table
    env_table <- read_csv(file.path("..", "tables", 
                                    "Table_S26_Environmental_Interaction.csv"),
                         show_col_types = FALSE)
    
    if (nrow(env_table) > 0) {
        # 准备数据 | Prepare data
        env_plot_data <- env_table %>%
            mutate(
                CI_lower = GHE_Coefficient - 1.96 * GHE_SE,
                CI_upper = GHE_Coefficient + 1.96 * GHE_SE,
                Significant = abs(GHE_Coefficient / GHE_SE) > 1.96
            )
        
        # Panel A: GHE系数 | Panel A: GHE coefficients
        p1 <- ggplot(env_plot_data, 
                    aes(x = reorder(Environmental_Indicator, GHE_Coefficient), 
                        y = GHE_Coefficient)) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
            geom_point(aes(color = Significant), size = 3) +
            geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper, color = Significant),
                         width = 0.3, linewidth = 0.8) +
            scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "#d7191c"),
                             guide = "none") +
            labs(x = "Environmental Indicator", 
                y = "GHE Coefficient (95% CI)") +
            coord_flip() +
            lancet_theme +
            annotate("text", x = 0.3, y = max(env_plot_data$CI_upper) * 0.9,
                    label = "A", size = 5, fontface = "bold", hjust = 0)
        
        # Panel B: 交互项系数 | Panel B: Interaction coefficients
        p2 <- ggplot(env_plot_data, 
                    aes(x = reorder(Environmental_Indicator, Interaction_Coefficient), 
                        y = Interaction_Coefficient)) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
            geom_point(aes(color = abs(Interaction_Coefficient / Interaction_SE) > 1.96), 
                      size = 3) +
            geom_errorbar(aes(ymin = Interaction_Coefficient - 1.96 * Interaction_SE,
                             ymax = Interaction_Coefficient + 1.96 * Interaction_SE,
                             color = abs(Interaction_Coefficient / Interaction_SE) > 1.96),
                         width = 0.3, linewidth = 0.8) +
            scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "#2c7bb6"),
                             guide = "none") +
            labs(x = "Environmental Indicator", 
                y = "Interaction Coefficient (95% CI)") +
            coord_flip() +
            lancet_theme +
            annotate("text", x = 0.3, y = max(env_plot_data$Interaction_Coefficient + 
                                              1.96 * env_plot_data$Interaction_SE) * 0.9,
                    label = "B", size = 5, fontface = "bold", hjust = 0)
        
        # 组合图表 | Combine plots
        fig_env <- p1 + p2 + plot_layout(ncol = 2)
        
        # 保存 | Save
        ggsave(
            filename = file.path("..", "figures", "extended", 
                               "Figure_SX3_Environmental_Interactions.pdf"),
            plot = fig_env,
            width = double_width,
            height = panel_height,
            units = "in",
            dpi = 300
        )
        
        cat("[OK] Environmental interactions figure saved\n")
    }
}, error = function(e) {
    cat(sprintf("[WARN] Failed to generate environmental interactions figure: %s\n", e$message))
})

# =============================================================================
# 4. 异质性分析图 | Heterogeneity Analysis
# =============================================================================

cat("\n[STEP 4/5] Generating heterogeneity analysis figure...\n")

tryCatch({
    hetero_results <- readRDS(file.path("..", "results", "analysis", 
                                        "14_fine_grained_heterogeneity.rds"))
    
    # 读取性别异质性表格 | Read gender heterogeneity table
    gender_table <- read_csv(file.path("..", "tables", 
                                       "Table_S10_Gender_Heterogeneity.csv"),
                            show_col_types = FALSE)
    
    # 读取城乡异质性表格 | Read urban-rural heterogeneity table
    urban_table <- read_csv(file.path("..", "tables", 
                                     "Table_S12_Urban_Rural_Heterogeneity.csv"),
                           show_col_types = FALSE)
    
    # Panel A: 性别异质性 | Panel A: Gender heterogeneity
    if (nrow(gender_table) > 0) {
        gender_plot_data <- gender_table %>%
            mutate(
                CI_lower = Coefficient - 1.96 * SE,
                CI_upper = Coefficient + 1.96 * SE,
                Gender = Group  # 使用Group列作为Gender
            )
        
        p1 <- ggplot(gender_plot_data, 
                    aes(x = Gender, y = Coefficient, fill = Gender)) +
            geom_bar(stat = "identity", width = 0.6) +
            geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                         width = 0.2, linewidth = 0.8) +
            scale_fill_manual(values = lancet_colors$qualitative[1:2]) +
            labs(x = "Gender", y = "GHE Coefficient (95% CI)") +
            lancet_theme +
            theme(legend.position = "none") +
            annotate("text", x = 0.5, y = max(gender_plot_data$CI_upper) * 0.9,
                    label = "A", size = 5, fontface = "bold", hjust = 0)
    } else {
        p1 <- ggplot() + theme_void()
    }
    
    # Panel B: 城乡异质性 | Panel B: Urban-rural heterogeneity
    if (nrow(urban_table) > 0) {
        urban_plot_data <- urban_table %>%
            mutate(
                CI_lower = Coefficient - 1.96 * SE,
                CI_upper = Coefficient + 1.96 * SE,
                Urban_Group = Urbanization_Level  # 使用Urbanization_Level列
            )
        
        p2 <- ggplot(urban_plot_data, 
                    aes(x = Urban_Group, y = Coefficient, fill = Urban_Group)) +
            geom_bar(stat = "identity", width = 0.6) +
            geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                         width = 0.2, linewidth = 0.8) +
            scale_fill_manual(values = lancet_colors$qualitative[1:3]) +
            labs(x = "Urbanization Level", y = "GHE Coefficient (95% CI)") +
            lancet_theme +
            theme(legend.position = "none", 
                 axis.text.x = element_text(angle = 45, hjust = 1)) +
            annotate("text", x = 0.5, y = max(urban_plot_data$CI_upper) * 0.9,
                    label = "B", size = 5, fontface = "bold", hjust = 0)
    } else {
        p2 <- ggplot() + theme_void()
    }
    
    # 组合图表 | Combine plots
    fig_hetero <- p1 + p2 + plot_layout(ncol = 2)
    
    # 保存 | Save
    ggsave(
        filename = file.path("..", "figures", "extended", 
                           "Figure_SX4_Heterogeneity_Analysis.pdf"),
        plot = fig_hetero,
        width = double_width,
        height = panel_height,
        units = "in",
        dpi = 300
    )
    
    cat("[OK] Heterogeneity analysis figure saved\n")
    
}, error = function(e) {
    cat(sprintf("[WARN] Failed to generate heterogeneity figure: %s\n", e$message))
})

# =============================================================================
# 5. HALE分析图 | HALE Analysis
# =============================================================================

cat("\n[STEP 5/5] Generating HALE analysis figure...\n")

tryCatch({
    # 读取HALE分析表格 | Read HALE analysis table
    hale_table <- read_csv(file.path("..", "tables", 
                                     "Table_S19_HALE_Analysis.csv"),
                          show_col_types = FALSE)
    
    if (nrow(hale_table) > 0) {
        hale_plot_data <- hale_table %>%
            mutate(
                Significant = p_value < 0.05  # 使用小写p_value
            )
        
        # 生成图表 | Generate plot
        p_hale <- ggplot(hale_plot_data, 
                        aes(x = Outcome, y = Coefficient)) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
            geom_point(aes(color = Significant), size = 4) +
            geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper, color = Significant),
                         width = 0.2, linewidth = 1) +
            scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "#d7191c"),
                             labels = c("FALSE" = "Not significant", 
                                       "TRUE" = "Significant (p<0.05)"),
                             name = "Significance") +
            labs(x = "Health Outcome", 
                y = "GHE Coefficient (95% CI)",
                title = NULL) +
            lancet_theme +
            annotate("text", x = 0.5, y = max(hale_plot_data$CI_upper) * 0.9,
                    label = "A", size = 5, fontface = "bold", hjust = 0)
        
        # 保存 | Save
        ggsave(
            filename = file.path("..", "figures", "extended", 
                               "Figure_SX5_HALE_Analysis.pdf"),
            plot = p_hale,
            width = single_width,
            height = panel_height,
            units = "in",
            dpi = 300
        )
        
        cat("[OK] HALE analysis figure saved\n")
    }
}, error = function(e) {
    cat(sprintf("[WARN] Failed to generate HALE figure: %s\n", e$message))
})

# =============================================================================
# 完成 | Complete
# =============================================================================

cat("\n=============================================================================\n")
cat("  Extended Analysis Figures Generation Complete\n")
cat("=============================================================================\n\n")

log_message("Extended analysis figures generation completed successfully")

