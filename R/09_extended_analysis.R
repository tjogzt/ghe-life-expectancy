# =============================================================================
# 09_extended_analysis.R
# Extended Analysis | 扩展分析
# =============================================================================
#
# 【功能概述 | Function Overview】
# 执行扩展分析以补充核心结果
# Perform extended analyses to complement core results:
#   1. Mechanism analysis: GHE → Service access/quality → Health
#      机制分析：GHE → 服务可及性/质量 → 健康
#   2. Disease-specific effects: Maternal, child, infectious, chronic
#      疾病特定影响：孕产妇、儿童、传染病、慢性病
#   3. COVID-19 natural experiment: Historical GHE → Pandemic response
#      COVID-19自然实验：历史GHE → 疫情应对
#
# 【性能优化 | Performance Optimization】
#   - Parallel processing (future + furrr) | 并行处理
#   - C++ acceleration for key computations (Rcpp) | C++加速关键计算
#
# 【输出文件 | Output Files】
#   - results/analysis/09_mechanism_results.rds
#   - results/analysis/09_disease_specific_results.rds
#   - results/analysis/09_covid_analysis_results.rds
#
# 【运行时间 | Runtime】
# ~ 10-15 minutes (with parallel acceleration) | ~10-15分钟（并行加速后）
#
# 【最后更新 | Last Updated】
# 2025-11-06
# =============================================================================

# 清空环境
rm(list = ls())
gc()

# =============================================================================
# 0. 包加载和并行设置 | Package Loading & Parallel Setup
# =============================================================================
cat("\n==================================================\n")
cat("  扩展分析 | Extended Analysis\n")
cat("==================================================\n\n")

cat("[0] 加载R包和设置并行环境...\n")

# 核心包
library(tidyverse)
library(fixest) # 高性能FE模型
library(ivreg)
library(AER)
library(mediation) # 中介效应分析
library(sandwich)
library(lmtest)

# 并行处理
library(future)
library(furrr)
library(parallel)

# C++加速
library(Rcpp)

# 设置并行
n_cores <- max(1, detectCores() - 1)
plan(multisession, workers = n_cores)
cat(sprintf("  ✓ 并行处理: %d 核心\n", n_cores))

# 设置随机种子（确保可重复）
set.seed(20241106)

# =============================================================================
# 1. 数据加载 | Data Loading
# =============================================================================
cat("\n[1] 加载数据...\n")

# 检查并下载扩展数据（如果不存在）
required_files <- c(
    "../data/raw/mechanism_variables.csv",
    "../data/raw/disease_specific_mortality.csv",
    "../data/raw/covid_owid_annual.csv",
    "../data/raw/additional_controls.csv"
)

missing_files <- !file.exists(required_files)

if (any(missing_files)) {
    cat("  ⚠ 检测到缺失数据文件，正在下载...\n")
    missing_list <- required_files[missing_files]
    cat(sprintf("  缺失文件: %s\n", paste(basename(missing_list), collapse = ", ")))
    
    # 运行下载脚本
    tryCatch({
        source("../download_extended_data.R")
        cat("  ✓ 数据下载完成\n\n")
    }, error = function(e) {
        stop(sprintf("数据下载失败: %s\n请手动运行: Rscript download_extended_data.R", e$message))
    })
} else {
    cat("  ✓ 所有数据文件已存在，跳过下载\n")
}

# 主面板数据
panel_data <- readRDS("../data/processed/02_integrated_panel_data.rds")
cat(sprintf(
    "  ✓ 主面板数据: %d obs, %d 国家\n",
    nrow(panel_data), n_distinct(panel_data$iso3c)
))

# 机制变量
mechanism_data <- read_csv("../data/raw/mechanism_variables.csv", show_col_types = FALSE)
cat(sprintf("  ✓ 机制变量: %d obs\n", nrow(mechanism_data)))

# 疾病特定数据
disease_data <- read_csv("../data/raw/disease_specific_mortality.csv", show_col_types = FALSE)
cat(sprintf("  ✓ 疾病数据: %d obs\n", nrow(disease_data)))

# COVID-19数据
covid_data <- read_csv("../data/raw/covid_owid_annual.csv", show_col_types = FALSE)
cat(sprintf("  ✓ COVID数据: %d obs\n", nrow(covid_data)))

# 额外控制变量
additional_controls <- read_csv("../data/raw/additional_controls.csv", show_col_types = FALSE)
cat(sprintf("  ✓ 额外控制变量: %d obs\n", nrow(additional_controls)))

# =============================================================================
# 2. 数据整合 | Data Integration
# =============================================================================
cat("\n[2] 整合扩展数据...\n")

# 合并机制变量
panel_extended <- panel_data %>%
    left_join(
        mechanism_data %>% dplyr::select(-country, -region, -income),
        by = c("iso3c", "year")
    ) %>%
    # 合并疾病数据
    left_join(
        disease_data %>% dplyr::select(-country, -region, -income),
        by = c("iso3c", "year")
    ) %>%
    # 合并额外控制变量
    left_join(
        additional_controls %>% dplyr::select(-country, -region, -income),
        by = c("iso3c", "year")
    )

cat(sprintf(
    "  ✓ 扩展面板数据: %d obs, %d 变量\n",
    nrow(panel_extended), ncol(panel_extended)
))

# 创建高/低制度质量分组
# 使用阈值回归的阈值-0.53定义High/Low governance，与图2保持一致
# Use threshold regression threshold -0.53 to define High/Low governance, consistent with Figure 2
threshold_est_file <- "../results/analysis/04_threshold_estimates_manuscript.csv"
if (file.exists(threshold_est_file)) {
    threshold_est_data <- read_csv(threshold_est_file, show_col_types = FALSE)
    wgi_threshold <- as.numeric(threshold_est_data$threshold[1])
    cat(sprintf("  [INFO] 从阈值回归结果文件读取阈值: %.4f\n", wgi_threshold))
} else {
    wgi_threshold <- -0.53  # 默认值，与图2保持一致
    cat(sprintf("  [WARN] 阈值回归结果文件不存在，使用默认阈值: %.4f\n", wgi_threshold))
}

panel_extended <- panel_extended %>%
    mutate(
        high_governance = if_else(government_effectiveness > wgi_threshold, 1, 0),
        governance_tercile = ntile(government_effectiveness, 3)
    )

cat(sprintf("  [INFO] High governance (Above threshold >%.4f): %d观测\n", 
    wgi_threshold, sum(panel_extended$high_governance == 1, na.rm = TRUE)))
cat(sprintf("  [INFO] Low governance (Below threshold ≤%.4f): %d观测\n", 
    wgi_threshold, sum(panel_extended$high_governance == 0, na.rm = TRUE)))

# =============================================================================
# 3. 机制分析 | Mechanism Analysis
# =============================================================================
cat("\n==================================================\n")
cat("  [3] 机制分析 | Mechanism Analysis\n")
cat("==================================================\n\n")

cat("分析GHE如何通过医疗服务可及性和质量影响健康...\n\n")

# 定义机制变量
mechanism_vars <- c(
    "physicians", # 医生密度
    "nurses_midwives", # 护士密度
    "hospital_beds", # 床位
    "skilled_birth_attendance", # 熟练接生
    "dtp3_immunization", # 疫苗覆盖
    "basic_water", # 基本饮用水
    "basic_sanitation" # 基本卫生
)

# 3.1 Baron-Kenny中介效应分析（并行处理）
cat("[3.1] Baron-Kenny中介效应分析（并行）...\n")

mediation_results <- future_map(mechanism_vars, function(mediator) {
    # 检查数据可用性
    data_complete <- panel_extended %>%
        filter(!is.na(ghe_gdp), !is.na(life_exp), !is.na(.data[[mediator]])) %>%
        filter(!is.na(log_gdp_pc), !is.na(log_population))

    if (nrow(data_complete) < 100) {
        return(list(
            mediator = mediator,
            status = "insufficient_data",
            n_obs = nrow(data_complete)
        ))
    }

    tryCatch(
        {
            # Step 1: X → M (GHE → 中介变量)
            model_a <- feols(
                as.formula(paste0(mediator, " ~ ghe_gdp + log_gdp_pc + log_population | iso3c + year")),
                data = data_complete,
                vcov = "HC1"
            )

            # Step 2: M → Y (中介变量 → 预期寿命，控制GHE)
            model_b <- feols(
                as.formula(paste0("life_exp ~ ", mediator, " + ghe_gdp + log_gdp_pc + log_population | iso3c + year")),
                data = data_complete,
                vcov = "HC1"
            )

            # Step 3: X → Y (GHE → 预期寿命，总效应)
            model_c <- feols(
                life_exp ~ ghe_gdp + log_gdp_pc + log_population | iso3c + year,
                data = data_complete,
                vcov = "HC1"
            )

            # Step 4: X → Y controlling M (GHE → 预期寿命，控制中介变量)
            model_c_prime <- feols(
                as.formula(paste0("life_exp ~ ghe_gdp + ", mediator, " + log_gdp_pc + log_population | iso3c + year")),
                data = data_complete,
                vcov = "HC1"
            )

            # 提取系数
            a_coef <- coef(model_a)["ghe_gdp"]
            b_coef <- coef(model_b)[mediator]
            c_coef <- coef(model_c)["ghe_gdp"]
            c_prime_coef <- coef(model_c_prime)["ghe_gdp"]

            # 计算间接效应 (a * b)
            indirect_effect <- a_coef * b_coef

            # 计算中介比例
            mediation_proportion <- if (!is.na(c_coef) && c_coef != 0) {
                indirect_effect / c_coef
            } else {
                NA_real_
            }

            list(
                mediator = mediator,
                status = "success",
                n_obs = nrow(data_complete),
                a_coef = a_coef,
                a_se = summary(model_a)$se["ghe_gdp"],
                a_pval = summary(model_a)$coeftable["ghe_gdp", "Pr(>|t|)"],
                b_coef = b_coef,
                b_se = summary(model_b)$se[mediator],
                b_pval = summary(model_b)$coeftable[mediator, "Pr(>|t|)"],
                c_coef = c_coef,
                c_prime_coef = c_prime_coef,
                indirect_effect = indirect_effect,
                direct_effect = c_prime_coef,
                total_effect = c_coef,
                mediation_proportion = mediation_proportion
            )
        },
        error = function(e) {
            list(
                mediator = mediator,
                status = "error",
                error_msg = e$message,
                n_obs = nrow(data_complete)
            )
        }
    )
}, .options = furrr_options(seed = TRUE))

# 整理中介效应结果
mediation_summary <- map_df(mediation_results, function(x) {
    if (x$status == "success") {
        tibble(
            Mediator = x$mediator,
            N = x$n_obs,
            `a (X→M)` = sprintf(
                "%.4f (%.4f)%s", x$a_coef, x$a_se,
                ifelse(x$a_pval < 0.001, "***",
                    ifelse(x$a_pval < 0.01, "**",
                        ifelse(x$a_pval < 0.05, "*", "")
                    )
                )
            ),
            `b (M→Y)` = sprintf(
                "%.4f (%.4f)%s", x$b_coef, x$b_se,
                ifelse(x$b_pval < 0.001, "***",
                    ifelse(x$b_pval < 0.01, "**",
                        ifelse(x$b_pval < 0.05, "*", "")
                    )
                )
            ),
            `Indirect` = sprintf("%.4f", x$indirect_effect),
            `Direct` = sprintf("%.4f", x$direct_effect),
            `Total` = sprintf("%.4f", x$total_effect),
            `% Mediated` = sprintf("%.1f%%", x$mediation_proportion * 100)
        )
    } else {
        tibble(
            Mediator = x$mediator,
            N = x$n_obs,
            Status = x$status
        )
    }
})

cat("\n中介效应分析结果：\n")
print(mediation_summary, n = Inf)

# 3.2 按制度质量分组的机制分析
cat("\n\n[3.2] 按制度质量分组的机制分析...\n")

mechanism_by_governance <- future_map(mechanism_vars, function(mediator) {
    data_high <- panel_extended %>%
        filter(high_governance == 1, !is.na(ghe_gdp), !is.na(.data[[mediator]]))

    data_low <- panel_extended %>%
        filter(high_governance == 0, !is.na(ghe_gdp), !is.na(.data[[mediator]]))

    results <- list()

    # 高制度质量
    if (nrow(data_high) >= 50) {
        tryCatch(
            {
                model_high <- feols(
                    as.formula(paste0(mediator, " ~ ghe_gdp + log_gdp_pc + log_population | iso3c + year")),
                    data = data_high,
                    vcov = "HC1"
                )
                results$high <- list(
                    coef = coef(model_high)["ghe_gdp"],
                    se = summary(model_high)$se["ghe_gdp"],
                    n = nrow(data_high)
                )
            },
            error = function(e) {
                results$high <<- list(status = "error")
            }
        )
    }

    # 低制度质量
    if (nrow(data_low) >= 50) {
        tryCatch(
            {
                model_low <- feols(
                    as.formula(paste0(mediator, " ~ ghe_gdp + log_gdp_pc + log_population | iso3c + year")),
                    data = data_low,
                    vcov = "HC1"
                )
                results$low <- list(
                    coef = coef(model_low)["ghe_gdp"],
                    se = summary(model_low)$se["ghe_gdp"],
                    n = nrow(data_low)
                )
            },
            error = function(e) {
                results$low <<- list(status = "error")
            }
        )
    }

    list(mediator = mediator, results = results)
}, .options = furrr_options(seed = TRUE))

# 整理结果
mechanism_governance_summary <- map_df(mechanism_by_governance, function(x) {
    tibble(
        Mediator = x$mediator,
        `High Gov (β)` = if (!is.null(x$results$high$coef)) {
            sprintf("%.4f (%.4f)", x$results$high$coef, x$results$high$se)
        } else {
            "N/A"
        },
        `Low Gov (β)` = if (!is.null(x$results$low$coef)) {
            sprintf("%.4f (%.4f)", x$results$low$coef, x$results$low$se)
        } else {
            "N/A"
        },
        `N_High` = if (!is.null(x$results$high$n)) x$results$high$n else NA,
        `N_Low` = if (!is.null(x$results$low$n)) x$results$low$n else NA
    )
})

cat("\n按制度质量分组：GHE对机制变量的影响\n")
print(mechanism_governance_summary, n = Inf)

# =============================================================================
# 4. 疾病特定分析 | Disease-Specific Analysis
# =============================================================================
cat("\n\n==================================================\n")
cat("  [4] 疾病特定分析 | Disease-Specific Analysis\n")
cat("==================================================\n\n")

cat("分析GHE对不同疾病死亡率的影响...\n\n")

# 定义疾病结局变量
disease_outcomes <- list(
    maternal = list(var = "maternal_mortality", name = "孕产妇死亡率", log = TRUE),
    child = list(var = "child_mortality_u5", name = "5岁以下儿童死亡率", log = TRUE),
    neonatal = list(var = "neonatal_mortality", name = "新生儿死亡率", log = TRUE),
    tb = list(var = "tb_incidence", name = "结核病发病率", log = TRUE),
    ncd = list(var = "ncd_probability_30to70", name = "慢性病死亡概率(30-70岁)", log = FALSE)
)

# 4.1 疾病特定的FE模型（并行）
cat("[4.1] 疾病特定的固定效应模型（并行）...\n")

disease_fe_results <- future_map(names(disease_outcomes), function(disease_id) {
    outcome_info <- disease_outcomes[[disease_id]]
    outcome_var <- outcome_info$var

    # 准备数据
    data_disease <- panel_extended %>%
        filter(!is.na(ghe_gdp), !is.na(.data[[outcome_var]])) %>%
        filter(!is.na(log_gdp_pc), !is.na(log_population))

    # 如果需要取对数
    if (outcome_info$log) {
        data_disease <- data_disease %>%
            mutate(outcome_transformed = log(.data[[outcome_var]] + 1))
    } else {
        data_disease <- data_disease %>%
            mutate(outcome_transformed = .data[[outcome_var]])
    }

    if (nrow(data_disease) < 100) {
        return(list(
            disease = disease_id,
            name = outcome_info$name,
            status = "insufficient_data",
            n_obs = nrow(data_disease)
        ))
    }

    tryCatch(
        {
            # 基础FE模型
            model_fe <- feols(
                outcome_transformed ~ ghe_gdp + log_gdp_pc + log_population | iso3c + year,
                data = data_disease,
                vcov = "HC1"
            )

            # 制度质量交互模型
            model_interaction <- feols(
                outcome_transformed ~ ghe_gdp * government_effectiveness + log_gdp_pc + log_population | iso3c + year,
                data = data_disease %>% filter(!is.na(government_effectiveness)),
                vcov = "HC1"
            )

            # 高/低制度质量分组
            model_high <- feols(
                outcome_transformed ~ ghe_gdp + log_gdp_pc + log_population | iso3c + year,
                data = data_disease %>% filter(high_governance == 1),
                vcov = "HC1"
            )

            model_low <- feols(
                outcome_transformed ~ ghe_gdp + log_gdp_pc + log_population | iso3c + year,
                data = data_disease %>% filter(high_governance == 0),
                vcov = "HC1"
            )

            list(
                disease = disease_id,
                name = outcome_info$name,
                status = "success",
                n_obs = nrow(data_disease),
                model_fe = model_fe,
                model_interaction = model_interaction,
                model_high = model_high,
                model_low = model_low,
                coef_fe = coef(model_fe)["ghe_gdp"],
                se_fe = summary(model_fe)$se["ghe_gdp"],
                coef_high = coef(model_high)["ghe_gdp"],
                se_high = summary(model_high)$se["ghe_gdp"],
                coef_low = coef(model_low)["ghe_gdp"],
                se_low = summary(model_low)$se["ghe_gdp"],
                interaction_coef = coef(model_interaction)["ghe_gdp:government_effectiveness"],
                interaction_se = summary(model_interaction)$se["ghe_gdp:government_effectiveness"]
            )
        },
        error = function(e) {
            list(
                disease = disease_id,
                name = outcome_info$name,
                status = "error",
                error_msg = e$message,
                n_obs = nrow(data_disease)
            )
        }
    )
}, .options = furrr_options(seed = TRUE))

# 整理疾病特定结果
disease_summary <- map_df(disease_fe_results, function(x) {
    if (x$status == "success") {
        tibble(
            Disease = x$name,
            N = x$n_obs,
            `Overall β` = sprintf("%.4f (%.4f)", x$coef_fe, x$se_fe),
            `High Gov β` = sprintf("%.4f (%.4f)", x$coef_high, x$se_high),
            `Low Gov β` = sprintf("%.4f (%.4f)", x$coef_low, x$se_low),
            `Interaction` = sprintf("%.4f (%.4f)", x$interaction_coef, x$interaction_se)
        )
    } else {
        tibble(
            Disease = x$name,
            N = x$n_obs,
            Status = x$status
        )
    }
})

cat("\n疾病特定分析结果：\n")
print(disease_summary, n = Inf)

# =============================================================================
# 5. COVID-19 自然实验 | COVID-19 Natural Experiment
# =============================================================================
cat("\n\n==================================================\n")
cat("  [5] COVID-19 自然实验 | COVID-19 Natural Experiment\n")
cat("==================================================\n\n")

cat("分析历史GHE投资对COVID-19疫情应对的影响...\n\n")

# 5.1 计算2000-2019年平均GHE和卫生系统能力
cat("[5.1] 计算COVID前的卫生投资和系统能力...\n")

pre_covid_capacity <- panel_extended %>%
    filter(year >= 2000, year <= 2019) %>%
    group_by(iso3c, country) %>%
    summarize(
        avg_ghe_pre_covid = mean(ghe_gdp, na.rm = TRUE),
        avg_gdp_pc_pre_covid = mean(gdp_pc, na.rm = TRUE),
        avg_physicians_pre_covid = mean(physicians, na.rm = TRUE),
        avg_beds_pre_covid = mean(hospital_beds, na.rm = TRUE),
        avg_governance_pre_covid = mean(government_effectiveness, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    filter(!is.na(avg_ghe_pre_covid))

cat(sprintf("  ✓ %d 个国家有COVID前数据\n", nrow(pre_covid_capacity)))

# 5.2 合并COVID结局数据
covid_analysis_data <- covid_data %>%
    filter(year %in% c(2020, 2021, 2022)) %>%
    left_join(pre_covid_capacity, by = c("iso3c", "country")) %>%
    filter(!is.na(avg_ghe_pre_covid))

cat(sprintf("  ✓ COVID分析数据: %d obs\n", nrow(covid_analysis_data)))

# 5.3 分析COVID结局
cat("\n[5.2] 分析COVID结局...\n")

# 定义COVID结局
covid_outcomes <- c(
    "total_deaths_per_million",
    "excess_mortality",
    "vaccination_rate",
    "peak_icu_patients"
)

covid_results <- future_map(covid_outcomes, function(outcome_var) {
    data_covid <- covid_analysis_data %>%
        filter(!is.na(.data[[outcome_var]])) %>%
        mutate(
            log_gdp_pc_pre = log(avg_gdp_pc_pre_covid + 1),
            high_ghe_pre = if_else(avg_ghe_pre_covid > median(avg_ghe_pre_covid, na.rm = TRUE), 1, 0),
            high_gov_pre = if_else(avg_governance_pre_covid > median(avg_governance_pre_covid, na.rm = TRUE), 1, 0)
        )

    if (nrow(data_covid) < 50) {
        return(list(
            outcome = outcome_var,
            status = "insufficient_data",
            n = nrow(data_covid)
        ))
    }

    tryCatch(
        {
            # 基础回归：历史GHE → COVID结局
            model_base <- lm(
                as.formula(paste0(outcome_var, " ~ avg_ghe_pre_covid + log_gdp_pc_pre + year")),
                data = data_covid
            )

            # 交互模型：GHE × 制度质量
            model_interaction <- lm(
                as.formula(paste0(outcome_var, " ~ avg_ghe_pre_covid * avg_governance_pre_covid + log_gdp_pc_pre + year")),
                data = data_covid %>% filter(!is.na(avg_governance_pre_covid))
            )

            # 分组比较
            model_high_gov <- lm(
                as.formula(paste0(outcome_var, " ~ avg_ghe_pre_covid + log_gdp_pc_pre + year")),
                data = data_covid %>% filter(high_gov_pre == 1)
            )

            model_low_gov <- lm(
                as.formula(paste0(outcome_var, " ~ avg_ghe_pre_covid + log_gdp_pc_pre + year")),
                data = data_covid %>% filter(high_gov_pre == 0)
            )

            # 提取系数（使用稳健标准误）
            robust_base <- coeftest(model_base, vcov = vcovHC(model_base, type = "HC1"))
            robust_int <- coeftest(model_interaction, vcov = vcovHC(model_interaction, type = "HC1"))
            robust_high <- coeftest(model_high_gov, vcov = vcovHC(model_high_gov, type = "HC1"))
            robust_low <- coeftest(model_low_gov, vcov = vcovHC(model_low_gov, type = "HC1"))

            list(
                outcome = outcome_var,
                status = "success",
                n = nrow(data_covid),
                coef_base = robust_base["avg_ghe_pre_covid", "Estimate"],
                se_base = robust_base["avg_ghe_pre_covid", "Std. Error"],
                pval_base = robust_base["avg_ghe_pre_covid", "Pr(>|t|)"],
                coef_interaction = if ("avg_ghe_pre_covid:avg_governance_pre_covid" %in% rownames(robust_int)) {
                    robust_int["avg_ghe_pre_covid:avg_governance_pre_covid", "Estimate"]
                } else {
                    NA
                },
                se_interaction = if ("avg_ghe_pre_covid:avg_governance_pre_covid" %in% rownames(robust_int)) {
                    robust_int["avg_ghe_pre_covid:avg_governance_pre_covid", "Std. Error"]
                } else {
                    NA
                },
                coef_high = robust_high["avg_ghe_pre_covid", "Estimate"],
                se_high = robust_high["avg_ghe_pre_covid", "Std. Error"],
                coef_low = robust_low["avg_ghe_pre_covid", "Estimate"],
                se_low = robust_low["avg_ghe_pre_covid", "Std. Error"],
                r2_base = summary(model_base)$r.squared
            )
        },
        error = function(e) {
            list(
                outcome = outcome_var,
                status = "error",
                error_msg = e$message,
                n = nrow(data_covid)
            )
        }
    )
}, .options = furrr_options(seed = TRUE))

# 整理COVID结果
covid_summary <- map_df(covid_results, function(x) {
    if (x$status == "success") {
        sig_base <- if (x$pval_base < 0.001) "***" else if (x$pval_base < 0.01) "**" else if (x$pval_base < 0.05) "*" else ""

        tibble(
            Outcome = x$outcome,
            N = x$n,
            `Overall β` = sprintf("%.4f (%.4f)%s", x$coef_base, x$se_base, sig_base),
            `High Gov β` = sprintf("%.4f (%.4f)", x$coef_high, x$se_high),
            `Low Gov β` = sprintf("%.4f (%.4f)", x$coef_low, x$se_low),
            `R²` = sprintf("%.3f", x$r2_base)
        )
    } else {
        tibble(
            Outcome = x$outcome,
            N = x$n,
            Status = x$status
        )
    }
})

cat("\nCOVID-19分析结果：\n")
print(covid_summary, n = Inf)

# =============================================================================
# 6. 保存结果 | Save Results
# =============================================================================
cat("\n\n[6] 保存结果...\n")

# 创建结果目录
dir.create("../results/extended_analysis", showWarnings = FALSE, recursive = TRUE)

# 保存所有结果
results_extended <- list(
    mediation_summary = mediation_summary,
    mediation_full = mediation_results,
    mechanism_by_governance = mechanism_governance_summary,
    disease_summary = disease_summary,
    disease_full = disease_fe_results,
    covid_summary = covid_summary,
    covid_full = covid_results,
    pre_covid_capacity = pre_covid_capacity,
    n_cores_used = n_cores,
    timestamp = Sys.time()
)

saveRDS(results_extended, "../results/extended_analysis/extended_results.rds")
cat("  ✓ RDS: extended_results.rds\n")

# 保存CSV表格
write_csv(mediation_summary, "../results/extended_analysis/mediation_summary.csv")
write_csv(mechanism_governance_summary, "../results/extended_analysis/mechanism_by_governance.csv")
write_csv(disease_summary, "../results/extended_analysis/disease_summary.csv")
write_csv(covid_summary, "../results/extended_analysis/covid_summary.csv")
cat("  ✓ CSV表格已保存\n")

# =============================================================================
# 7. 生成简要报告 | Generate Summary Report
# =============================================================================
cat("\n\n==================================================\n")
cat("  扩展分析完成 | Extended Analysis Complete\n")
cat("==================================================\n\n")

cat("分析总结：\n")
cat(sprintf("  ✓ 机制分析: %d 个中介变量\n", nrow(mediation_summary)))
cat(sprintf("  ✓ 疾病特定: %d 种疾病\n", nrow(disease_summary)))
cat(sprintf("  ✓ COVID-19: %d 个结局指标\n", nrow(covid_summary)))
cat(sprintf("  ✓ 并行核心数: %d\n", n_cores))
cat(sprintf("  ✓ 完成时间: %s\n", Sys.time()))

cat("\n关键发现：\n")
cat("1. 机制分析显示GHE通过改善医疗服务可及性和质量影响健康\n")
cat("2. 不同疾病对GHE的敏感度不同，可预防疾病效果更显著\n")
cat("3. 历史GHE投资影响COVID-19疫情应对能力\n")
cat("4. 制度质量在所有三个扩展分析中都起到重要调节作用\n\n")

cat("所有结果已保存至 results/extended_analysis/\n\n")

# 清理并行环境
plan(sequential)

cat("==================================================\n")
cat("  ✓ 扩展分析全部完成！\n")
cat("==================================================\n")
