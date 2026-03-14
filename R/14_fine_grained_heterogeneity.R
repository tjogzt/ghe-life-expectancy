# =============================================================================
# 14_fine_grained_heterogeneity.R
# Fine-Grained Heterogeneity Analysis | 更细粒度的异质性分析
# =============================================================================
#
# 【功能概述 | Function Overview】
# 分析政府卫生支出效应的更细粒度异质性
# Analyze fine-grained heterogeneity in government health expenditure effects:
# - Gender heterogeneity | 性别异质性
# - Age group heterogeneity | 年龄组异质性
# - Urban-rural heterogeneity | 城乡差异
#
# 【输入文件 | Input Files】
# - data/processed/02_integrated_panel_data.rds
#
# 【输出文件 | Output Files】
# - results/analysis/14_fine_grained_heterogeneity.rds
# - tables/Table_S10_Gender_Heterogeneity.csv
# - tables/Table_S11_Age_Group_Heterogeneity.csv
# - tables/Table_S12_Urban_Rural_Heterogeneity.csv
# - figures/Figure_S14_Gender_Heterogeneity.pdf
# - figures/Figure_S15_Age_Group_Heterogeneity.pdf
#
# 【运行时间 | Runtime】
# ~ 10-15 minutes | ~10-15分钟
#
# 【最后更新 | Last Updated】
# 2025-01-XX
# =============================================================================

# 运行配置脚本 | Run configuration script
if (!exists("CONFIG")) {
    source("01_setup_and_configuration.R")
}

cat("\n")
cat("=============================================================================\n")
cat("  14 - Fine-Grained Heterogeneity Analysis\n")
cat("=============================================================================\n\n")

log_message("Starting fine-grained heterogeneity analysis")

# =============================================================================
# 0. 加载必要的包 | Load Required Packages
# =============================================================================

# 检查并安装必要的包 | Check and install required packages
required_packages <- c("dplyr", "tidyr", "plm", "lmtest", "sandwich", 
                       "ggplot2", "gridExtra", "WDI", "readr")

for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
        install.packages(pkg)
        library(pkg, character.only = TRUE)
    }
}

cat("[OK] Required packages loaded\n")

# =============================================================================
# 1. 数据加载与准备 | Data Loading and Preparation
# =============================================================================

cat("\n[STEP 1/6] Loading and preparing data...\n")

# 加载整合的面板数据 | Load integrated panel data
panel_data <- readRDS(file.path("..", "data", "processed", "02_integrated_panel_data.rds"))

cat(sprintf(
    "[OK] Loaded panel data: %d observations, %d countries\n",
    nrow(panel_data), n_distinct(panel_data$iso3c)
))

# =============================================================================
# 2. 下载性别和年龄组数据（如需要）| Download Gender and Age Group Data (if needed)
# =============================================================================

cat("\n[STEP 2/6] Checking and downloading gender/age-specific data...\n")

# 检查数据目录 | Check data directory
gender_data_file <- file.path("..", "data", "raw", "gender_life_expectancy.csv")
age_mortality_file <- file.path("..", "data", "raw", "age_specific_mortality.csv")
urban_data_file <- file.path("..", "data", "raw", "urban_rural_health.csv")

# 初始化变量 | Initialize variables
gender_data <- NULL
age_data <- NULL

# 2.1 下载性别别预期寿命数据 | Download gender-specific life expectancy
if (!file.exists(gender_data_file)) {
    cat("[INFO] Downloading gender-specific life expectancy from WDI...\n")
    
    tryCatch({
        # WDI指标代码 | WDI indicator codes
        gender_indicators <- c(
            "SP.DYN.LE00.FE.IN",  # Life expectancy at birth, female (years)
            "SP.DYN.LE00.MA.IN"   # Life expectancy at birth, male (years)
        )
        
        gender_data <- WDI(
            indicator = gender_indicators,
            start = 2000,
            end = 2022,
            extra = TRUE
        ) %>%
            as_tibble() %>%
            filter(region != "Aggregates") %>%
            dplyr::select(
                iso3c, country, year, region, income,
                life_exp_female = SP.DYN.LE00.FE.IN,
                life_exp_male = SP.DYN.LE00.MA.IN
            ) %>%
            arrange(iso3c, year)
        
        write_csv(gender_data, gender_data_file)
        cat(sprintf("  ✓ Downloaded gender data: %d observations\n", nrow(gender_data)))
    }, error = function(e) {
        cat(sprintf("  ✗ Failed to download gender data: %s\n", e$message))
        gender_data <<- NULL  # 使用<<-确保赋值到外层作用域 | Use <<- to assign to outer scope
    })
} else {
    cat("[OK] Gender data file already exists\n")
    tryCatch({
        gender_data <- read_csv(gender_data_file, show_col_types = FALSE)
    }, error = function(e) {
        cat(sprintf("  ✗ Failed to read gender data file: %s\n", e$message))
        gender_data <<- NULL
    })
}

# 2.2 下载年龄别死亡率数据 | Download age-specific mortality rates
age_data <- NULL  # 初始化为NULL | Initialize as NULL

if (!file.exists(age_mortality_file)) {
    cat("[INFO] Downloading age-specific mortality rates from WDI...\n")
    
    tryCatch({
        # WDI指标代码 | WDI indicator codes
        age_indicators <- c(
            "SP.DYN.IMRT.IN",      # Infant mortality rate (per 1,000 live births) - 0-1岁
            "SH.DYN.MORT",         # Under-5 mortality rate (per 1,000 live births) - 0-5岁
            "SP.DYN.AMRT.MA",       # Adult male mortality rate (per 1,000 adults) - 15-49岁男性
            "SP.DYN.AMRT.FE",       # Adult female mortality rate (per 1,000 adults) - 15-49岁女性
            "SP.DYN.AMRT"           # Adult mortality rate (per 1,000 adults) - 15-49岁
        )
        
        age_data <- WDI(
            indicator = age_indicators,
            start = 2000,
            end = 2022,
            extra = TRUE
        ) %>%
            as_tibble() %>%
            filter(region != "Aggregates") %>%
            dplyr::select(
                iso3c, country, year, region, income,
                infant_mort = SP.DYN.IMRT.IN,
                u5_mort = SH.DYN.MORT,
                adult_mort_male = SP.DYN.AMRT.MA,
                adult_mort_female = SP.DYN.AMRT.FE,
                adult_mort = SP.DYN.AMRT
            ) %>%
            arrange(iso3c, year)
        
        write_csv(age_data, age_mortality_file)
        cat(sprintf("  ✓ Downloaded age-specific data: %d observations\n", nrow(age_data)))
    }, error = function(e) {
        cat(sprintf("  ✗ Failed to download age-specific data: %s\n", e$message))
        age_data <<- NULL  # 使用<<-确保赋值到外层作用域 | Use <<- to assign to outer scope
    })
} else {
    cat("[OK] Age-specific data file already exists\n")
    tryCatch({
        age_data <- read_csv(age_mortality_file, show_col_types = FALSE)
    }, error = function(e) {
        cat(sprintf("  ✗ Failed to read age-specific data file: %s\n", e$message))
        age_data <<- NULL
    })
}

# 2.3 检查城乡数据（使用城市化率作为代理）| Check urban-rural data (use urbanization rate as proxy)
cat("[INFO] Using urbanization rate as proxy for urban-rural analysis...\n")
# 城市化率数据应该已经在整合数据中 | Urbanization rate should already be in integrated data

# =============================================================================
# 3. 数据合并 | Data Merging
# =============================================================================

cat("\n[STEP 3/6] Merging data...\n")

# 合并性别数据 | Merge gender data
if (!is.null(gender_data)) {
    panel_data <- panel_data %>%
        left_join(
            gender_data %>% dplyr::select(iso3c, year, life_exp_female, life_exp_male),
            by = c("iso3c", "year")
        )
    cat("[OK] Gender data merged\n")
} else {
    cat("[WARN] Gender data not available, skipping gender analysis\n")
}

# 合并年龄组数据 | Merge age group data
if (!is.null(age_data)) {
    panel_data <- panel_data %>%
        left_join(
            age_data %>% dplyr::select(iso3c, year, infant_mort, u5_mort, 
                                       adult_mort_male, adult_mort_female, adult_mort),
            by = c("iso3c", "year")
        )
    cat("[OK] Age-specific data merged\n")
} else {
    cat("[WARN] Age-specific data not available, skipping age group analysis\n")
}

# 准备分析数据 | Prepare analysis data
analysis_data <- panel_data %>%
    filter(!is.na(life_exp), !is.na(ghe_gdp), !is.na(gdp_pc)) %>%
    mutate(
        log_gdp_pc = log(gdp_pc),
        log_population = log(population),
        time_trend = year - min(year) + 1
    ) %>%
    arrange(iso3c, year)

cat(sprintf("[OK] Analysis data prepared: %d observations\n", nrow(analysis_data)))

# =============================================================================
# 4. 性别异质性分析 | Gender Heterogeneity Analysis
# =============================================================================

cat("\n[STEP 4/6] Analyzing gender heterogeneity...\n")

gender_results <- list()

if (all(c("life_exp_female", "life_exp_male") %in% names(analysis_data))) {
    # 转换为面板数据 | Convert to panel data
    pdata_gender <- pdata.frame(analysis_data, index = c("iso3c", "year"))
    
    # 4.1 女性预期寿命 | Female life expectancy
    cat("[INFO] Estimating model for female life expectancy...\n")
    
    if (sum(!is.na(analysis_data$life_exp_female)) > 100) {
        pdata_female <- pdata.frame(
            analysis_data %>% filter(!is.na(life_exp_female)),
            index = c("iso3c", "year")
        )
        
        fe_female <- plm(
            life_exp_female ~ ghe_gdp + log_gdp_pc + log_population + time_trend,
            data = pdata_female,
            model = "within",
            effect = "twoways"
        )
        
        fe_female_robust <- coeftest(fe_female, vcov = vcovHC(fe_female, type = "HC1"))
        
        gender_results$female <- list(
            model = fe_female,
            robust = fe_female_robust,
            n_obs = nrow(pdata_female),
            n_countries = n_distinct(pdata_female$iso3c)
        )
        
        cat(sprintf(
            "[OK] Female: GHE coef = %.4f (SE: %.4f)\n",
            coef(fe_female)["ghe_gdp"],
            sqrt(diag(vcovHC(fe_female, type = "HC1")))["ghe_gdp"]
        ))
    }
    
    # 4.2 男性预期寿命 | Male life expectancy
    cat("[INFO] Estimating model for male life expectancy...\n")
    
    if (sum(!is.na(analysis_data$life_exp_male)) > 100) {
        pdata_male <- pdata.frame(
            analysis_data %>% filter(!is.na(life_exp_male)),
            index = c("iso3c", "year")
        )
        
        fe_male <- plm(
            life_exp_male ~ ghe_gdp + log_gdp_pc + log_population + time_trend,
            data = pdata_male,
            model = "within",
            effect = "twoways"
        )
        
        fe_male_robust <- coeftest(fe_male, vcov = vcovHC(fe_male, type = "HC1"))
        
        gender_results$male <- list(
            model = fe_male,
            robust = fe_male_robust,
            n_obs = nrow(pdata_male),
            n_countries = n_distinct(pdata_male$iso3c)
        )
        
        cat(sprintf(
            "[OK] Male: GHE coef = %.4f (SE: %.4f)\n",
            coef(fe_male)["ghe_gdp"],
            sqrt(diag(vcovHC(fe_male, type = "HC1")))["ghe_gdp"]
        ))
    }
    
    # 4.3 性别差异（交互项模型）| Gender difference (interaction model)
    cat("[INFO] Estimating gender interaction model...\n")
    
    # 创建长格式数据 | Create long format data
    gender_long <- analysis_data %>%
        filter(!is.na(life_exp_female), !is.na(life_exp_male)) %>%
        dplyr::select(iso3c, year, ghe_gdp, log_gdp_pc, log_population, time_trend,
                     life_exp_female, life_exp_male) %>%
        pivot_longer(
            cols = c(life_exp_female, life_exp_male),
            names_to = "gender",
            values_to = "life_exp_gender"
        ) %>%
        mutate(
            gender = ifelse(gender == "life_exp_female", "Female", "Male"),
            is_female = ifelse(gender == "Female", 1, 0)
        )
    
    if (nrow(gender_long) > 200) {
        pdata_gender_long <- pdata.frame(gender_long, index = c("iso3c", "year", "gender"))
        
        fe_gender_interaction <- plm(
            life_exp_gender ~ ghe_gdp * is_female + log_gdp_pc + log_population + time_trend,
            data = pdata_gender_long,
            model = "within",
            effect = "twoways"
        )
        
        fe_gender_interaction_robust <- coeftest(
            fe_gender_interaction,
            vcov = vcovHC(fe_gender_interaction, type = "HC1")
        )
        
        gender_results$interaction <- list(
            model = fe_gender_interaction,
            robust = fe_gender_interaction_robust,
            n_obs = nrow(gender_long)
        )
        
        cat("[OK] Gender interaction model estimated\n")
    }
    
} else {
    cat("[WARN] Gender-specific data not available, skipping gender analysis\n")
}

# =============================================================================
# 5. 年龄组异质性分析 | Age Group Heterogeneity Analysis
# =============================================================================

cat("\n[STEP 5/6] Analyzing age group heterogeneity...\n")

age_results <- list()

# 5.1 0-5岁儿童死亡率 | Under-5 mortality
if ("u5_mort" %in% names(analysis_data) && sum(!is.na(analysis_data$u5_mort)) > 100) {
    cat("[INFO] Estimating model for under-5 mortality...\n")
    
    pdata_u5 <- pdata.frame(
        analysis_data %>% filter(!is.na(u5_mort)),
        index = c("iso3c", "year")
    )
    
    # 使用对数变换（死亡率通常是对数正态分布）| Use log transformation (mortality rates are typically log-normal)
    fe_u5 <- plm(
        log(u5_mort + 1) ~ ghe_gdp + log_gdp_pc + log_population + time_trend,
        data = pdata_u5,
        model = "within",
        effect = "twoways"
    )
    
    fe_u5_robust <- coeftest(fe_u5, vcov = vcovHC(fe_u5, type = "HC1"))
    
    age_results$u5 <- list(
        model = fe_u5,
        robust = fe_u5_robust,
        n_obs = nrow(pdata_u5),
        n_countries = n_distinct(pdata_u5$iso3c)
    )
    
    cat(sprintf(
        "[OK] Under-5: GHE coef = %.4f (SE: %.4f)\n",
        coef(fe_u5)["ghe_gdp"],
        sqrt(diag(vcovHC(fe_u5, type = "HC1")))["ghe_gdp"]
    ))
}

# 5.2 15-49岁成人死亡率 | Adult mortality (15-49)
if ("adult_mort" %in% names(analysis_data) && sum(!is.na(analysis_data$adult_mort)) > 100) {
    cat("[INFO] Estimating model for adult mortality...\n")
    
    pdata_adult <- pdata.frame(
        analysis_data %>% filter(!is.na(adult_mort)),
        index = c("iso3c", "year")
    )
    
    fe_adult <- plm(
        log(adult_mort + 1) ~ ghe_gdp + log_gdp_pc + log_population + time_trend,
        data = pdata_adult,
        model = "within",
        effect = "twoways"
    )
    
    fe_adult_robust <- coeftest(fe_adult, vcov = vcovHC(fe_adult, type = "HC1"))
    
    age_results$adult <- list(
        model = fe_adult,
        robust = fe_adult_robust,
        n_obs = nrow(pdata_adult),
        n_countries = n_distinct(pdata_adult$iso3c)
    )
    
    cat(sprintf(
        "[OK] Adult: GHE coef = %.4f (SE: %.4f)\n",
        coef(fe_adult)["ghe_gdp"],
        sqrt(diag(vcovHC(fe_adult, type = "HC1")))["ghe_gdp"]
    ))
}

# 5.3 婴儿死亡率（0-1岁）| Infant mortality (0-1)
if ("infant_mort" %in% names(analysis_data) && sum(!is.na(analysis_data$infant_mort)) > 100) {
    cat("[INFO] Estimating model for infant mortality...\n")
    
    pdata_infant <- pdata.frame(
        analysis_data %>% filter(!is.na(infant_mort)),
        index = c("iso3c", "year")
    )
    
    fe_infant <- plm(
        log(infant_mort + 1) ~ ghe_gdp + log_gdp_pc + log_population + time_trend,
        data = pdata_infant,
        model = "within",
        effect = "twoways"
    )
    
    fe_infant_robust <- coeftest(fe_infant, vcov = vcovHC(fe_infant, type = "HC1"))
    
    age_results$infant <- list(
        model = fe_infant,
        robust = fe_infant_robust,
        n_obs = nrow(pdata_infant),
        n_countries = n_distinct(pdata_infant$iso3c)
    )
    
    cat(sprintf(
        "[OK] Infant: GHE coef = %.4f (SE: %.4f)\n",
        coef(fe_infant)["ghe_gdp"],
        sqrt(diag(vcovHC(fe_infant, type = "HC1")))["ghe_gdp"]
    ))
}

# =============================================================================
# 6. 城乡差异分析 | Urban-Rural Heterogeneity Analysis
# =============================================================================

cat("\n[STEP 6/6] Analyzing urban-rural heterogeneity...\n")

urban_results <- list()

# 检查是否有城市化率变量 | Check if urbanization rate variable exists
urban_vars <- c("urban_population_pct", "SP.URB.TOTL.IN.ZS", "urbanization_rate")

urban_var <- NULL
for (var in urban_vars) {
    if (var %in% names(analysis_data) && sum(!is.na(analysis_data[[var]])) > 100) {
        urban_var <- var
        break
    }
}

if (!is.null(urban_var)) {
    cat(sprintf("[INFO] Using %s as urbanization proxy...\n", urban_var))
    
    # 创建城市化分组 | Create urbanization groups
    # 先提取变量值用于计算分位数 | Extract variable values for quantile calculation
    urban_values <- analysis_data[[urban_var]]
    urban_q33 <- quantile(urban_values, 0.33, na.rm = TRUE)
    urban_q67 <- quantile(urban_values, 0.67, na.rm = TRUE)
    
    analysis_data_urban <- analysis_data %>%
        filter(!is.na(.data[[urban_var]])) %>%
        mutate(
            urban_group = case_when(
                .data[[urban_var]] < urban_q33 ~ "Low Urbanization",
                .data[[urban_var]] < urban_q67 ~ "Medium Urbanization",
                TRUE ~ "High Urbanization"
            ),
            urban_group = factor(urban_group, levels = c("Low Urbanization", "Medium Urbanization", "High Urbanization"))
        )
    
    # 6.1 按城市化水平分组回归 | Regression by urbanization level
    for (urban_level in levels(analysis_data_urban$urban_group)) {
        cat(sprintf("[INFO] Estimating model for %s countries...\n", urban_level))
        
        data_subset <- analysis_data_urban %>% filter(urban_group == urban_level)
        
        if (nrow(data_subset) > 50) {
            pdata_subset <- pdata.frame(data_subset, index = c("iso3c", "year"))
            
            fe_urban <- plm(
                life_exp ~ ghe_gdp + log_gdp_pc + log_population + time_trend,
                data = pdata_subset,
                model = "within",
                effect = "twoways"
            )
            
            fe_urban_robust <- coeftest(fe_urban, vcov = vcovHC(fe_urban, type = "HC1"))
            
            urban_results[[urban_level]] <- list(
                model = fe_urban,
                robust = fe_urban_robust,
                n_obs = nrow(data_subset),
                n_countries = n_distinct(data_subset$iso3c)
            )
            
            cat(sprintf(
                "[OK] %s: GHE coef = %.4f (SE: %.4f)\n",
                urban_level,
                coef(fe_urban)["ghe_gdp"],
                sqrt(diag(vcovHC(fe_urban, type = "HC1")))["ghe_gdp"]
            ))
        }
    }
    
    # 6.2 交互项模型 | Interaction model
    cat("[INFO] Estimating urbanization interaction model...\n")
    
    # 重命名变量以便在公式中使用 | Rename variable for use in formula
    analysis_data_urban_formula <- analysis_data_urban %>%
        rename(urban_var_value = !!sym(urban_var))
    
    pdata_urban <- pdata.frame(analysis_data_urban_formula, index = c("iso3c", "year"))
    
    fe_urban_interaction <- plm(
        life_exp ~ ghe_gdp * urban_var_value + log_gdp_pc + log_population + time_trend,
        data = pdata_urban,
        model = "within",
        effect = "twoways"
    )
    
    fe_urban_interaction_robust <- coeftest(
        fe_urban_interaction,
        vcov = vcovHC(fe_urban_interaction, type = "HC1")
    )
    
    urban_results$interaction <- list(
        model = fe_urban_interaction,
        robust = fe_urban_interaction_robust,
        n_obs = nrow(analysis_data_urban)
    )
    
    cat("[OK] Urbanization interaction model estimated\n")
    
} else {
    cat("[WARN] Urbanization data not available, skipping urban-rural analysis\n")
}

# =============================================================================
# 7. 保存结果 | Save Results
# =============================================================================

cat("\n[STEP 7/7] Saving results...\n")

# 保存完整结果对象 | Save complete results object
all_results <- list(
    gender = gender_results,
    age = age_results,
    urban = urban_results,
    data_summary = list(
        total_obs = nrow(analysis_data),
        n_countries = n_distinct(analysis_data$iso3c),
        year_range = range(analysis_data$year, na.rm = TRUE)
    )
)

saveRDS(all_results, file.path("..", "results", "analysis", "14_fine_grained_heterogeneity.rds"))
cat("[OK] Results saved to RDS file\n")

# 生成汇总表格 | Generate summary tables
# 性别异质性表格 | Gender heterogeneity table
if (length(gender_results) > 0) {
    gender_table <- data.frame(
        Group = character(),
        Coefficient = numeric(),
        SE = numeric(),
        p_value = numeric(),
        CI_lower = numeric(),
        CI_upper = numeric(),
        N_obs = numeric(),
        N_countries = numeric(),
        stringsAsFactors = FALSE
    )
    
    if (!is.null(gender_results$female)) {
        coef_val <- coef(gender_results$female$model)["ghe_gdp"]
        se_val <- sqrt(diag(vcovHC(gender_results$female$model, type = "HC1")))["ghe_gdp"]
        p_val <- gender_results$female$robust["ghe_gdp", "Pr(>|t|)"]
        
        gender_table <- rbind(gender_table, data.frame(
            Group = "Female",
            Coefficient = coef_val,
            SE = se_val,
            p_value = p_val,
            CI_lower = coef_val - 1.96 * se_val,
            CI_upper = coef_val + 1.96 * se_val,
            N_obs = gender_results$female$n_obs,
            N_countries = gender_results$female$n_countries
        ))
    }
    
    if (!is.null(gender_results$male)) {
        coef_val <- coef(gender_results$male$model)["ghe_gdp"]
        se_val <- sqrt(diag(vcovHC(gender_results$male$model, type = "HC1")))["ghe_gdp"]
        p_val <- gender_results$male$robust["ghe_gdp", "Pr(>|t|)"]
        
        gender_table <- rbind(gender_table, data.frame(
            Group = "Male",
            Coefficient = coef_val,
            SE = se_val,
            p_value = p_val,
            CI_lower = coef_val - 1.96 * se_val,
            CI_upper = coef_val + 1.96 * se_val,
            N_obs = gender_results$male$n_obs,
            N_countries = gender_results$male$n_countries
        ))
    }
    
    write_csv(gender_table, file.path("..", "tables", "Table_S10_Gender_Heterogeneity.csv"))
    cat("[OK] Gender heterogeneity table saved\n")
}

# 年龄组异质性表格 | Age group heterogeneity table
if (length(age_results) > 0) {
    age_table <- data.frame(
        Age_Group = character(),
        Outcome = character(),
        Coefficient = numeric(),
        SE = numeric(),
        p_value = numeric(),
        CI_lower = numeric(),
        CI_upper = numeric(),
        N_obs = numeric(),
        N_countries = numeric(),
        stringsAsFactors = FALSE
    )
    
    if (!is.null(age_results$u5)) {
        coef_val <- coef(age_results$u5$model)["ghe_gdp"]
        se_val <- sqrt(diag(vcovHC(age_results$u5$model, type = "HC1")))["ghe_gdp"]
        p_val <- age_results$u5$robust["ghe_gdp", "Pr(>|t|)"]
        
        age_table <- rbind(age_table, data.frame(
            Age_Group = "0-5 years",
            Outcome = "Under-5 mortality (log)",
            Coefficient = coef_val,
            SE = se_val,
            p_value = p_val,
            CI_lower = coef_val - 1.96 * se_val,
            CI_upper = coef_val + 1.96 * se_val,
            N_obs = age_results$u5$n_obs,
            N_countries = age_results$u5$n_countries
        ))
    }
    
    if (!is.null(age_results$adult)) {
        coef_val <- coef(age_results$adult$model)["ghe_gdp"]
        se_val <- sqrt(diag(vcovHC(age_results$adult$model, type = "HC1")))["ghe_gdp"]
        p_val <- age_results$adult$robust["ghe_gdp", "Pr(>|t|)"]
        
        age_table <- rbind(age_table, data.frame(
            Age_Group = "15-49 years",
            Outcome = "Adult mortality (log)",
            Coefficient = coef_val,
            SE = se_val,
            p_value = p_val,
            CI_lower = coef_val - 1.96 * se_val,
            CI_upper = coef_val + 1.96 * se_val,
            N_obs = age_results$adult$n_obs,
            N_countries = age_results$adult$n_countries
        ))
    }
    
    if (!is.null(age_results$infant)) {
        coef_val <- coef(age_results$infant$model)["ghe_gdp"]
        se_val <- sqrt(diag(vcovHC(age_results$infant$model, type = "HC1")))["ghe_gdp"]
        p_val <- age_results$infant$robust["ghe_gdp", "Pr(>|t|)"]
        
        age_table <- rbind(age_table, data.frame(
            Age_Group = "0-1 years",
            Outcome = "Infant mortality (log)",
            Coefficient = coef_val,
            SE = se_val,
            p_value = p_val,
            CI_lower = coef_val - 1.96 * se_val,
            CI_upper = coef_val + 1.96 * se_val,
            N_obs = age_results$infant$n_obs,
            N_countries = age_results$infant$n_countries
        ))
    }
    
    write_csv(age_table, file.path("..", "tables", "Table_S11_Age_Group_Heterogeneity.csv"))
    cat("[OK] Age group heterogeneity table saved\n")
}

# 城乡差异表格 | Urban-rural heterogeneity table
if (length(urban_results) > 0 && !is.null(urban_results$interaction)) {
    urban_table <- data.frame(
        Urbanization_Level = character(),
        Coefficient = numeric(),
        SE = numeric(),
        p_value = numeric(),
        CI_lower = numeric(),
        CI_upper = numeric(),
        N_obs = numeric(),
        N_countries = numeric(),
        stringsAsFactors = FALSE
    )
    
    for (level in names(urban_results)) {
        if (level != "interaction" && !is.null(urban_results[[level]])) {
            coef_val <- coef(urban_results[[level]]$model)["ghe_gdp"]
            se_val <- sqrt(diag(vcovHC(urban_results[[level]]$model, type = "HC1")))["ghe_gdp"]
            p_val <- urban_results[[level]]$robust["ghe_gdp", "Pr(>|t|)"]
            
            urban_table <- rbind(urban_table, data.frame(
                Urbanization_Level = level,
                Coefficient = coef_val,
                SE = se_val,
                p_value = p_val,
                CI_lower = coef_val - 1.96 * se_val,
                CI_upper = coef_val + 1.96 * se_val,
                N_obs = urban_results[[level]]$n_obs,
                N_countries = urban_results[[level]]$n_countries
            ))
        }
    }
    
    if (nrow(urban_table) > 0) {
        write_csv(urban_table, file.path("..", "tables", "Table_S12_Urban_Rural_Heterogeneity.csv"))
        cat("[OK] Urban-rural heterogeneity table saved\n")
    }
}

cat("\n=============================================================================\n")
cat("  Fine-Grained Heterogeneity Analysis Complete\n")
cat("=============================================================================\n\n")

log_message("Fine-grained heterogeneity analysis completed successfully")

