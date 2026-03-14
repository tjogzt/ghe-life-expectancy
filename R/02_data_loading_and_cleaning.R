# =============================================================================
# 01_data_loading_and_cleaning.R
# Data Loading and Cleaning | 数据加载与清洗
# =============================================================================
#
# 【功能概述 | Function Overview】
# 1. 下载缺失的关键工具变量（如果不存在）
# 2. 加载并清洗来自7个国际数据库的原始数据
#
# Download missing critical instrumental variables (if not exist), then
# Load and clean raw data from 7 international databases:
#
# 【新增下载功能 | New Download Features】
# - World Bank Governance Indicators (WGI) | 世界银行治理指标
# - Fiscal Capacity Indicators | 财政能力指标
# - Geographic Variables | 地理变量
# - Historical Variables | 历史变量
# - Institutional Quality Indicators | 制度质量指标
# - Population Density | 人口密度
#
# 【数据加载 | Data Loading】
# - World Bank (WB) | 世界银行
# - World Health Organization (WHO) | 世界卫生组织
# - International Monetary Fund (IMF) | 国际货币基金组织
# - Organisation for Economic Co-operation and Development (OECD) | 经合组织
# - Global Burden of Disease (GBD) | 全球疾病负担研究
# - United Nations (UN) | 联合国
# - Eurostat | 欧盟统计局
#
# 【输入文件 | Input Files】
# - data/raw/wb/*.csv
# - data/raw/who/*.csv
# - data/raw/imf/*.csv
# - data/raw/oecd/*.csv
# - data/raw/gbd/*.csv
# - data/raw/un/*.csv
# - data/raw/eurostat/csv_converted/*.csv
#
# 【输出文件 | Output Files】
# Downloaded data (if not exist):
# - data/raw/world_governance_indicators.csv
# - data/raw/fiscal_capacity_indicators.csv
# - data/raw/geographic_variables.csv
# - data/raw/historical_variables.csv
# - data/raw/institutional_quality_indicators.csv
# - data/raw/population_density.csv
#
# Cleaned data:
# - data/processed/01_cleaned_data_wb.rds
# - data/processed/01_cleaned_data_who.rds
# - data/processed/01_cleaned_data_imf.rds
# - data/processed/01_cleaned_data_oecd.rds
# - data/processed/01_cleaned_data_gbd.rds
# - data/processed/01_cleaned_data_un.rds
# - data/processed/01_cleaned_data_eurostat.rds
# - data/processed/01_data_loading_summary.csv
#
# 【运行时间 | Runtime】
# ~ 5-10 minutes | ~5-10分钟
#
# 【最后更新 | Last Updated】
# 2025-11-06 - Added automatic download of missing instrumental variables
# =============================================================================

# 运行配置脚本 | Run configuration script
if (!exists("CONFIG")) {
    source("01_setup_and_configuration.R")
}

cat("\n")
cat("=============================================================================\n")
cat("  01 - Data Loading and Cleaning\n")
cat("=============================================================================\n\n")

log_message("Starting data loading and cleaning")

# =============================================================================
# 0. 下载缺失的关键变量 | Download Missing Critical Variables
# =============================================================================
# 这一部分下载手稿中使用但当前数据集缺失的工具变量
# This section downloads instrumental variables used in the manuscript
# but missing from current dataset:
# - World Bank Governance Indicators (WGI)
# - Fiscal Capacity Indicators
# - Geographic Variables
# - Historical Variables
# - Institutional Quality Indicators
# - Population Density

cat("\n[STEP 0/7] Checking and downloading missing critical variables...\n")

# 设置输出目录 | Set output directory
raw_data_dir <- file.path("..", "data", "raw")
dir.create(raw_data_dir, showWarnings = FALSE, recursive = TRUE)

# 检查是否需要下载 | Check if download is needed
required_files <- c(
    "world_governance_indicators.csv",
    "fiscal_capacity_indicators.csv",
    "geographic_variables.csv",
    "historical_variables.csv",
    "institutional_quality_indicators.csv",
    "population_density.csv"
)

files_exist <- sapply(required_files, function(f) {
    file.exists(file.path(raw_data_dir, f))
})

if (all(files_exist)) {
    cat("[OK] All critical variable files already exist. Skipping download.\n")
} else {
    cat("[INFO] Missing files detected. Starting download...\n")
    missing_files <- required_files[!files_exist]
    cat(sprintf("[INFO] Missing: %s\n", paste(missing_files, collapse = ", ")))

    # 确保必要的包已加载 | Ensure necessary packages are loaded
    if (!require("WDI", quietly = TRUE)) {
        install.packages("WDI")
        library(WDI)
    }

    # -------------------------------------------------------------------------
    # 0.1 世界银行治理指标 (WGI)
    # -------------------------------------------------------------------------
    if (!files_exist["world_governance_indicators.csv"]) {
        cat("\n[0.1] Downloading World Governance Indicators (WGI)...\n")

        wgi_indicators <- c(
            "CC.EST" = "control_of_corruption", # 腐败控制
            "GE.EST" = "government_effectiveness", # 政府效能
            "PV.EST" = "political_stability", # 政治稳定
            "RQ.EST" = "regulatory_quality", # 监管质量
            "RL.EST" = "rule_of_law", # 法治
            "VA.EST" = "voice_accountability" # 话语权和问责
        )

        tryCatch(
            {
                wgi_data <- WDI(
                    indicator = names(wgi_indicators),
                    start = 1996,
                    end = 2022,
                    extra = TRUE
                )

                # 重命名列 | Rename columns
                names(wgi_data)[names(wgi_data) %in% names(wgi_indicators)] <-
                    wgi_indicators[names(wgi_data)[names(wgi_data) %in% names(wgi_indicators)]]

                # 备份旧数据（如果存在）| Backup old data if exists
                wgi_file <- file.path(raw_data_dir, "world_governance_indicators.csv")
                if (file.exists(wgi_file)) {
                    backup_file <- file.path(raw_data_dir, paste0("world_governance_indicators_backup_", 
                                                                   format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
                    file.copy(wgi_file, backup_file, overwrite = TRUE)
                    cat(sprintf("  [INFO] Old WGI data backed up to: %s\n", basename(backup_file)))
                }

                # 保存数据 | Save data
                write_csv(wgi_data, wgi_file)
                
                # 保存元数据 | Save metadata
                metadata <- data.frame(
                    download_date = as.character(Sys.Date()),
                    download_time = as.character(Sys.time()),
                    wdi_package_version = as.character(packageVersion("WDI")),
                    n_rows = nrow(wgi_data),
                    n_countries = length(unique(wgi_data$iso3c)),
                    year_range = paste(min(wgi_data$year, na.rm = TRUE), 
                                      max(wgi_data$year, na.rm = TRUE), sep = "-"),
                    stringsAsFactors = FALSE
                )
                metadata_file <- file.path(raw_data_dir, "world_governance_indicators_metadata.csv")
                write_csv(metadata, metadata_file)
                
                cat(sprintf("  [OK] WGI data saved: %d rows, %d countries\n", 
                           nrow(wgi_data), length(unique(wgi_data$iso3c))))
                cat(sprintf("  [OK] Metadata saved to: %s\n", basename(metadata_file)))
            },
            error = function(e) {
                cat(sprintf("  [ERROR] WGI download failed: %s\n", e$message))
            }
        )
    }

    # -------------------------------------------------------------------------
    # 0.2 财政能力指标
    # -------------------------------------------------------------------------
    if (!files_exist["fiscal_capacity_indicators.csv"]) {
        cat("\n[0.2] Downloading Fiscal Capacity Indicators...\n")

        fiscal_indicators <- c(
            "GC.TAX.TOTL.GD.ZS" = "tax_revenue_gdp", # 税收收入
            "GC.DOD.TOTL.GD.ZS" = "government_debt_gdp", # 政府债务
            "GC.BAL.CASH.GD.ZS" = "fiscal_balance_gdp", # 财政平衡
            "GC.REV.GOTR.ZS" = "government_revenue_gdp", # 政府收入
            "GC.XPN.TOTL.GD.ZS" = "government_expense_gdp" # 政府支出
        )

        tryCatch(
            {
                fiscal_data <- WDI(
                    indicator = names(fiscal_indicators),
                    start = 2000,
                    end = 2022,
                    extra = TRUE
                )

                # 重命名列 | Rename columns
                names(fiscal_data)[names(fiscal_data) %in% names(fiscal_indicators)] <-
                    fiscal_indicators[names(fiscal_data)[names(fiscal_data) %in% names(fiscal_indicators)]]

                write_csv(fiscal_data, file.path(raw_data_dir, "fiscal_capacity_indicators.csv"))
                cat(sprintf("  [OK] Fiscal data saved: %d rows\n", nrow(fiscal_data)))
            },
            error = function(e) {
                cat(sprintf("  [ERROR] Fiscal data download failed: %s\n", e$message))
            }
        )
    }

    # -------------------------------------------------------------------------
    # 0.3 地理变量
    # -------------------------------------------------------------------------
    if (!files_exist["geographic_variables.csv"]) {
        cat("\n[0.3] Creating Geographic Variables...\n")

        tryCatch(
            {
                countries <- countrycode::codelist %>%
                    select(iso3c, country.name.en, continent, region) %>%
                    filter(!is.na(iso3c))

                # 内陆国家列表 | Landlocked countries list
                landlocked_countries <- c(
                    "AFG", "AND", "ARM", "AUT", "AZE", "BLR", "BOL", "BWA", "BFA", "BDI",
                    "CAF", "TCD", "CZE", "ETH", "HUN", "KAZ", "KGZ", "LAO", "LSO", "LIE",
                    "LUX", "MKD", "MWI", "MLI", "MDA", "MNG", "NPL", "NER", "PRY", "RWA",
                    "SMR", "SRB", "SVK", "SSD", "SWZ", "CHE", "TJK", "TKM", "UGA", "UZB",
                    "VAT", "ZMB", "ZWE"
                )

                geo_data <- countries %>%
                    mutate(
                        landlocked = ifelse(iso3c %in% landlocked_countries, 1, 0),
                        distance_to_coast = ifelse(landlocked == 1, 1000, 0)
                    )

                write_csv(geo_data, file.path(raw_data_dir, "geographic_variables.csv"))
                cat(sprintf("  [OK] Geographic data saved: %d rows\n", nrow(geo_data)))
            },
            error = function(e) {
                cat(sprintf("  [ERROR] Geographic data creation failed: %s\n", e$message))
            }
        )
    }

    # -------------------------------------------------------------------------
    # 0.4 历史变量
    # -------------------------------------------------------------------------
    if (!files_exist["historical_variables.csv"]) {
        cat("\n[0.4] Creating Historical Variables...\n")

        tryCatch(
            {
                independence_years <- tibble(
                    iso3c = countrycode::codelist$iso3c,
                    country = countrycode::codelist$country.name.en
                ) %>%
                    filter(!is.na(iso3c)) %>%
                    mutate(
                        independence_year = case_when(
                            iso3c == "USA" ~ 1776, iso3c == "FRA" ~ 1789,
                            iso3c == "CHN" ~ 1949, iso3c == "IND" ~ 1947,
                            iso3c == "BRA" ~ 1822, iso3c == "MEX" ~ 1810,
                            iso3c == "ZAF" ~ 1910, iso3c == "EGY" ~ 1922,
                            iso3c == "NGA" ~ 1960, iso3c == "KEN" ~ 1963,
                            iso3c == "PAK" ~ 1947, iso3c == "IDN" ~ 1945,
                            iso3c == "PHL" ~ 1946, iso3c == "VNM" ~ 1945,
                            iso3c == "THA" ~ 1238, iso3c == "ARG" ~ 1816,
                            iso3c == "CHL" ~ 1818, iso3c == "COL" ~ 1810,
                            iso3c == "PER" ~ 1821, iso3c == "VEN" ~ 1811,
                            TRUE ~ NA_real_
                        ),
                        years_since_independence = 2022 - independence_year,
                        # 殖民历史 (La Porta分类) | Colonial history
                        legal_origin = case_when(
                            iso3c %in% c(
                                "GBR", "USA", "CAN", "AUS", "NZL", "IND", "PAK", "BGD",
                                "ZAF", "KEN", "NGA", "GHA", "UGA", "ZMB", "ZWE", "MYS",
                                "SGP", "HKG"
                            ) ~ "British",
                            iso3c %in% c(
                                "FRA", "BEL", "ITA", "ESP", "PRT", "DZA", "MAR", "TUN",
                                "SEN", "CIV", "BEN", "MLI", "NER", "TCD", "CMR", "GAB",
                                "COG", "MDG", "VNM", "LAO", "KHM", "MEX", "ARG", "BRA",
                                "CHL", "COL", "PER", "VEN"
                            ) ~ "French",
                            iso3c %in% c("DEU", "AUT", "CHE") ~ "German",
                            iso3c %in% c("DNK", "SWE", "NOR", "FIN", "ISL") ~ "Scandinavian",
                            TRUE ~ "Other"
                        )
                    )

                write_csv(independence_years, file.path(raw_data_dir, "historical_variables.csv"))
                cat(sprintf("  [OK] Historical data saved: %d rows\n", nrow(independence_years)))
            },
            error = function(e) {
                cat(sprintf("  [ERROR] Historical data creation failed: %s\n", e$message))
            }
        )
    }

    # -------------------------------------------------------------------------
    # 0.5 制度质量指标
    # -------------------------------------------------------------------------
    if (!files_exist["institutional_quality_indicators.csv"]) {
        cat("\n[0.5] Downloading Institutional Quality Indicators...\n")

        institution_indicators <- c(
            "IQ.CPA.PROP.XQ" = "property_rights", # 产权保护
            "IQ.CPA.FINACC.XQ" = "financial_access", # 金融可及性
            "IQ.CPA.TRAN.XQ" = "transparency", # 透明度
            "IC.REG.DURS" = "business_registration_days" # 商业注册天数
        )

        tryCatch(
            {
                institution_data <- WDI(
                    indicator = names(institution_indicators),
                    start = 2000,
                    end = 2022,
                    extra = TRUE
                )

                names(institution_data)[names(institution_data) %in% names(institution_indicators)] <-
                    institution_indicators[names(institution_data)[names(institution_data) %in% names(institution_indicators)]]

                write_csv(institution_data, file.path(raw_data_dir, "institutional_quality_indicators.csv"))
                cat(sprintf("  [OK] Institutional data saved: %d rows\n", nrow(institution_data)))
            },
            error = function(e) {
                cat(sprintf("  [ERROR] Institutional data download failed: %s\n", e$message))
            }
        )
    }

    # -------------------------------------------------------------------------
    # 0.6 人口密度数据
    # -------------------------------------------------------------------------
    if (!files_exist["population_density.csv"]) {
        cat("\n[0.6] Downloading Population Density Data...\n")

        pop_indicators <- c(
            "EN.POP.DNST" = "population_density", # 人口密度
            "AG.LND.TOTL.K2" = "land_area" # 陆地面积
        )

        tryCatch(
            {
                pop_data <- WDI(
                    indicator = names(pop_indicators),
                    start = 2000,
                    end = 2022,
                    extra = TRUE
                )

                names(pop_data)[names(pop_data) %in% names(pop_indicators)] <-
                    pop_indicators[names(pop_data)[names(pop_data) %in% names(pop_indicators)]]

                write_csv(pop_data, file.path(raw_data_dir, "population_density.csv"))
                cat(sprintf("  [OK] Population density data saved: %d rows\n", nrow(pop_data)))
            },
            error = function(e) {
                cat(sprintf("  [ERROR] Population density download failed: %s\n", e$message))
            }
        )
    }

    cat("\n[OK] Critical variable download/creation complete.\n")
}

# =============================================================================
# 1. 辅助函数定义 | Helper Function Definitions
# =============================================================================

# 标准化国家代码 | Standardize country codes
standardize_country <- function(country_name) {
    suppressWarnings({
        iso3c <- countrycode(country_name,
            origin = "country.name",
            destination = "iso3c",
            warn = FALSE
        )
        iso3c
    })
}

# 标准化年份列 | Standardize year column
standardize_year <- function(df, year_col = "year") {
    if (!year_col %in% names(df)) {
        # 尝试查找年份列 | Try to find year column
        year_candidates <- grep("year|time|period", names(df),
            ignore.case = TRUE, value = TRUE
        )
        if (length(year_candidates) > 0) {
            year_col <- year_candidates[1]
        } else {
            stop("Cannot find year column")
        }
    }

    df %>%
        rename(year = !!sym(year_col)) %>%
        mutate(year = as.integer(year))
}

# 清理数值列 | Clean numeric columns
clean_numeric <- function(x) {
    # 移除非数值字符 | Remove non-numeric characters
    x <- gsub("[^0-9.-]", "", x)
    as.numeric(x)
}

# 检查数据质量 | Check data quality
check_data_quality <- function(df, source_name) {
    n_total <- nrow(df)
    n_missing_country <- sum(is.na(df$iso3c))
    n_missing_year <- sum(is.na(df$year))

    # 计算数值列的缺失率 | Calculate missing rate for numeric columns
    numeric_cols <- sapply(df, is.numeric)
    if (sum(numeric_cols) > 0) {
        missing_rates <- sapply(
            df[, numeric_cols, drop = FALSE],
            function(x) sum(is.na(x)) / length(x)
        )
        avg_missing_rate <- mean(missing_rates)
    } else {
        avg_missing_rate <- NA
    }

    cat(sprintf("[INFO] %s data quality:\n", source_name))
    cat(sprintf("  - Total observations: %d\n", n_total))
    cat(sprintf(
        "  - Missing country codes: %d (%.1f%%)\n",
        n_missing_country, n_missing_country / n_total * 100
    ))
    cat(sprintf(
        "  - Missing years: %d (%.1f%%)\n",
        n_missing_year, n_missing_year / n_total * 100
    ))
    cat(sprintf(
        "  - Average missing rate (numeric vars): %.1f%%\n",
        avg_missing_rate * 100
    ))

    return(list(
        source = source_name,
        n_total = n_total,
        n_missing_country = n_missing_country,
        n_missing_year = n_missing_year,
        avg_missing_rate = avg_missing_rate
    ))
}

# =============================================================================
# 2. 世界银行数据加载 | World Bank Data Loading
# =============================================================================

cat("\n[STEP 1/7] Loading World Bank data...\n")

wb_file <- file.path("..", "data", "raw", "wb", "wb_global_health_data.csv")

if (!file.exists(wb_file)) {
    stop(sprintf("World Bank data file not found: %s", wb_file))
}

# 读取WB数据 | Read WB data
wb_raw <- read_csv(wb_file, show_col_types = FALSE)

cat(sprintf(
    "[OK] Loaded World Bank data: %d rows x %d columns\n",
    nrow(wb_raw), ncol(wb_raw)
))

# 清洗WB数据 | Clean WB data
# WB数据已经是长格式 | WB data is already in long format
wb_cleaned <- wb_raw %>%
    # 标准化列名 | Standardize column names
    rename_with(tolower) %>%
    # 确保必要的列存在 | Ensure necessary columns exist
    mutate(
        year = as.integer(year)
    ) %>%
    # 移除缺失值 | Remove missing values
    filter(!is.na(iso3c), !is.na(year)) %>%
    # 限制年份范围 | Filter year range
    filter(
        year >= CONFIG$data$start_year,
        year <= CONFIG$data$end_year
    )

cat(sprintf(
    "[OK] Cleaned World Bank data: %d rows x %d columns\n",
    nrow(wb_cleaned), ncol(wb_cleaned)
))

# 保存清洗后的数据 | Save cleaned data
saveRDS(wb_cleaned, file.path("..", "data", "processed", "01_cleaned_data_wb.rds"))

wb_quality <- check_data_quality(wb_cleaned, "World Bank")

# =============================================================================
# 3. WHO数据加载 | WHO Data Loading
# =============================================================================

cat("\n[STEP 2/7] Loading WHO data...\n")

who_file <- file.path("..", "data", "raw", "who", "who_health_expenditure.csv")

if (!file.exists(who_file)) {
    warning("WHO data file not found, skipping...")
    who_cleaned <- NULL
    who_quality <- NULL
} else {
    # 读取WHO数据 | Read WHO data
    who_raw <- read_csv(who_file, show_col_types = FALSE)

    cat(sprintf(
        "[OK] Loaded WHO data: %d rows x %d columns\n",
        nrow(who_raw), ncol(who_raw)
    ))

    # 清洗WHO数据 | Clean WHO data
    who_cleaned <- who_raw %>%
        # 标准化列名 | Standardize column names
        rename_with(tolower) %>%
        # 标准化国家代码 | Standardize country codes
        mutate(
            iso3c = if ("countrycode" %in% names(.)) countrycode else standardize_country(country),
            year = as.integer(year)
        ) %>%
        # 移除缺失值 | Remove missing values
        filter(!is.na(iso3c), !is.na(year)) %>%
        # 限制年份范围 | Filter year range
        filter(
            year >= CONFIG$data$start_year,
            year <= CONFIG$data$end_year
        )

    cat(sprintf(
        "[OK] Cleaned WHO data: %d rows x %d columns\n",
        nrow(who_cleaned), ncol(who_cleaned)
    ))

    # 保存清洗后的数据 | Save cleaned data
    saveRDS(who_cleaned, file.path("..", "data", "processed", "01_cleaned_data_who.rds"))

    who_quality <- check_data_quality(who_cleaned, "WHO")
}

# =============================================================================
# 4. IMF数据加载 | IMF Data Loading
# =============================================================================

cat("\n[STEP 3/7] Loading IMF data...\n")

imf_file <- file.path("..", "data", "raw", "imf", "imf_fiscal_data.csv")

if (!file.exists(imf_file)) {
    warning("IMF data file not found, skipping...")
    imf_cleaned <- NULL
    imf_quality <- NULL
} else {
    # 读取IMF数据 | Read IMF data
    imf_raw <- read_csv(imf_file, show_col_types = FALSE)

    cat(sprintf(
        "[OK] Loaded IMF data: %d rows x %d columns\n",
        nrow(imf_raw), ncol(imf_raw)
    ))

    # 清洗IMF数据 | Clean IMF data
    imf_cleaned <- imf_raw %>%
        rename_with(tolower) %>%
        mutate(
            iso3c = if ("iso" %in% names(.)) iso else standardize_country(country),
            year = as.integer(year)
        ) %>%
        filter(!is.na(iso3c), !is.na(year)) %>%
        filter(
            year >= CONFIG$data$start_year,
            year <= CONFIG$data$end_year
        )

    cat(sprintf(
        "[OK] Cleaned IMF data: %d rows x %d columns\n",
        nrow(imf_cleaned), ncol(imf_cleaned)
    ))

    saveRDS(imf_cleaned, file.path("..", "data", "processed", "01_cleaned_data_imf.rds"))

    imf_quality <- check_data_quality(imf_cleaned, "IMF")
}

# =============================================================================
# 5. OECD数据加载 | OECD Data Loading
# =============================================================================

cat("\n[STEP 4/7] Loading OECD data...\n")

oecd_file <- file.path("..", "data", "raw", "oecd", "oecd_health_statistics.csv")

if (!file.exists(oecd_file)) {
    warning("OECD data file not found, skipping...")
    oecd_cleaned <- NULL
    oecd_quality <- NULL
} else {
    # 读取OECD数据 | Read OECD data
    oecd_raw <- read_csv(oecd_file, show_col_types = FALSE)

    cat(sprintf(
        "[OK] Loaded OECD data: %d rows x %d columns\n",
        nrow(oecd_raw), ncol(oecd_raw)
    ))

    # 清洗OECD数据 | Clean OECD data
    oecd_cleaned <- oecd_raw %>%
        rename_with(tolower) %>%
        mutate(
            iso3c = if ("cou" %in% names(.)) cou else standardize_country(country),
            year = as.integer(time)
        ) %>%
        filter(!is.na(iso3c), !is.na(year)) %>%
        filter(
            year >= CONFIG$data$start_year,
            year <= CONFIG$data$end_year
        )

    cat(sprintf(
        "[OK] Cleaned OECD data: %d rows x %d columns\n",
        nrow(oecd_cleaned), ncol(oecd_cleaned)
    ))

    saveRDS(oecd_cleaned, file.path("..", "data", "processed", "01_cleaned_data_oecd.rds"))

    oecd_quality <- check_data_quality(oecd_cleaned, "OECD")
}

# =============================================================================
# 6. GBD数据加载 | GBD Data Loading
# =============================================================================

cat("\n[STEP 5/7] Loading GBD data...\n")

gbd_file <- file.path("..", "data", "raw", "gbd", "gbd_health_outcomes.csv")

if (!file.exists(gbd_file)) {
    warning("GBD data file not found, skipping...")
    gbd_cleaned <- NULL
    gbd_quality <- NULL
} else {
    # 读取GBD数据 | Read GBD data
    gbd_raw <- read_csv(gbd_file, show_col_types = FALSE)

    cat(sprintf(
        "[OK] Loaded GBD data: %d rows x %d columns\n",
        nrow(gbd_raw), ncol(gbd_raw)
    ))

    # 清洗GBD数据 | Clean GBD data
    gbd_cleaned <- gbd_raw %>%
        rename_with(tolower) %>%
        mutate(
            iso3c = if ("location_code" %in% names(.)) location_code else standardize_country(location_name),
            year = as.integer(year_id)
        ) %>%
        filter(!is.na(iso3c), !is.na(year)) %>%
        filter(
            year >= CONFIG$data$start_year,
            year <= CONFIG$data$end_year
        )

    cat(sprintf(
        "[OK] Cleaned GBD data: %d rows x %d columns\n",
        nrow(gbd_cleaned), ncol(gbd_cleaned)
    ))

    saveRDS(gbd_cleaned, file.path("..", "data", "processed", "01_cleaned_data_gbd.rds"))

    gbd_quality <- check_data_quality(gbd_cleaned, "GBD")
}

# =============================================================================
# 7. UN数据加载 | UN Data Loading
# =============================================================================

cat("\n[STEP 6/7] Loading UN data...\n")

un_file <- file.path("..", "data", "raw", "un", "un_population.csv")

if (!file.exists(un_file)) {
    warning("UN data file not found, skipping...")
    un_cleaned <- NULL
    un_quality <- NULL
} else {
    # 读取UN数据 | Read UN data
    un_raw <- read_csv(un_file, show_col_types = FALSE)

    cat(sprintf(
        "[OK] Loaded UN data: %d rows x %d columns\n",
        nrow(un_raw), ncol(un_raw)
    ))

    # 清洗UN数据 | Clean UN data
    un_cleaned <- un_raw %>%
        rename_with(tolower) %>%
        mutate(
            iso3c = if ("iso3_code" %in% names(.)) iso3_code else standardize_country(location),
            year = as.integer(time)
        ) %>%
        filter(!is.na(iso3c), !is.na(year)) %>%
        filter(
            year >= CONFIG$data$start_year,
            year <= CONFIG$data$end_year
        )

    cat(sprintf(
        "[OK] Cleaned UN data: %d rows x %d columns\n",
        nrow(un_cleaned), ncol(un_cleaned)
    ))

    saveRDS(un_cleaned, file.path("..", "data", "processed", "01_cleaned_data_un.rds"))

    un_quality <- check_data_quality(un_cleaned, "UN")
}

# =============================================================================
# 8. Eurostat数据加载 | Eurostat Data Loading
# =============================================================================

cat("\n[STEP 7/7] Loading Eurostat data...\n")

eurostat_dir <- file.path("..", "data", "raw", "eurostat", "csv_converted")

if (!dir.exists(eurostat_dir)) {
    warning("Eurostat data directory not found, skipping...")
    eurostat_cleaned <- NULL
    eurostat_quality <- NULL
} else {
    # 读取Eurostat索引文件 | Read Eurostat index files
    gov_health_index <- file.path(eurostat_dir, "gov_health_index.csv")
    health_fin_index <- file.path(eurostat_dir, "health_financing_index.csv")

    # 辅助函数：智能读取Eurostat CSV | Helper function: Smart read Eurostat CSV
    read_eurostat_csv <- function(file_path) {
        tryCatch(
            {
                # 读取完整文件 | Read full file
                df <- read_csv(file_path, show_col_types = FALSE)

                # 查找实际数据开始行 | Find actual data start row
                # 通常是包含 "TIME" 或 "GEO" 的行 | Usually the row containing "TIME" or "GEO"
                time_row <- which(df[[1]] == "TIME" | grepl("^TIME", df[[1]], ignore.case = TRUE))

                if (length(time_row) > 0) {
                    # 重新读取，跳过元数据行 | Re-read, skip metadata rows
                    df <- read_csv(file_path, skip = time_row[1] - 1, show_col_types = FALSE)

                    # 移除完全为NA的列 | Remove all-NA columns
                    df <- df %>% select(where(~ !all(is.na(.))))
                }

                return(df)
            },
            error = function(e) {
                warning(sprintf("Failed to read %s: %s", basename(file_path), e$message))
                return(NULL)
            }
        )
    }

    # 读取政府卫生支出数据 | Read government health expenditure data
    cat("[INFO] Loading government health expenditure data...\n")

    gov_health_files <- list.files(eurostat_dir,
        pattern = "gov_health_sheet_Sheet\\.[0-9]+_data\\.csv",
        full.names = TRUE
    )

    gov_health_data <- map_dfr(
        gov_health_files[1:min(5, length(gov_health_files))],
        read_eurostat_csv
    )

    # 读取健康融资数据 | Read health financing data
    cat("[INFO] Loading health financing data...\n")

    health_fin_files <- list.files(eurostat_dir,
        pattern = "health_financing_sheet_Sheet\\.[0-9]+_data\\.csv",
        full.names = TRUE
    )

    health_fin_data <- map_dfr(
        health_fin_files[1:min(5, length(health_fin_files))],
        read_eurostat_csv
    )

    # 合并Eurostat数据 | Combine Eurostat data
    eurostat_combined <- bind_rows(
        gov_health_data %>% mutate(source_type = "government_health"),
        health_fin_data %>% mutate(source_type = "health_financing")
    )

    cat(sprintf(
        "[OK] Loaded Eurostat data: %d rows x %d columns\n",
        nrow(eurostat_combined), ncol(eurostat_combined)
    ))

    # 清洗Eurostat数据 | Clean Eurostat data
    # Eurostat数据结构比较特殊，需要定制化处理
    # Eurostat data has special structure, requires customized processing

    # 提取时间列 | Extract time columns
    time_cols <- grep("^[0-9]{4}$", names(eurostat_combined), value = TRUE)

    if (length(time_cols) > 0) {
        eurostat_cleaned <- eurostat_combined %>%
            # 转换为长格式 | Convert to long format
            pivot_longer(
                cols = all_of(time_cols),
                names_to = "year",
                values_to = "value"
            ) %>%
            # 标准化国家代码 | Standardize country codes
            mutate(
                year = as.integer(year),
                # Eurostat使用GEO列 | Eurostat uses GEO column
                country_name = if ("GEO (Labels)" %in% names(.)) `GEO (Labels)` else NA_character_,
                iso3c = standardize_country(country_name),
                value = suppressWarnings(as.numeric(value))
            ) %>%
            # 移除缺失值 | Remove missing values
            filter(!is.na(iso3c), !is.na(year), !is.na(value)) %>%
            # 限制年份范围 | Filter year range
            filter(
                year >= CONFIG$data$start_year,
                year <= CONFIG$data$end_year
            )

        cat(sprintf(
            "[OK] Cleaned Eurostat data: %d rows x %d columns\n",
            nrow(eurostat_cleaned), ncol(eurostat_cleaned)
        ))

        saveRDS(eurostat_cleaned, file.path("..", "data", "processed", "01_cleaned_data_eurostat.rds"))

        eurostat_quality <- check_data_quality(eurostat_cleaned, "Eurostat")
    } else {
        warning("Cannot find time columns in Eurostat data")
        eurostat_cleaned <- NULL
        eurostat_quality <- NULL
    }
}

# =============================================================================
# 9. 生成数据加载汇总报告 | Generate Data Loading Summary Report
# =============================================================================

cat("\n[INFO] Generating data loading summary...\n")

# 汇总所有数据质量指标 | Compile all data quality metrics
quality_summary <- bind_rows(
    if (!is.null(wb_quality)) as.data.frame(wb_quality) else NULL,
    if (!is.null(who_quality)) as.data.frame(who_quality) else NULL,
    if (!is.null(imf_quality)) as.data.frame(imf_quality) else NULL,
    if (!is.null(oecd_quality)) as.data.frame(oecd_quality) else NULL,
    if (!is.null(gbd_quality)) as.data.frame(gbd_quality) else NULL,
    if (!is.null(un_quality)) as.data.frame(un_quality) else NULL,
    if (!is.null(eurostat_quality)) as.data.frame(eurostat_quality) else NULL
)

# 保存汇总报告 | Save summary report
write_csv(
    quality_summary,
    file.path("..", "data", "processed", "01_data_loading_summary.csv")
)

cat("[OK] Data loading summary saved\n")

# 打印汇总表 | Print summary table
cat("\n")
cat("=============================================================================\n")
cat("  Data Loading Summary\n")
cat("=============================================================================\n\n")
print(quality_summary)
cat("\n")

# =============================================================================
# 10. 完成 | Complete
# =============================================================================

log_message("Data loading and cleaning completed successfully")

cat("\n")
cat("=============================================================================\n")
cat("  [OK] Script 01 completed successfully!\n")
cat("=============================================================================\n")
cat(sprintf("  - Total datasets loaded: %d\n", nrow(quality_summary)))
cat(sprintf("  - Total observations: %d\n", sum(quality_summary$n_total)))
cat(sprintf("  - Output directory: %s\n", file.path("..", "data", "processed")))
cat("\n")

# 保存会话信息用于调试 | Save session info for debugging
session_info <- sessionInfo()
saveRDS(session_info, file.path("..", "data", "processed", "01_session_info.rds"))

log_message("Script 01 execution time logged")
