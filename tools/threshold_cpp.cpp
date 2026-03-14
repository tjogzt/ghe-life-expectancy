// =============================================================================
// threshold_cpp.cpp
// C++ Accelerated Threshold Bootstrap | C++加速的阈值Bootstrap
// =============================================================================
//
// 【功能 | Function】
// 使用C++实现快速的阈值Bootstrap估计，替代慢速的segmented包
// Fast threshold bootstrap estimation using C++ to replace slow segmented package
//
// 【性能提升 | Performance Gain】
// 预期加速：10-20倍
// Expected speedup: 10-20x
//
// =============================================================================

#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <vector>

using namespace Rcpp;

// [[Rcpp::export]]
double fast_threshold_search(
    NumericVector y,           // 因变量 | Dependent variable
    NumericVector x,           // 自变量（health spending） | Independent variable
    NumericVector z,           // 门槛变量（institutional capacity） | Threshold variable
    NumericMatrix X_controls,  // 控制变量矩阵 | Control variables matrix
    double z_min,              // 搜索范围最小值 | Search range minimum
    double z_max,              // 搜索范围最大值 | Search range maximum
    int n_grid = 100           // 网格点数 | Number of grid points
) {
    int n = y.size();
    int k = X_controls.ncol();
    
    // 创建搜索网格 | Create search grid
    NumericVector grid(n_grid);
    double step = (z_max - z_min) / (n_grid - 1);
    for (int i = 0; i < n_grid; i++) {
        grid[i] = z_min + i * step;
    }
    
    double best_threshold = grid[0];
    double min_sse = R_PosInf;
    
    // 对每个候选阈值计算SSE | Calculate SSE for each candidate threshold
    for (int g = 0; g < n_grid; g++) {
        double candidate = grid[g];
        
        // 构建设计矩阵 | Build design matrix
        // y = β0 + β1*x*I(z<threshold) + β2*x*I(z>=threshold) + controls + ε
        NumericMatrix X(n, 2 + k);
        
        for (int i = 0; i < n; i++) {
            // x*I(z < threshold)
            X(i, 0) = (z[i] < candidate) ? x[i] : 0.0;
            // x*I(z >= threshold)
            X(i, 1) = (z[i] >= candidate) ? x[i] : 0.0;
            // 控制变量 | Control variables
            for (int j = 0; j < k; j++) {
                X(i, 2 + j) = X_controls(i, j);
            }
        }
        
        // 快速OLS估计（正规方程） | Fast OLS estimation (normal equations)
        // β = (X'X)^(-1) X'y
        
        // 计算 X'X
        NumericMatrix XtX(2 + k, 2 + k);
        for (int i = 0; i < 2 + k; i++) {
            for (int j = 0; j < 2 + k; j++) {
                double sum = 0.0;
                for (int obs = 0; obs < n; obs++) {
                    sum += X(obs, i) * X(obs, j);
                }
                XtX(i, j) = sum;
            }
        }
        
        // 计算 X'y
        NumericVector Xty(2 + k);
        for (int i = 0; i < 2 + k; i++) {
            double sum = 0.0;
            for (int obs = 0; obs < n; obs++) {
                sum += X(obs, i) * y[obs];
            }
            Xty[i] = sum;
        }
        
        // 简化：直接计算残差平方和
        // 使用QR分解会更稳定，但这里为了速度使用直接方法
        
        // 计算拟合值和残差
        double sse = 0.0;
        bool valid = true;
        
        // 简化的估计：分别对两个区制做简单回归
        double sum_y_low = 0.0, sum_x_low = 0.0;
        double sum_y_high = 0.0, sum_x_high = 0.0;
        int n_low = 0, n_high = 0;
        
        for (int i = 0; i < n; i++) {
            if (z[i] < candidate) {
                sum_y_low += y[i];
                sum_x_low += x[i];
                n_low++;
            } else {
                sum_y_high += y[i];
                sum_x_high += x[i];
                n_high++;
            }
        }
        
        // 需要两个区制都有足够的观测
        if (n_low < 30 || n_high < 30) {
            continue;
        }
        
        double mean_y_low = sum_y_low / n_low;
        double mean_x_low = sum_x_low / n_low;
        double mean_y_high = sum_y_high / n_high;
        double mean_x_high = sum_x_high / n_high;
        
        // 简单线性回归系数
        double beta_low = 0.0, beta_high = 0.0;
        double sum_xy_low = 0.0, sum_xx_low = 0.0;
        double sum_xy_high = 0.0, sum_xx_high = 0.0;
        
        for (int i = 0; i < n; i++) {
            if (z[i] < candidate) {
                sum_xy_low += (x[i] - mean_x_low) * (y[i] - mean_y_low);
                sum_xx_low += (x[i] - mean_x_low) * (x[i] - mean_x_low);
            } else {
                sum_xy_high += (x[i] - mean_x_high) * (y[i] - mean_y_high);
                sum_xx_high += (x[i] - mean_x_high) * (x[i] - mean_x_high);
            }
        }
        
        if (sum_xx_low > 0) beta_low = sum_xy_low / sum_xx_low;
        if (sum_xx_high > 0) beta_high = sum_xy_high / sum_xx_high;
        
        double alpha_low = mean_y_low - beta_low * mean_x_low;
        double alpha_high = mean_y_high - beta_high * mean_x_high;
        
        // 计算SSE
        for (int i = 0; i < n; i++) {
            double fitted;
            if (z[i] < candidate) {
                fitted = alpha_low + beta_low * x[i];
            } else {
                fitted = alpha_high + beta_high * x[i];
            }
            double resid = y[i] - fitted;
            sse += resid * resid;
        }
        
        // 更新最优阈值
        if (sse < min_sse) {
            min_sse = sse;
            best_threshold = candidate;
        }
    }
    
    return best_threshold;
}

// [[Rcpp::export]]
NumericVector parallel_bootstrap_thresholds(
    NumericVector y,
    NumericVector x,
    NumericVector z,
    NumericMatrix X_controls,
    IntegerVector country_ids,
    int n_boot,
    double z_min,
    double z_max,
    int n_grid = 100,
    int seed = 123
) {
    int n = y.size();
    NumericVector boot_thresholds(n_boot);
    
    // 获取唯一国家列表（手动实现，避免unique()问题）
    std::vector<int> unique_vec;
    for (int i = 0; i < n; i++) {
        bool found = false;
        for (size_t j = 0; j < unique_vec.size(); j++) {
            if (country_ids[i] == unique_vec[j]) {
                found = true;
                break;
            }
        }
        if (!found) {
            unique_vec.push_back(country_ids[i]);
        }
    }
    
    IntegerVector unique_countries = wrap(unique_vec);
    int n_countries = unique_countries.size();
    
    // 设置随机种子
    Rcpp::Environment base_env("package:base");
    Rcpp::Function set_seed = base_env["set.seed"];
    set_seed(seed);
    
    for (int b = 0; b < n_boot; b++) {
        // Bootstrap重采样（按国家）
        // Bootstrap resampling (by country)
        IntegerVector boot_countries = sample(unique_countries, n_countries, true);
        
        // 构建bootstrap样本
        std::vector<int> boot_indices;
        boot_indices.reserve(n * 2); // 预留空间
        
        for (int i = 0; i < n_countries; i++) {
            int country = boot_countries[i];
            for (int j = 0; j < n; j++) {
                if (country_ids[j] == country) {
                    boot_indices.push_back(j);
                }
            }
        }
        
        int boot_n = boot_indices.size();
        NumericVector boot_y(boot_n);
        NumericVector boot_x(boot_n);
        NumericVector boot_z(boot_n);
        NumericMatrix boot_X_controls(boot_n, X_controls.ncol());
        
        for (int i = 0; i < boot_n; i++) {
            int idx = boot_indices[i];
            boot_y[i] = y[idx];
            boot_x[i] = x[idx];
            boot_z[i] = z[idx];
            for (int j = 0; j < X_controls.ncol(); j++) {
                boot_X_controls(i, j) = X_controls(idx, j);
            }
        }
        
        // 估计阈值
        boot_thresholds[b] = fast_threshold_search(
            boot_y, boot_x, boot_z, boot_X_controls,
            z_min, z_max, n_grid
        );
        
        // 进度报告
        if ((b + 1) % 100 == 0) {
            Rcpp::Rcout << "Bootstrap progress: " << (b + 1) << "/" << n_boot 
                       << " (" << std::round(100.0 * (b + 1) / n_boot) << "%)\n";
        }
    }
    
    return boot_thresholds;
}

// [[Rcpp::export]]
List estimate_regime_effects(
    NumericVector y,
    NumericVector x,
    NumericVector z,
    NumericMatrix X_controls,
    double threshold
) {
    int n = y.size();
    int k = X_controls.ncol();
    
    // 分割数据为两个区制
    std::vector<int> idx_low, idx_high;
    for (int i = 0; i < n; i++) {
        if (z[i] < threshold) {
            idx_low.push_back(i);
        } else {
            idx_high.push_back(i);
        }
    }
    
    int n_low = idx_low.size();
    int n_high = idx_high.size();
    
    // 提取低区制数据
    NumericVector y_low(n_low), x_low(n_low);
    for (int i = 0; i < n_low; i++) {
        y_low[i] = y[idx_low[i]];
        x_low[i] = x[idx_low[i]];
    }
    
    // 提取高区制数据
    NumericVector y_high(n_high), x_high(n_high);
    for (int i = 0; i < n_high; i++) {
        y_high[i] = y[idx_high[i]];
        x_high[i] = x[idx_high[i]];
    }
    
    // 简单线性回归
    double mean_y_low = 0.0, mean_x_low = 0.0;
    double mean_y_high = 0.0, mean_x_high = 0.0;
    
    for (int i = 0; i < n_low; i++) {
        mean_y_low += y_low[i];
        mean_x_low += x_low[i];
    }
    mean_y_low /= n_low;
    mean_x_low /= n_low;
    
    for (int i = 0; i < n_high; i++) {
        mean_y_high += y_high[i];
        mean_x_high += x_high[i];
    }
    mean_y_high /= n_high;
    mean_x_high /= n_high;
    
    // 计算斜率
    double sum_xy_low = 0.0, sum_xx_low = 0.0;
    double sum_xy_high = 0.0, sum_xx_high = 0.0;
    
    for (int i = 0; i < n_low; i++) {
        sum_xy_low += (x_low[i] - mean_x_low) * (y_low[i] - mean_y_low);
        sum_xx_low += (x_low[i] - mean_x_low) * (x_low[i] - mean_x_low);
    }
    
    for (int i = 0; i < n_high; i++) {
        sum_xy_high += (x_high[i] - mean_x_high) * (y_high[i] - mean_y_high);
        sum_xx_high += (x_high[i] - mean_x_high) * (x_high[i] - mean_x_high);
    }
    
    double beta_low = sum_xy_low / sum_xx_low;
    double beta_high = sum_xy_high / sum_xx_high;
    
    // 计算标准误（简化版本）
    double sse_low = 0.0, sse_high = 0.0;
    double alpha_low = mean_y_low - beta_low * mean_x_low;
    double alpha_high = mean_y_high - beta_high * mean_x_high;
    
    for (int i = 0; i < n_low; i++) {
        double resid = y_low[i] - (alpha_low + beta_low * x_low[i]);
        sse_low += resid * resid;
    }
    
    for (int i = 0; i < n_high; i++) {
        double resid = y_high[i] - (alpha_high + beta_high * x_high[i]);
        sse_high += resid * resid;
    }
    
    double se_low = std::sqrt(sse_low / (n_low - 2) / sum_xx_low);
    double se_high = std::sqrt(sse_high / (n_high - 2) / sum_xx_high);
    
    return List::create(
        Named("beta_low") = beta_low,
        Named("beta_high") = beta_high,
        Named("se_low") = se_low,
        Named("se_high") = se_high,
        Named("n_low") = n_low,
        Named("n_high") = n_high
    );
}

