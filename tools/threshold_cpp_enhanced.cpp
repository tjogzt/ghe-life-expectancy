// =============================================================================
// threshold_cpp_enhanced.cpp
// Enhanced C++ Threshold Analysis with Fixed Effects and Robust SE
// C++增强版阈值分析：含固定效应和稳健标准误
// =============================================================================
//
// 【功能 | Function】
// 完整实现面板数据阈值回归，包括：
// Full panel data threshold regression including:
// - 国家固定效应 | Country fixed effects
// - 时间固定效应 | Time fixed effects  
// - 稳健标准误 | Robust standard errors (HC1)
// - 快速网格搜索 | Fast grid search
//
// 【理论基础 | Theoretical Foundation】
// 基于Hansen (1999)阈值回归方法，适应面板数据结构
// Based on Hansen (1999) threshold regression for panel data
//
// =============================================================================

#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <vector>
#include <map>

using namespace Rcpp;

// =============================================================================
// 辅助函数：固定效应转换 | Helper: Fixed Effects Transformation
// =============================================================================

// 对数据进行within变换（去除固定效应）
// Within transformation to remove fixed effects
// [[Rcpp::export]]
List within_transformation(
    NumericVector y,
    NumericVector x,
    NumericVector z,
    IntegerVector country_ids,
    IntegerVector time_ids
) {
    int n = y.size();
    
    // 计算国家均值 | Calculate country means
    std::map<int, double> y_country_mean, x_country_mean, z_country_mean;
    std::map<int, int> country_count;
    
    for (int i = 0; i < n; i++) {
        int cid = country_ids[i];
        y_country_mean[cid] += y[i];
        x_country_mean[cid] += x[i];
        z_country_mean[cid] += z[i];
        country_count[cid]++;
    }
    
    for (auto& pair : y_country_mean) {
        int cid = pair.first;
        y_country_mean[cid] /= country_count[cid];
        x_country_mean[cid] /= country_count[cid];
        z_country_mean[cid] /= country_count[cid];
    }
    
    // 计算时间均值 | Calculate time means
    std::map<int, double> y_time_mean, x_time_mean, z_time_mean;
    std::map<int, int> time_count;
    
    for (int i = 0; i < n; i++) {
        int tid = time_ids[i];
        y_time_mean[tid] += y[i];
        x_time_mean[tid] += x[i];
        z_time_mean[tid] += z[i];
        time_count[tid]++;
    }
    
    for (auto& pair : y_time_mean) {
        int tid = pair.first;
        y_time_mean[tid] /= time_count[tid];
        x_time_mean[tid] /= time_count[tid];
        z_time_mean[tid] /= time_count[tid];
    }
    
    // 总均值 | Grand means
    double y_grand_mean = 0.0, x_grand_mean = 0.0, z_grand_mean = 0.0;
    for (int i = 0; i < n; i++) {
        y_grand_mean += y[i];
        x_grand_mean += x[i];
        z_grand_mean += z[i];
    }
    y_grand_mean /= n;
    x_grand_mean /= n;
    z_grand_mean /= n;
    
    // Within变换 | Within transformation
    // y_it - y_i_bar - y_t_bar + y_grand_mean
    NumericVector y_within(n), x_within(n), z_within(n);
    
    for (int i = 0; i < n; i++) {
        int cid = country_ids[i];
        int tid = time_ids[i];
        
        y_within[i] = y[i] - y_country_mean[cid] - y_time_mean[tid] + y_grand_mean;
        x_within[i] = x[i] - x_country_mean[cid] - x_time_mean[tid] + x_grand_mean;
        z_within[i] = z[i] - z_country_mean[cid] - z_time_mean[tid] + z_grand_mean;
    }
    
    return List::create(
        Named("y_within") = y_within,
        Named("x_within") = x_within,
        Named("z_within") = z_within,
        Named("y_country_mean") = wrap(y_country_mean),
        Named("x_country_mean") = wrap(x_country_mean),
        Named("n_countries") = country_count.size(),
        Named("n_times") = time_count.size()
    );
}

// =============================================================================
// 固定效应下的阈值搜索 | Threshold Search with Fixed Effects
// =============================================================================

// [[Rcpp::export]]
double threshold_search_fe(
    NumericVector y,
    NumericVector x,
    NumericVector z,
    IntegerVector country_ids,
    IntegerVector time_ids,
    NumericMatrix X_controls,
    double z_min,
    double z_max,
    int n_grid = 200
) {
    int n = y.size();
    int k = X_controls.ncol();
    
    // 先对数据进行within变换 | First apply within transformation
    List within_result = within_transformation(y, x, z, country_ids, time_ids);
    NumericVector y_w = within_result["y_within"];
    NumericVector x_w = within_result["x_within"];
    NumericVector z_w = within_result["z_within"];
    
    // 也对控制变量做within变换 | Also transform control variables
    NumericMatrix X_w(n, k);
    for (int j = 0; j < k; j++) {
        std::map<int, double> country_mean, time_mean;
        std::map<int, int> country_count, time_count;
        double grand_mean = 0.0;
        
        // 计算均值
        for (int i = 0; i < n; i++) {
            int cid = country_ids[i];
            int tid = time_ids[i];
            country_mean[cid] += X_controls(i, j);
            time_mean[tid] += X_controls(i, j);
            country_count[cid]++;
            time_count[tid]++;
            grand_mean += X_controls(i, j);
        }
        
        grand_mean /= n;
        for (auto& p : country_mean) country_mean[p.first] /= country_count[p.first];
        for (auto& p : time_mean) time_mean[p.first] /= time_count[p.first];
        
        // Within变换
        for (int i = 0; i < n; i++) {
            int cid = country_ids[i];
            int tid = time_ids[i];
            X_w(i, j) = X_controls(i, j) - country_mean[cid] - time_mean[tid] + grand_mean;
        }
    }
    
    // 创建搜索网格 | Create search grid
    NumericVector grid(n_grid);
    double step = (z_max - z_min) / (n_grid - 1);
    for (int i = 0; i < n_grid; i++) {
        grid[i] = z_min + i * step;
    }
    
    double best_threshold = grid[0];
    double min_ssr = R_PosInf;
    
    // 对每个候选阈值计算SSR | Calculate SSR for each candidate threshold
    for (int g = 0; g < n_grid; g++) {
        double candidate = grid[g];
        
        // 构建设计矩阵（使用原始z判断，但拟合用within变换的数据）
        // Build design matrix (use original z for thresholding, fit with within-transformed data)
        std::vector<int> idx_low, idx_high;
        for (int i = 0; i < n; i++) {
            if (z[i] < candidate) {
                idx_low.push_back(i);
            } else {
                idx_high.push_back(i);
            }
        }
        
        int n_low = idx_low.size();
        int n_high = idx_high.size();
        
        // 需要两个区制都有足够的观测
        if (n_low < 50 || n_high < 50) {
            continue;
        }
        
        // 分别对两个区制估计（使用within变换后的数据）
        // Estimate for each regime (using within-transformed data)
        double sum_y_low = 0.0, sum_x_low = 0.0;
        double sum_y_high = 0.0, sum_x_high = 0.0;
        
        for (int i : idx_low) {
            sum_y_low += y_w[i];
            sum_x_low += x_w[i];
        }
        
        for (int i : idx_high) {
            sum_y_high += y_w[i];
            sum_x_high += x_w[i];
        }
        
        double mean_y_low = sum_y_low / n_low;
        double mean_x_low = sum_x_low / n_low;
        double mean_y_high = sum_y_high / n_high;
        double mean_x_high = sum_x_high / n_high;
        
        // 简单线性回归系数
        double sum_xy_low = 0.0, sum_xx_low = 0.0;
        double sum_xy_high = 0.0, sum_xx_high = 0.0;
        
        for (int i : idx_low) {
            sum_xy_low += (x_w[i] - mean_x_low) * (y_w[i] - mean_y_low);
            sum_xx_low += (x_w[i] - mean_x_low) * (x_w[i] - mean_x_low);
        }
        
        for (int i : idx_high) {
            sum_xy_high += (x_w[i] - mean_x_high) * (y_w[i] - mean_y_high);
            sum_xx_high += (x_w[i] - mean_x_high) * (x_w[i] - mean_x_high);
        }
        
        double beta_low = (sum_xx_low > 1e-10) ? (sum_xy_low / sum_xx_low) : 0.0;
        double beta_high = (sum_xx_high > 1e-10) ? (sum_xy_high / sum_xx_high) : 0.0;
        
        double alpha_low = mean_y_low - beta_low * mean_x_low;
        double alpha_high = mean_y_high - beta_high * mean_x_high;
        
        // 计算SSR
        double ssr = 0.0;
        for (int i : idx_low) {
            double fitted = alpha_low + beta_low * x_w[i];
            double resid = y_w[i] - fitted;
            ssr += resid * resid;
        }
        
        for (int i : idx_high) {
            double fitted = alpha_high + beta_high * x_w[i];
            double resid = y_w[i] - fitted;
            ssr += resid * resid;
        }
        
        // 更新最优阈值
        if (ssr < min_ssr) {
            min_ssr = ssr;
            best_threshold = candidate;
        }
    }
    
    return best_threshold;
}

// =============================================================================
// 估计区制效应（含固定效应和稳健标准误）
// Estimate Regime Effects with FE and Robust SE
// =============================================================================

// [[Rcpp::export]]
List estimate_regime_effects_fe(
    NumericVector y,
    NumericVector x,
    NumericVector z,
    IntegerVector country_ids,
    IntegerVector time_ids,
    NumericMatrix X_controls,
    double threshold
) {
    int n = y.size();
    
    // Within变换 | Within transformation
    List within_result = within_transformation(y, x, z, country_ids, time_ids);
    NumericVector y_w = within_result["y_within"];
    NumericVector x_w = within_result["x_within"];
    
    // 分割数据为两个区制（使用原始z）
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
    
    // 对每个区制估计（使用within变换的数据）
    double sum_y_low = 0.0, sum_x_low = 0.0;
    double sum_y_high = 0.0, sum_x_high = 0.0;
    
    for (int i : idx_low) {
        sum_y_low += y_w[i];
        sum_x_low += x_w[i];
    }
    
    for (int i : idx_high) {
        sum_y_high += y_w[i];
        sum_x_high += x_w[i];
    }
    
    double mean_y_low = sum_y_low / n_low;
    double mean_x_low = sum_x_low / n_low;
    double mean_y_high = sum_y_high / n_high;
    double mean_x_high = sum_x_high / n_high;
    
    // 计算斜率
    double sum_xy_low = 0.0, sum_xx_low = 0.0;
    double sum_xy_high = 0.0, sum_xx_high = 0.0;
    
    for (int i : idx_low) {
        sum_xy_low += (x_w[i] - mean_x_low) * (y_w[i] - mean_y_low);
        sum_xx_low += (x_w[i] - mean_x_low) * (x_w[i] - mean_x_low);
    }
    
    for (int i : idx_high) {
        sum_xy_high += (x_w[i] - mean_x_high) * (y_w[i] - mean_y_high);
        sum_xx_high += (x_w[i] - mean_x_high) * (x_w[i] - mean_x_high);
    }
    
    double beta_low = sum_xy_low / sum_xx_low;
    double beta_high = sum_xy_high / sum_xx_high;
    
    // 计算残差和稳健标准误 (HC1) | Calculate residuals and robust SE
    double alpha_low = mean_y_low - beta_low * mean_x_low;
    double alpha_high = mean_y_high - beta_high * mean_x_high;
    
    // 低区制的残差平方和 | Low regime SSE
    NumericVector resid_low(n_low);
    double sse_low = 0.0;
    for (int j = 0; j < n_low; j++) {
        int i = idx_low[j];
        resid_low[j] = y_w[i] - (alpha_low + beta_low * x_w[i]);
        sse_low += resid_low[j] * resid_low[j];
    }
    
    // 高区制的残差平方和 | High regime SSE
    NumericVector resid_high(n_high);
    double sse_high = 0.0;
    for (int j = 0; j < n_high; j++) {
        int i = idx_high[j];
        resid_high[j] = y_w[i] - (alpha_high + beta_high * x_w[i]);
        sse_high += resid_high[j] * resid_high[j];
    }
    
    // HC1稳健标准误 | HC1 robust standard errors
    // SE = sqrt((n/(n-k)) * sum(e_i^2 * x_i^2) / sum(x_i^2)^2)
    
    double hc1_factor_low = static_cast<double>(n_low) / (n_low - 2);
    double hc1_factor_high = static_cast<double>(n_high) / (n_high - 2);
    
    double sum_resid2_x2_low = 0.0;
    for (int j = 0; j < n_low; j++) {
        int i = idx_low[j];
        double x_dev = x_w[i] - mean_x_low;
        sum_resid2_x2_low += resid_low[j] * resid_low[j] * x_dev * x_dev;
    }
    
    double sum_resid2_x2_high = 0.0;
    for (int j = 0; j < n_high; j++) {
        int i = idx_high[j];
        double x_dev = x_w[i] - mean_x_high;
        sum_resid2_x2_high += resid_high[j] * resid_high[j] * x_dev * x_dev;
    }
    
    double se_low = std::sqrt(hc1_factor_low * sum_resid2_x2_low) / sum_xx_low;
    double se_high = std::sqrt(hc1_factor_high * sum_resid2_x2_high) / sum_xx_high;
    
    // 返回结果 | Return results
    return List::create(
        Named("beta_low") = beta_low,
        Named("beta_high") = beta_high,
        Named("se_low") = se_low,
        Named("se_high") = se_high,
        Named("n_low") = n_low,
        Named("n_high") = n_high,
        Named("sse_low") = sse_low,
        Named("sse_high") = sse_high
    );
}

// =============================================================================
// Bootstrap（使用增强方法）| Bootstrap with Enhanced Method
// =============================================================================

// [[Rcpp::export]]
NumericVector bootstrap_thresholds_fe(
    NumericVector y,
    NumericVector x,
    NumericVector z,
    IntegerVector country_ids,
    IntegerVector time_ids,
    NumericMatrix X_controls,
    int n_boot,
    double z_min,
    double z_max,
    int n_grid,
    int seed
) {
    // 注意：实际的Bootstrap重采样在R端进行
    // 这个函数为单次Bootstrap迭代服务
    // Note: Actual bootstrap resampling done in R
    // This function serves single bootstrap iteration
    
    NumericVector boot_thresholds(n_boot);
    
    // 这里只返回一个占位符
    // Just return placeholder
    // 实际使用时，R端会循环调用threshold_search_fe
    
    return boot_thresholds;
}

