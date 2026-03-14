# 地形崎岖度数据说明
# Terrain Ruggedness Data Documentation

**生成时间:** 2025-11-06 23:39:27.600293
**数据源:** geodata包 + WorldClim海拔数据
**国家数量:** 178

## 变量说明

1. **iso3c**: ISO 3166-1 alpha-3 国家代码
2. **mean_elevation**: 平均海拔 (米)
3. **sd_elevation**: 海拔标准差 (米)
4. **min_elevation**: 最低海拔 (米)
5. **max_elevation**: 最高海拔 (米)
6. **range_elevation**: 海拔范围 (米)
7. **cv_elevation**: 变异系数 (CV = SD/Mean) - **主要崎岖度指标**
8. **ruggedness_index**: 崎岖度指数 (标准差)
9. **terrain_heterogeneity**: 地形异质性 (Range/Mean)

## 核心指标

**cv_elevation (变异系数)** 是本研究的主要地形崎岖度指标：
- 取值范围: 0.102 - 1.995
- 平均值: 0.795
- 中位数: 0.746

**解释:**
- CV > 1.0: 地形非常崎岖
- 0.5 < CV < 1.0: 地形中等崎岖
- CV < 0.5: 地形相对平坦

## 数据质量

- 成功率: 90.4% (178/197 个国家)
- 分辨率: 10分 (约18公里)
- 数据来源: WorldClim 2.1

## 作为工具变量

变异系数 (cv_elevation) 可以作为地理工具变量：
1. 时间不变（地形不随时间变化）
2. 外生性（地形不受健康支出影响）
3. 相关性（地形影响经济发展和政府能力）
4. 排他性（通过经济发展间接影响健康）

## 参考文献

- Nunn, N., & Puga, D. (2012). Ruggedness: The blessing of bad geography in Africa. Review of Economics and Statistics, 94(1), 20-36.
- WorldClim 2.1 Global Climate Database

---
生成时间: 2025-11-06 23:39:27.600354

