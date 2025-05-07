# 加载必要的包
library(readxl)
library(lavaan)
library(dplyr)
library(flextable)
library(officer)

# 读取 Excel 文件，去掉第一行
data <- read_excel("C:/Users/阿龟/Desktop/父母抑郁与子女抑郁/250108返修/lgm/重要变量提取.xlsx", skip = 1)
names(data) <- c("C1","C2","C3","F1","F2","F3","M1","M2","M3",
                 "con1","con2","con3","P_mar","F_edu","M_edu")

# 定义三个模型
models <- list(
  child = '
    # Growth curves
    i1 =~ 1*C1 + 1*C2 + 1*C3
    s1 =~ 0*C1 + 1*C2 + 2*C3
    
    i2 =~ 1*con1 + 1*con2 + 1*con3
    s2 =~ 0*con1 + 1*con2 + 2*con3
    
    # Direct effects of conflict on depression
    C1 ~~ con1
    C2 ~~ con2
    C3 ~~ con3
    
    # Effects on growth factors
    i1 ~ P_mar + F_edu + M_edu
    s1 ~ P_mar + F_edu + M_edu
    i2 ~ P_mar + F_edu + M_edu
    s2 ~ P_mar + F_edu + M_edu
  ',
  
  father = '
    # Growth curves
    i1 =~ 1*F1 + 1*F2 + 1*F3
    s1 =~ 0*F1 + 1*F2 + 2*F3
    
    i2 =~ 1*con1 + 1*con2 + 1*con3
    s2 =~ 0*con1 + 1*con2 + 2*con3
    
    # Direct effects of conflict on depression
    F1 ~~ con1
    F2 ~~ con2
    F3 ~~ con3
    
    # Effects on growth factors
    i1 ~ P_mar + F_edu + M_edu
    s1 ~ P_mar + F_edu + M_edu
    i2 ~ P_mar + F_edu + M_edu
    s2 ~ P_mar + F_edu + M_edu
  ',
  
  mother = '
    # Growth curves
    i1 =~ 1*M1 + 1*M2 + 1*M3
    s1 =~ 0*M1 + 1*M2 + 2*M3
    
    i2 =~ 1*con1 + 1*con2 + 1*con3
    s2 =~ 0*con1 + 1*con2 + 2*con3
    
    # Direct effects of conflict on depression
    M1 ~~ con1
    M2 ~~ con2
    M3 ~~ con3
    
    # Effects on growth factors
    i1 ~ P_mar + F_edu + M_edu
    s1 ~ P_mar + F_edu + M_edu
    i2 ~ P_mar + F_edu + M_edu
    s2 ~ P_mar + F_edu + M_edu
  '
)

# 生成模型结果的函数
generate_model_results <- function(fit, model_name) {
  # 提取拟合指标
  fit_indices <- fitMeasures(fit, 
                             c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
  
  # 创建拟合指标表格
  fit_table <- data.frame(
    指标 = c("χ²", "df", "p", "CFI", "TLI", "RMSEA", "SRMR"),
    数值 = sprintf("%.3f", fit_indices)
  )
  
  # 提取参数估计
  params <- parameterEstimates(fit)
  
  # 添加显著性标记
  params$significance <- ifelse(params$pvalue < 0.001, "***",
                                ifelse(params$pvalue < 0.01, "**",
                                       ifelse(params$pvalue < 0.05, "*", "")))
  
  # 创建所有参数的参数估计表格
  param_table <- params %>%
    dplyr::select(lhs, op, rhs, est, se, pvalue, significance) %>%
    mutate(
      est = sprintf("%.3f", est),
      se = sprintf("%.3f", se),
      pvalue = ifelse(is.na(pvalue), "NA", sprintf("%.3f", pvalue))
    ) %>%
    rename(
      参数 = lhs,
      关系 = op,
      变量 = rhs,
      估计值 = est,
      标准误 = se,
      p值 = pvalue,
      显著性 = significance
    ) %>%
    flextable() %>%
    autofit() %>%
    align(align = "center", part = "all")
  
  # **新增部分：提取 i1、s1、i2、s2 的总体参数估计（均值和方差）**
  latent_params <- params %>%
    filter(lhs %in% c("i1", "s1", "i2", "s2") & (op == "~1" | (op == "~~" & lhs == rhs))) %>%
    dplyr::select(lhs, op, est, se, pvalue, significance) %>%
    mutate(
      est = sprintf("%.3f", est),
      se = sprintf("%.3f", se),
      pvalue = ifelse(is.na(pvalue), "NA", sprintf("%.3f", pvalue))
    ) %>%
    rename(
      潜在变量 = lhs,
      关系 = op,
      估计值 = est,
      标准误 = se,
      p值 = pvalue,
      显著性 = significance
    )
  
  latent_param_table <- latent_params %>%
    flextable() %>%
    autofit() %>%
    align(align = "center", part = "all")
  
  return(list(
    fit_table = fit_table,
    param_table = param_table,
    latent_param_table = latent_param_table  # 返回新增的表格
  ))
}

# 创建一个新的 Word 文档
doc <- read_docx()

# 在创建文档时，为每个模型结果后添加显著性说明
doc <- body_add_par(doc, "注：* p < 0.05, ** p < 0.01, *** p < 0.001", style = "Normal")

# 添加卡方分析函数
analyze_categorical <- function(data) {
  # 将教育程度和婚姻质量转换为因子
  data$F_edu <- factor(data$F_edu)
  data$M_edu <- factor(data$M_edu)
  data$P_mar <- factor(data$P_mar)
  
  # 父母教育程度交叉表
  edu_table <- table(data$F_edu, data$M_edu)
  edu_chi <- chisq.test(edu_table)
  
  # 父亲教育与婚姻质量交叉表
  f_mar_table <- table(data$F_edu, data$P_mar)
  f_mar_chi <- chisq.test(f_mar_table)
  
  # 母亲教育与婚姻质量交叉表
  m_mar_table <- table(data$M_edu, data$P_mar)
  m_mar_chi <- chisq.test(m_mar_table)
  
  # 为每个检验结果添加显著性标记
  add_significance <- function(p_value) {
    if(p_value < 0.001) return("***")
    if(p_value < 0.01) return("**")
    if(p_value < 0.05) return("*")
    return("")
  }
  
  return(list(
    edu_table = edu_table,
    edu_chi = list(
      statistic = edu_chi$statistic,
      df = edu_chi$parameter,
      p_value = edu_chi$p.value,
      significance = add_significance(edu_chi$p.value)
    ),
    f_mar_table = f_mar_table,
    f_mar_chi = list(
      statistic = f_mar_chi$statistic,
      df = f_mar_chi$parameter,
      p_value = f_mar_chi$p.value,
      significance = add_significance(f_mar_chi$p.value)
    ),
    m_mar_table = m_mar_table,
    m_mar_chi = list(
      statistic = m_mar_chi$statistic,
      df = m_mar_chi$parameter,
      p_value = m_mar_chi$p.value,
      significance = add_significance(m_mar_chi$p.value)
    )
  ))
}

# 在文档中添加潜在增长模型分析结果的标题
doc <- body_add_par(doc, "潜在增长模型分析结果", style = "heading 1")

# 循环处理每个模型
for(model_name in names(models)) {
  # 添加模型标题
  model_titles <- list(
    child = "1. 子女抑郁模型",
    father = "2. 父亲抑郁模型",
    mother = "3. 母亲抑郁模型"
  )
  
  doc <- body_add_par(doc, model_titles[[model_name]], style = "heading 1")
  
  # 拟合模型
  fit <- growth(models[[model_name]], data = data)
  
  # 添加拟合指标小节
  doc <- body_add_par(doc, "2.1 模型拟合指标", style = "heading 2")
  results <- generate_model_results(fit, model_name)
  
  # 创建并添加拟合指标表格
  fit_ft <- flextable(results$fit_table) %>%
    autofit() %>%
    align(align = "center", part = "all")
  doc <- body_add_flextable(doc, fit_ft)
  
  # 添加参数估计表格
  doc <- body_add_par(doc, "2.2 参数估计", style = "heading 2")
  doc <- body_add_flextable(doc, results$param_table)
  
  # **新增部分：添加主要增长因子参数估计表格**
  doc <- body_add_par(doc, "2.3 主要增长因子参数估计", style = "heading 2")
  doc <- body_add_flextable(doc, results$latent_param_table)
  
  # 添加显著性说明
  doc <- body_add_par(doc, "\n注：* p < 0.05, ** p < 0.01, *** p < 0.001", style = "Normal")
  
  # 添加空行分隔
  doc <- body_add_par(doc, "")
}

# 运行卡方分析
cat_results <- analyze_categorical(data)

# 添加协变量关联分析结果
doc <- body_add_par(doc, "协变量关联分析", style = "heading 1")

# 1. 父母教育程度关联
doc <- body_add_par(doc, "1. 父母教育程度交叉分析", style = "heading 2")
edu_cross_table <- as.data.frame.matrix(cat_results$edu_table)
doc <- body_add_flextable(doc, flextable(edu_cross_table))
edu_result <- data.frame(
  项目 = c("卡方值", "自由度", "p值"),
  数值 = sprintf("%s %s",
               c(
                 round(cat_results$edu_chi$statistic, 3),
                 cat_results$edu_chi$df,
                 round(cat_results$edu_chi$p_value, 3)
               ),
               c(cat_results$edu_chi$significance, "", cat_results$edu_chi$significance)
  )
)
doc <- body_add_flextable(doc, flextable(edu_result))

# 2. 父亲教育与婚姻质量关联
doc <- body_add_par(doc, "2. 父亲教育程度与婚姻质量交叉分析", style = "heading 2")
f_mar_cross_table <- as.data.frame.matrix(cat_results$f_mar_table)
doc <- body_add_flextable(doc, flextable(f_mar_cross_table))
f_mar_result <- data.frame(
  项目 = c("卡方值", "自由度", "p值"),
  数值 = sprintf("%s %s",
               c(
                 round(cat_results$f_mar_chi$statistic, 3),
                 cat_results$f_mar_chi$df,
                 round(cat_results$f_mar_chi$p_value, 3)
               ),
               c(cat_results$f_mar_chi$significance, "", cat_results$f_mar_chi$significance)
  )
)
doc <- body_add_flextable(doc, flextable(f_mar_result))

# 3. 母亲教育与婚姻质量关联
doc <- body_add_par(doc, "3. 母亲教育程度与婚姻质量交叉分析", style = "heading 2")
m_mar_cross_table <- as.data.frame.matrix(cat_results$m_mar_table)
doc <- body_add_flextable(doc, flextable(m_mar_cross_table))
m_mar_result <- data.frame(
  项目 = c("卡方值", "自由度", "p值"),
  数值 = sprintf("%s %s",
               c(
                 round(cat_results$m_mar_chi$statistic, 3),
                 cat_results$m_mar_chi$df,
                 round(cat_results$m_mar_chi$p_value, 3)
               ),
               c(cat_results$m_mar_chi$significance, "", cat_results$m_mar_chi$significance)
  )
)
doc <- body_add_flextable(doc, flextable(m_mar_result))

# 添加相关性分析函数
correlation_analysis <- function(data) {
  # 提取协变量并确保为数值型
  covariates <- data.frame(
    F_edu = as.numeric(data$F_edu),
    M_edu = as.numeric(data$M_edu),
    P_mar = as.numeric(data$P_mar)
  )
  
  # 计算Spearman相关系数矩阵
  cor_matrix <- cor(covariates, method = "spearman")
  
  # 计算p值矩阵，使用渐进p值
  n <- nrow(covariates)
  p_matrix <- matrix(NA, 3, 3)
  for(i in 1:3) {
    for(j in 1:3) {
      if(i != j) {
        test_result <- cor.test(covariates[,i], covariates[,j], 
                                method = "spearman",
                                exact = FALSE) # 使用渐进p值
        p_matrix[i,j] <- test_result$p.value
      }
    }
  }
  
  # 添加显著性标记
  add_significance <- function(p_value) {
    if(is.na(p_value)) return("")
    if(p_value < 0.001) return("***")
    if(p_value < 0.01) return("**")
    if(p_value < 0.05) return("*")
    return("")
  }
  
  # 创建结果矩阵
  result_matrix <- matrix("", 3, 3)
  for(i in 1:3) {
    for(j in 1:3) {
      if(i != j) {
        result_matrix[i,j] <- sprintf("%.3f%s", 
                                      cor_matrix[i,j],
                                      add_significance(p_matrix[i,j]))
      } else {
        result_matrix[i,j] <- "1.000"
      }
    }
  }
  
  # 转换为数据框
  result_df <- as.data.frame(result_matrix)
  names(result_df) <- c("父亲教育程度", "母亲教育程度", "婚姻质量")
  result_df <- cbind(变量 = c("父亲教育程度", "母亲教育程度", "婚姻质量"), result_df)
  
  return(result_df)
}

# 进行相关性分析
cor_results <- correlation_analysis(data)

# 在Word文档中添加相关性分析结果
doc <- body_add_par(doc, "4. 协变量相关性分析", style = "heading 2")
doc <- body_add_par(doc, "使用Spearman相关系数进行分析，因为教育程度和婚姻质量均为有序分类变量。", 
                    style = "Normal")

# 创建并添加相关性矩阵表格
cor_ft <- flextable(cor_results) %>%
  autofit() %>%
  align(align = "center", part = "all")

doc <- body_add_flextable(doc, cor_ft)
doc <- body_add_par(doc, "注：* p < 0.05, ** p < 0.01, *** p < 0.001", style = "Normal")

# 保存文档
print(doc, target = "C:/Users/阿龟/Desktop/父母抑郁与子女抑郁/250108返修/lgm/model_results.docx")
