# 定义需要的包
required_packages <- c("ggplot2", "dplyr", "gghalves", "readr", "ggsignif", 
                       "ggpubr", "PupillometryR", "tidyr", "car", "emmeans",
                       "multcomp", "stats")

# 检查并安装/加载包
for(package in required_packages) {
  if(!require(package, character.only = TRUE)) {
    cat("Installing", package, "...\n")
    install.packages(package)
    if(!require(package, character.only = TRUE)) {
      stop("Package", package, "failed to install/load")
    }
  } else {
    cat(package, "is already loaded\n")
  }
}

# 读取数据并设置基本参数
library(tidyverse)
library(effectsize)
library(rstatix)  # 用于更好的统计分析输出
library(ggpubr)   # 用于创建出版质量的图形



# 读取数据
df <- read_csv("C:/Users/阿龟/Desktop/父母抑郁与子女抑郁/250108返修/描述性统计的图/cesd&conflict.csv")
names(df) <- c("cesd16","cesd18","cesd20","conflict16","conflict18","conflict20")

# 转换为长数据格式
df_long <- data.frame(
  id = rep(1:nrow(df), 3),
  time = rep(c("2016", "2018", "2020"), each = nrow(df)),
  cesd = c(df$cesd16, df$cesd18, df$cesd20),
  conflict = c(df$conflict16, df$conflict18, df$conflict20)
)

# 转换time为因子
df_long$time <- factor(df_long$time, levels = c("2016", "2018", "2020"))

# 描述性统计
desc_stats <- df_long %>%
  group_by(time) %>%
  summarise(
    n = n(),
    mean = round(mean(cesd, na.rm = TRUE), 2),
    sd = round(sd(cesd, na.rm = TRUE), 2),
    median = round(median(cesd, na.rm = TRUE), 2),
    iqr = round(IQR(cesd, na.rm = TRUE), 2),
    min = round(min(cesd, na.rm = TRUE), 2),
    max = round(max(cesd, na.rm = TRUE), 2)
  )
print(desc_stats)

# 正态性检验（使用随机抽样）
set.seed(123)
sample_size <- 5000
sample_indices <- sample(1:nrow(df_long), sample_size)
shapiro_result <- shapiro.test(df_long$cesd[sample_indices])

# QQ图
qq_plot <- ggplot(df_long, aes(sample = cesd)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal() +
  labs(title = "Normal Q-Q Plot of CESD Scores")
print(qq_plot)

# Friedman检验
friedman_result <- friedman.test(cbind(df$cesd16, df$cesd18, df$cesd20))

# Wilcoxon符号秩检验（使用Bonferroni校正）
wilcox_results <- list(
  "2016-2018" = wilcox.test(df$cesd16, df$cesd18, paired = TRUE),
  "2016-2020" = wilcox.test(df$cesd16, df$cesd20, paired = TRUE),
  "2018-2020" = wilcox.test(df$cesd18, df$cesd20, paired = TRUE)
)

# 计算效应量
wilcox_effect_size <- function(x, y) {
  w_test <- wilcox.test(x, y, paired = TRUE)
  z <- qnorm(w_test$p.value/2)
  r <- abs(z/sqrt(length(x)))
  return(r)
}

effect_sizes <- list(
  "2016-2018" = wilcox_effect_size(df$cesd16, df$cesd18),
  "2016-2020" = wilcox_effect_size(df$cesd16, df$cesd20),
  "2018-2020" = wilcox_effect_size(df$cesd18, df$cesd20)
)

# 创建箱线图
box_plot <- ggplot(df_long, aes(x = time, y = cesd)) +
  geom_boxplot(fill = "lightblue", alpha = 0.5) +
  theme_minimal() +
  labs(title = "CESD Scores Distribution Across Time",
       x = "Year",
       y = "CESD Score")
print(box_plot)

# 输出完整的统计结果报告
cat("\n=== Statistical Analysis Report ===\n")
cat("\n1. Descriptive Statistics:\n")
print(desc_stats)

cat("\n2. Normality Test (Shapiro-Wilk on random sample):\n")
cat("W =", shapiro_result$statistic, ", p =", shapiro_result$p.value, "\n")

cat("\n3. Friedman Test Results:\n")
print(friedman_result)

cat("\n4. Wilcoxon Signed-Rank Tests (with Bonferroni correction, α = 0.0167):\n")
for(comparison in names(wilcox_results)) {
  cat("\n", comparison, ":\n")
  cat("p-value =", wilcox_results[[comparison]]$p.value)
  cat("\nEffect size r =", effect_sizes[[comparison]])
  cat("\nEffect size interpretation: ", 
      ifelse(effect_sizes[[comparison]] < 0.3, "small",
             ifelse(effect_sizes[[comparison]] < 0.5, "medium", "large")),
      "\n")
}

# 保存图形

##处理数据
mean16 <- mean(df$cesd16, na.rm = TRUE)
mean18 <- mean(df$cesd18, na.rm = TRUE)
mean20 <- mean(df$cesd20, na.rm = TRUE)
conflict16 <- mean(df$conflict16,na.rm = TRUE)
conflict18 <- mean(df$conflict18, na.rm = TRUE)
conflict20 <- mean(df$conflict20, na.rm = TRUE)
# 16年数据处理
df <- mutate(df, conflict16_0 = ifelse(conflict16 == 0, cesd16, NA))
df <- mutate(df, conflict16_1 = ifelse(conflict16 == 1 | conflict16 == 2| conflict16 == 3, cesd16, NA))
df <- mutate(df, conflict16_2 = ifelse(conflict16 == 4 | conflict16 == 5| conflict16 == 6, cesd16, NA))

mean16_0 <- mean(df$conflict16_0, na.rm = TRUE)
mean16_1 <- mean(df$conflict16_1, na.rm = TRUE)
mean16_2 <- mean(df$conflict16_2, na.rm = TRUE)

# 18年数据处理
df <- mutate(df, conflict18_0 = ifelse(conflict18 == 0, cesd18, NA))
df <- mutate(df, conflict18_1 = ifelse(conflict18 == 1 | conflict18 == 2| conflict18 == 3, cesd18, NA))
df <- mutate(df, conflict18_2 = ifelse(conflict18 == 4 | conflict18 == 5| conflict18 == 6, cesd18, NA))

mean18_0 <- mean(df$conflict18_0, na.rm = TRUE)
mean18_1 <- mean(df$conflict18_1, na.rm = TRUE)
mean18_2 <- mean(df$conflict18_2, na.rm = TRUE)

# 20年数据处理
df <- mutate(df, conflict20_0 = ifelse(conflict20 == 0, cesd20, NA))
df <- mutate(df, conflict20_1 = ifelse(conflict20 == 1 | conflict20 == 2| conflict20 == 3, cesd20, NA))
df <- mutate(df, conflict20_2 = ifelse(conflict20 == 4 | conflict20 == 5| conflict20 == 6, cesd20, NA))


mean20_0 <- mean(df$conflict20_0, na.rm = TRUE)
mean20_1 <- mean(df$conflict20_1, na.rm = TRUE)
mean20_2 <- mean(df$conflict20_2, na.rm = TRUE)

# 存入向量
conflict0 <- matrix(c(mean16_0, mean18_0, mean20_0))
conflict1 <- matrix(c(mean16_1, mean18_1, mean20_1))
conflict2 <- matrix(c(mean16_2, mean18_2, mean20_2))

#加入分类变量
df <- df %>% 
  mutate(conflict16_type = ifelse(conflict16 == 0, "No-conflict groups",
                                  ifelse(conflict16 >= 1 & conflict16 <= 3, "Medium-conflict groups",
                                         "Strong-conflict groups")))
df <- df %>% 
  mutate(conflict18_type = ifelse(conflict18 == 0, "No-conflict groups",
                                  ifelse(conflict18 >= 1 & conflict18 <= 3, "Medium-conflict groups",
                                         "Strong-conflict groups")))
df <- df %>% 
  mutate(conflict20_type = ifelse(conflict20 == 0, "No-conflict groups",
                                  ifelse(conflict20 >= 1 & conflict20 <= 3, "Medium-conflict groups",
                                         "Strong-conflict groups")))

#计算标准差
sd16_0 <- sd(df$conflict16_0, na.rm = TRUE)/sqrt(sum(!is.na(df$conflict16_0)))
sd16_1 <- sd(df$conflict16_1, na.rm = TRUE)/sqrt(sum(!is.na(df$conflict16_1)))
sd16_2 <- sd(df$conflict16_2, na.rm = TRUE)/sqrt(sum(!is.na(df$conflict16_2)))

sd18_0 <- sd(df$conflict18_0, na.rm = TRUE)/sqrt(sum(!is.na(df$conflict18_0)))
sd18_1 <- sd(df$conflict18_1, na.rm = TRUE)/sqrt(sum(!is.na(df$conflict18_1)))
sd18_2 <- sd(df$conflict18_2, na.rm = TRUE)/sqrt(sum(!is.na(df$conflict18_2)))

sd20_0 <- sd(df$conflict20_0, na.rm = TRUE)/sqrt(sum(!is.na(df$conflict20_0)))
sd20_1 <- sd(df$conflict20_1, na.rm = TRUE)/sqrt(sum(!is.na(df$conflict20_1)))
sd20_2 <- sd(df$conflict20_2, na.rm = TRUE)/sqrt(sum(!is.na(df$conflict20_2)))

# time_points <- c(1,2,3)
result <-data.frame(conflict=matrix(c(conflict0,conflict1,conflict2)))
result <- mutate(result,standard_deviations=(c(sd16_0, sd18_0, sd20_0,sd16_1, sd18_1, sd20_1,sd16_2, sd18_2, sd20_2)))
result <- mutate(result,type=(c("No-conflict groups", "No-conflict groups", "No-conflict groups","Medium-conflict groups", "Medium-conflict groups", "Medium-conflict groups","Strong-conflict groups", "Strong-conflict groups", "Strong-conflict groups")))
result <- mutate(result,year=(c(2016,2018,2020,2016,2018,2020,2016,2018,2020)))

##画图
pd <- position_dodge(0.1)

ggplot(result, aes(x = year, y = conflict, colour = type)) + 
  geom_errorbar(aes(ymin = conflict - standard_deviations, ymax = conflict + standard_deviations), width = 0.1, position = pd) +
  geom_line(position = position_dodge(width = 0.1)) +
  geom_point(position = position_dodge(width = 0.1))
##偏差折线图
# 创建带有偏差线图的 ggplot 对象
pd <- position_dodge(0.1)

p <- ggplot(result, aes(x = year, y = conflict, colour = type, group = type)) + 
  geom_errorbar(aes(ymin = conflict - standard_deviations, ymax = conflict + standard_deviations), 
                colour = "black", width = 0.1, position = position_dodge(width = 0.2)) +
  geom_line(size = 1,position = position_dodge(width = 0.2)) +
  geom_point(position = position_dodge(width = 0.2), size = 3, shape = 21, fill = "white") + 
  xlab("Year") +
  ylab("CESD-A scores") +
  scale_colour_manual(name = "Group types",
                      breaks = c("No-conflict groups", "Medium-conflict groups", "Strong-conflict groups"),
                      labels = c("No-conflict ", "Medium-conflict ", "High-conflict "),
                      values = c("#72A9D0","#90C7C2","#E07B54")) + 
  ggtitle("CESD-A scores for different conflict groups in three waves") +
  expand_limits(y = 6.3, x = c(2016, 2021)) +
  scale_y_continuous(breaks = seq(0, 80, by =0.5)) +
  scale_x_continuous(breaks = seq(2016, 2020, by = 2)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.99, 0.02),
        plot.title = element_text(hjust = 0.5, family = "serif", face = "bold"),
        text = element_text(family = "serif"),
        axis.text = element_text(family = "serif"),
        legend.text = element_text(family = "serif"))

p
# 保存图形为 PNG 文件，指定完整路径
ggsave("C:/Users/阿龟/Desktop/父母抑郁与子女抑郁/250108返修/0111图片整理/偏差折线图.png", 
       plot = p, width = 7, height = 4.67, dpi = 1500)



##云雨图

# 设定你想要的新的x轴标签
new_labels <- c("High-conflict", "Medium-conflict", "No-conflict")

#2016年
# 绘制小提琴图
# 定义新的标签（移除"groups"）

order_vec <- c("Strong-conflict groups", "Medium-conflict groups", "No-conflict groups")
df$conflict16_type <- factor(df$conflict16_type, levels = order_vec)
p <- ggplot(df, aes(x=conflict16_type,
                    y=cesd16,
                    fill=conflict16_type,
                    color=conflict16_type)) 
p

mycolor <- c("#72A9D0","#90C7C2","#E07B54") 
p0 <- p + scale_color_manual(values=rev(mycolor)) +
  scale_fill_manual(values=rev(mycolor))+scale_x_discrete(labels=new_labels)  # 更改x轴标签
p0

#先画一半小提琴图(geom_half_violin)，得到p1：
p1<-p0+geom_half_violin()
p1

#一半小提琴图的参数调整：
#position：位置调整，这里将其向右水平移动0.1；
#side：显示哪一侧， "I"代表左侧，"R"代表右侧，默认"I"；
#adjust：调整带宽，这里设为1.2使宽带略变平滑；
#trim：小提琴图尾部的数据修整，默认为"T",表示将尾部修整到数据范围；"F"表示不修剪尾部；
# 一半小提琴图的参数调整
p2 <- p + geom_half_violin(position = position_nudge(x = 0.1, y = 0),
                           side = 'R',adjust=1, trim = F, color = NA, alpha = 0.8)
p2
# 显示绘制的小提琴图
#在半个小提琴图左侧加上散点图，得到p3：
p3<- p2+geom_point()+scale_x_discrete(labels=new_labels)  # 更改x轴标签
p3
#调整散点，得到p4：
# 检查和处理缺失值
# 将字符变量转换为数值型变量
df$conflict16_type_numeric <- ifelse(df$conflict16_type == "No-conflict groups",3,
                                     ifelse(df$conflict16_type == "Medium-conflict groups",2,1))
# 重新绘制散点图
p4 <- p2 + geom_point(data = df,aes(x = conflict16_type_numeric - 0.1, y = cesd16, color = conflict16_type),
                      position = position_jitter(width = 0.03), size = 0.2, shape = 20)

# 显示绘制的散点图
p4

#在p4散点和二分之一小提琴图中间添加箱线图，得到p5：
p5 <- p4+geom_boxplot(outlier.shape = NA, #隐藏离群点；
                      width =0.1,
                      alpha=0.7)
p5

#将图形翻转
p6<-p5+coord_flip()
p6

#再次自定义颜色
mycolor2 <- c("#72A9D0","#90C7C2","#E07B54") 
p7 <- p6+scale_color_manual(values=rev(mycolor2))+scale_fill_manual(values=rev(mycolor2))
p7

# 去除灰底的 ggplot2 图形
p8 <- p7 + theme_bw()
p8

#再去掉背景网格线
# 去掉背景网格线并修改 x 轴标签、y 轴标签和图形标题
p9 <- p8 + theme_bw() + theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5)) +
  xlab("Gruop Type") + ylab("CESD-A scores") +
  ggtitle("2016 Cloud and Rain Chart of CESD Values for Different Conflict Groups")

p9+scale_x_discrete(labels=new_labels)  # 更改x轴标签

# shapiro.test(df$cesd16)
# #结果表明，p>0.05，接受原假设，说明数据在多个水平下都是正态分布的。
# bartlett.test(cesd16~conflict16_type,data=df)
# #结果表明，p>0.05，接受原假设，说明数据在不同水平下是等方差的。
# fit<-aov(cesd16~conflict16_type,data=df)
# summary(fit)
#Kruskal-Wallis Test，可使用 ?kruskal.test 查看帮助
kruskal.test(cesd16~conflict16_type,data=df)
#非正态性的数据，不太适合以均值及标准差的形式作为展示，可以使用中位数等代替。
#p<0.05，存在显著差异

# compare_means(cesd16~conflict16_type,data=df,method="anova")
# compare_means(cesd16~conflict16_type,data=df,method="kruskal.test")
# 
# my_comparisons <- list(c("无冲突组", "中等冲突组"), c("无冲突组", "强冲突组"), c("中等冲突组", "强冲突组"))
# 
# p10 <- p9 + stat_compare_means(label.y = 22,method = "anova")+
#   stat_compare_means(comparisons = my_comparisons,label.y = c(21,23,22))
# p10

#step1
stat_data <- df %>%
  group_by(conflict16_type) %>%
  summarise( 
    n=n(),
    mean=mean(cesd16),
    sd=sd(cesd16),
    se=sd(cesd16)/sqrt(n)
  )
# step2 指定分组进行比较
comparisons <- list(  c("No-conflict groups", "Strong-conflict groups"), c("Medium-conflict groups", "Strong-conflict groups"))

# step3 指定检验方法，支持t.test, wilcox.test等
test <- "wilcox.test"

# step4 

p10 <- p9+geom_signif(
  comparisons = comparisons, 
  test = test,
  map_signif_level = TRUE, 
  textsize = 4,
  # 避免绘制显著性结果时发生重合
  step_increase = 0.2
)
p10+scale_x_discrete(labels=new_labels)  # 更改x轴标签

# 定义新的图例标签
legend_labels <- c("High-conflict", "Medium-conflict", "No-conflict")

# 最终图形 p10 的修改
p10 <- p9 + 
  theme_bw() + 
  theme(
    panel.grid = element_blank(),  # 去掉背景网格线
    plot.title = element_text(family = "serif", size = 14, face = "bold", hjust = 0.5),  
    plot.subtitle = element_text(family = "serif", size = 10, face = "italic", hjust = 0.5),  
    axis.title.x = element_text(family = "serif", size = 12),  
    axis.title.y = element_blank(),  # 去掉y轴标题
    axis.text = element_text(family = "serif", size = 12),  
    legend.text = element_text(family = "serif", size = 10),  # 减小图例文字大小
    legend.title = element_blank(),  # 去掉图例标题
    legend.justification = c(0, 0),  # 图例左下角对齐点
    legend.position = c(0.74, 0.82),
    legend.background = element_rect(fill = "white", color = NA),  # 图例背景
    legend.key.size = unit(1.1, "lines"),  # 减小图例符号大小
    plot.margin = unit(c(1, 5, 1, 1), "cm"),  # 增加右边距
    legend.spacing.y = unit(2, "lines")  # 减小图例项目间距
  ) +
  xlab("Group Type") + 
  ggtitle("(2016) Distribution of CESD-A scores by conflict groups") +
  scale_color_manual(values = rev(mycolor2), labels = legend_labels) +  
  scale_fill_manual(values = rev(mycolor2), labels = legend_labels) +
  geom_signif(
    comparisons = comparisons, 
    test = test,
    map_signif_level = TRUE, 
    textsize = 4,
    step_increase = 0.2
  ) + 
  scale_x_discrete(labels = new_labels)

# 显示最终图形
p10

# 保存图形为 PNG 文件，指定完整路径
ggsave("C:/Users/阿龟/Desktop/父母抑郁与子女抑郁/250108返修/0111图片整理/2016cloud.png", 
       plot = p10, width = 9, height = 6, dpi = 800)








#2018
kruskal.test(cesd18~conflict18_type,data=df)
#p<0.05，中位数有显著差异
#非正态性的数据，不太适合以均值及标准差的形式作为展示，可以使用中位数等代替。
#p<0.05，存在显著差异
# 绘制小提琴图
order_vec <- c("Strong-conflict groups", "Medium-conflict groups", "No-conflict groups")
df$conflict18_type <- factor(df$conflict18_type, levels = order_vec)  # 改为2018
p <- ggplot(df, aes(x=conflict18_type,
                    y=cesd18,  # 改为2018
                    fill=conflict18_type,
                    color=conflict18_type)) 
p

mycolor <- c("#72A9D0","#90C7C2","#E07B54") 
p0 <- p + scale_color_manual(values=rev(mycolor)) +
  scale_fill_manual(values=rev(mycolor))+scale_x_discrete(labels=new_labels)  # 更改x轴标签
p0

# 先画一半小提琴图(geom_half_violin)，得到p1：
p1 <- p0 + geom_half_violin()
p1

# 一半小提琴图的参数调整：
p2 <- p + geom_half_violin(position = position_nudge(x = 0.1, y = 0),
                           side = 'R', adjust=1, trim = F, color = NA, alpha = 0.8)
p2

# 显示绘制的小提琴图
p3 <- p2 + geom_point() + scale_x_discrete(labels=new_labels)  # 更改x轴标签
p3

# 调整散点，得到p4：
df$conflict18_type_numeric <- ifelse(df$conflict18_type == "No-conflict groups", 3,
                                     ifelse(df$conflict18_type == "Medium-conflict groups", 2, 1))
# 重新绘制散点图
p4 <- p2 + geom_point(data = df, aes(x = conflict18_type_numeric - 0.1, y = cesd18, color = conflict18_type),  # 改为2018
                      position = position_jitter(width = 0.03), size = 0.2, shape = 20)
p4

# 在p4散点和二分之一小提琴图中间添加箱线图，得到p5：
p5 <- p4 + geom_boxplot(outlier.shape = NA, width = 0.1, alpha = 0.7)
p5

# 将图形翻转
p6 <- p5 + coord_flip()
p6

# 再次自定义颜色
mycolor2 <- c("#72A9D0","#90C7C2","#E07B54") 
p7 <- p6 + scale_color_manual(values = rev(mycolor2)) + scale_fill_manual(values = rev(mycolor2))
p7

# 去除灰底的 ggplot2 图形
p8 <- p7 + theme_bw()
p8

# 再去掉背景网格线，修改x轴标签、y轴标签和图形标题（改为2018）
p9 <- p8 + theme_bw() + theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5)) +
  xlab("Group types") + ylab("CESD-A scores") +
  ggtitle("2018 Cloud and Rain Chart of CESD Values for Different Conflict Groups")  # 改为2018

p9 + scale_x_discrete(labels = new_labels)  # 更改x轴标签

# Kruskal-Wallis Test
kruskal.test(cesd18 ~ conflict18_type, data = df)  # 改为2018

# 添加显著性标记
stat_data <- df %>%
  group_by(conflict18_type) %>%
  summarise( 
    n = n(),
    mean = mean(cesd18),  # 改为2018
    sd = sd(cesd18),
    se = sd(cesd18) / sqrt(n)
  )

# 指定分组进行比较
comparisons <- list(c("No-conflict groups", "Strong-conflict groups"), c("Medium-conflict groups", "Strong-conflict groups"))
test <- "wilcox.test"

# 最终图表，添加显著性标记
p10 <- p9 + geom_signif(
  comparisons = comparisons, 
  test = test,
  map_signif_level = TRUE, 
  textsize = 4,
  step_increase = 0.2
) + 
  scale_x_discrete(labels = new_labels)  # 更改x轴标签


# 最终图形 p10 的修改
p10 <- p9 + 
  theme_bw() + 
  theme(
    panel.grid = element_blank(),  # 去掉背景网格线
    plot.title = element_text(family = "serif", size = 14, face = "bold", hjust = 0.5),  
    plot.subtitle = element_text(family = "serif", size = 10, face = "italic", hjust = 0.5),  
    axis.title.x = element_text(family = "serif", size = 12),  
    axis.title.y = element_blank(),  # 去掉y轴标题
    axis.text = element_text(family = "serif", size = 12),  
    legend.text = element_text(family = "serif", size = 10),  # 减小图例文字大小
    legend.title = element_blank(),  # 去掉图例标题
    legend.justification = c(0, 0),  # 图例左下角对齐点
    legend.position = c(0.74, 0.82),
    legend.background = element_rect(fill = "white", color = NA),  # 图例背景
    legend.key.size = unit(1.1, "lines"),  # 减小图例符号大小
    plot.margin = unit(c(1, 5, 1, 1), "cm"),  # 增加右边距
    legend.spacing.y = unit(2, "lines")  # 减小图例项目间距
  ) +
  xlab("Group Type") + 
  ggtitle("(2018) Distribution of CESD-A scores by conflict groups") +
  scale_color_manual(values = rev(mycolor2), labels = legend_labels) +  
  scale_fill_manual(values = rev(mycolor2), labels = legend_labels) +
  geom_signif(
    comparisons = comparisons, 
    test = test,
    map_signif_level = TRUE, 
    textsize = 4,
    step_increase = 0.2
  ) + 
  scale_x_discrete(labels = new_labels)

# 显示最终图形
p10

# 保存图形为 PNG 文件，指定完整路径
ggsave("C:/Users/阿龟/Desktop/父母抑郁与子女抑郁/250108返修/0111图片整理/2018cloud.png", 
       plot = p10, width = 9, height = 6, dpi = 800)



#2020
kruskal.test(cesd20~conflict20_type,data=df)
#p<0.05，中位数有显著差异
#非正态性的数据，不太适合以均值及标准差的形式作为展示，可以使用中位数等代替。
#p<0.05，存在显著差异
# 绘制小提琴图
order_vec <- c("Strong-conflict groups", "Medium-conflict groups", "No-conflict groups")
df$conflict20_type <- factor(df$conflict20_type, levels = order_vec)
p <- ggplot(df, aes(x=conflict20_type,
                    y=cesd20,
                    fill=conflict20_type,
                    color=conflict20_type)) 
p

mycolor <- c("#72A9D0","#90C7C2","#E07B54") 
p0 <- p + scale_color_manual(values=rev(mycolor)) +
  scale_fill_manual(values=rev(mycolor))+scale_x_discrete(labels=new_labels)

p1 <- p0 + geom_half_violin()

p2 <- p + geom_half_violin(position = position_nudge(x = 0.1, y = 0),
                           side = 'R', adjust=1, trim = F, color = NA, alpha = 0.8)

p3 <- p2 + geom_point() + scale_x_discrete(labels=new_labels)

df$conflict20_type_numeric <- ifelse(df$conflict20_type == "No-conflict groups", 3,
                                     ifelse(df$conflict20_type == "Medium-conflict groups", 2, 1))

p4 <- p2 + geom_point(data = df, aes(x = conflict20_type_numeric - 0.1, y = cesd20,
                                     color = conflict20_type),
                      position = position_jitter(width = 0.03), size = 0.2, shape = 20)

p5 <- p4 + geom_boxplot(outlier.shape = NA, width = 0.1, alpha = 0.7)

p6 <- p5 + coord_flip()

mycolor2 <- c("#72A9D0","#90C7C2","#E07B54") 
p7 <- p6 + scale_color_manual(values = rev(mycolor2)) + scale_fill_manual(values = rev(mycolor2))

p8 <- p7 + theme_bw()

p9 <- p8 + theme_bw() + theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5)) +
  xlab("Group types") + ylab("CESD-A scores") +
  ggtitle("2020 Cloud and Rain Chart of CESD-A Values for Different Conflict Groups")

p9 + scale_x_discrete(labels = new_labels)

kruskal.test(cesd20 ~ conflict20_type, data = df)

stat_data <- df %>%
  group_by(conflict20_type) %>%
  summarise( 
    n = n(),
    mean = mean(cesd20),
    sd = sd(cesd20),
    se = sd(cesd20) / sqrt(n)
  )

comparisons <- list(c("No-conflict groups", "Strong-conflict groups"), 
                    c("Medium-conflict groups", "Strong-conflict groups"))
test <- "wilcox.test"

p10 <- p9 + geom_signif(
  comparisons = comparisons, 
  test = test,
  map_signif_level = TRUE, 
  textsize = 4,
  step_increase = 0.2
) + 
  scale_x_discrete(labels = new_labels)

p10 <- p9 + 
  theme_bw() + 
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(family = "serif", size = 14, face = "bold", hjust = 0.5),  
    plot.subtitle = element_text(family = "serif", size = 10, face = "italic", hjust = 0.5),  
    axis.title.x = element_text(family = "serif", size = 12),  
    axis.title.y = element_blank(),
    axis.text = element_text(family = "serif", size = 12),  
    legend.text = element_text(family = "serif", size = 10),
    legend.title = element_blank(),
    legend.justification = c(0, 0),
    legend.position = c(0.74, 0.82),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.size = unit(1.1, "lines"),
    plot.margin = unit(c(1, 5, 1, 1), "cm"),
    legend.spacing.y = unit(2, "lines")
  ) +
  xlab("Group Type") + 
  ggtitle("(2020) Distribution of CESD-A scores by conflict groups") +
  scale_color_manual(values = rev(mycolor2), labels = legend_labels) +  
  scale_fill_manual(values = rev(mycolor2), labels = legend_labels) +
  geom_signif(
    comparisons = comparisons, 
    test = test,
    map_signif_level = TRUE, 
    textsize = 4,
    step_increase = 0.2
  ) + 
  scale_x_discrete(labels = new_labels)

p10

ggsave("C:/Users/阿龟/Desktop/父母抑郁与子女抑郁/250108返修/0111图片整理/2020cloud.png", 
       plot = p10, width = 9, height = 6, dpi = 800)










