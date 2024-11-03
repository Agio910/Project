library(ggplot2)
install.packages("dplyr")
library("dplyr")
#安装gghalves包：
install.packages('gghalves')
library(readr)
#载入ggplot2、gghalves包：
library(ggplot2)
library(gghalves)
install.packages("ggsignif")
# 载入R包
library(ggplot2)
library(ggsignif)
install.packages("ggpubr")
library("ggpubr")

install.packages("PupillometryR")
library(PupillometryR)

install.packages("tidyr")
# 加载需要的包
library(tidyr)
install.packages("car")
library(car) # 用于重复测量方差分析
install.packages("emmeans")
library(emmeans)     # 用于Tukey post-hoc测试
install.packages("multcomp")
library(multcomp)    # 用于多重比较的Tukey HSD测试
install.packages("stats")
library(stats)
library(dplyr)

df <- read_csv("C:/Users/阿龟/Desktop/《我的大学》/科研助理/父母抑郁与子女抑郁/R绘图/conflict_cesd.csv")
names(df) <- c("cesd16","cesd18","cesd20","conflict16","conflict18","conflict20")
###
#方差检验
# 转换为长格式
df_long <- df %>%
  select(cesd16, cesd18, cesd20) %>%
  pivot_longer(cols = everything(),
               names_to = "time",
               values_to = "cesd")

# 将 time 转为有序因子
df_long$time <- factor(df_long$time, levels = c("cesd16", "cesd18", "cesd20"), ordered = TRUE)

# 进行单因素方差分析
aov_result <- aov(cesd ~ time, data = df_long)

# Tukey事后检验
tukey_result <- TukeyHSD(aov_result)

# 打印结果
print(tukey_result)

# 方差分析结果
summary(aov_result)











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










df2 <- read_csv("C:/Users/阿龟/Desktop/父母抑郁与子女抑郁/R绘图/0406全变量.csv")
df2 <- as.data.frame(df2)
# 1. 创建孩子的长格式数据
child_long <- df2 %>%
  select(cesd16, cesd18, cesd20, conflict_16, conflict_18, conflict_20) %>%
  pivot_longer(
    cols = c(cesd16, cesd18, cesd20),
    names_to = "time",
    values_to = "cesd"
  ) %>%
  mutate(
    year = case_when(
      time == "cesd16" ~ 16,
      time == "cesd18" ~ 18,
      time == "cesd20" ~ 20
    ),
    conflict = case_when(
      time == "cesd16" ~ conflict_16,
      time == "cesd18" ~ conflict_18,
      time == "cesd20" ~ conflict_20
    )
  )

# 2. 创建母亲的长格式数据
mother_long <- df2 %>%
  select(cesd_m16, cesd_m18, cesd_m20, conflict_16, conflict_18, conflict_20) %>%
  pivot_longer(
    cols = c(cesd_m16, cesd_m18, cesd_m20),
    names_to = "time",
    values_to = "cesd_m"
  ) %>%
  mutate(
    year = case_when(
      time == "cesd_m16" ~ 16,
      time == "cesd_m18" ~ 18,
      time == "cesd_m20" ~ 20
    ),
    conflict = case_when(
      time == "cesd_m16" ~ conflict_16,
      time == "cesd_m18" ~ conflict_18,
      time == "cesd_m20" ~ conflict_20
    )
  )

# 3. 建立线性模型
# 孩子模型
child_model <- lm(cesd ~ year + conflict, data = child_long)

# 母亲模型
mother_model <- lm(cesd_m ~ year + conflict, data = mother_long)

# 4. 查看模型结果
summary(child_model)
summary(mother_model)































# # 绘图

pd <- position_dodge(0.1)

ggplot(result, aes(x = year, y = conflict, colour = type)) + 
  geom_errorbar(aes(ymin = conflict - standard_deviations, ymax = conflict + standard_deviations), width = 0.1, position = pd) +
  geom_line(position = position_dodge(width = 0.1)) +
  geom_point(position = position_dodge(width = 0.1))
##偏差折线图
# 创建带有偏差线图的 ggplot 对象
pd <- position_dodge(0.1)

ggplot(result, aes(x = year, y = conflict, colour = type, group = type)) + 
  geom_errorbar(aes(ymin = conflict - standard_deviations, ymax = conflict + standard_deviations), 
                colour = "black", width = 0.1, position = position_dodge(width = 0.2)) +
  geom_line(size = 1,position = position_dodge(width = 0.2)) +
  geom_point(position = position_dodge(width = 0.2), size = 3, shape = 21, fill = "white") + 
  xlab("Year") +
  ylab("CESD Scores") +
  scale_colour_manual(name = "Group Type",
                      breaks = c("No-conflict groups", "Medium-conflict groups", "Strong-conflict groups"),
                      labels = c("No ", "Medium ", "Strong "),
                      values = c("#72A9D0","#90C7C2","#E07B54")) + 
  ggtitle("CESD Scores for different Groups of conflict in different years") +
  expand_limits(y = 11, x = c(2016, 2021)) +
  scale_y_continuous(breaks = seq(0, 80, by = 0.25)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(1, 0),
        plot.title = element_text(hjust = 0.5, family = "serif", face = "bold"),
        text = element_text(family = "serif"),
        axis.text = element_text(family = "serif"),
        legend.text = element_text(family = "serif"))

##云雨图

#绘图开始，将数据与图形建立映射，存到变量p
#（使用Species及Sepal.Length为例来进行绘制）：

# 设定你想要的新的x轴标签
new_labels <- c("High", "Medium", "No")

#2016年
# 绘制小提琴图
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
  xlab("Gruop Type") + ylab("CESD Scores") +
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

p10 <- p9 + 
  theme_bw() + 
  theme(
    panel.grid = element_blank(),  # 去掉背景网格线
    plot.title = element_text(family = "serif", size = 14, face = "bold", hjust = 0.5),  
    plot.subtitle = element_text(family = "serif", size = 10, face = "italic", hjust = 0.5),  
    axis.title.x = element_text(family = "serif", size = 12),  
    axis.title.y = element_blank(),  # 去掉y轴标题
    axis.text = element_text(family = "serif", size = 12),  
    legend.text = element_text(family = "serif", size = 11),  # 减小图例文字大小
    legend.title = element_blank(),  # 去掉图例标题
    legend.justification = c(0, 0),  # 图例左下角对齐点
    legend.position = c(1.02, 0),   # 图例位置
    legend.background = element_rect(fill = "white", color = NA),  # 图例背景
    legend.key.size = unit(1.1, "lines"),  # 减小图例符号大小
    plot.margin = unit(c(1, 5, 1, 1), "cm"),  # 增加右边距
    legend.spacing.y = unit(2, "lines")  # 减小图例项目间距
  ) +
  xlab("Group Type") + 
  ggtitle("(2016)Distribution of CESD Scores by Conflict Groups") +
  scale_color_manual(values = rev(mycolor2)) +  
  scale_fill_manual(values = rev(mycolor2)) +
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


p10 <- p9 + 
  theme_bw() + 
  theme(
    panel.grid = element_blank(),  # 去掉背景网格线
    plot.title = element_text(family = "serif", size = 14, face = "bold", hjust = 0.5),  
    plot.subtitle = element_text(family = "serif", size = 10, face = "italic", hjust = 0.5),  
    axis.title.x = element_text(family = "serif", size = 12),  
    axis.title.y = element_blank(),  # 去掉y轴标题
    axis.text = element_text(family = "serif", size = 12),  
    legend.text = element_text(family = "serif", size = 8),  
    legend.title = element_blank(),  # 去掉图例标题
    legend.justification = c(1, 0),  # 图例右下角对齐
    legend.position = c(0.98, 0.02),   # 图例位置在绘图区域内的右下角
    legend.background = element_rect(fill = "white", color = NA),  
    legend.key.size = unit(1.1, "lines"),  
    plot.margin = unit(c(1, 1, 1, 1), "cm"),  
    legend.spacing.y = unit(0.8, "lines"),
    legend.margin = margin(2, 2, 2, 2)  # 图例内边距
  ) +
  xlab("Group Type") + 
  ggtitle("(2016)Distribution of CESD Scores by Conflict Groups") +
  scale_color_manual(values = rev(mycolor2)) +  
  scale_fill_manual(values = rev(mycolor2)) +
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
  xlab("Group Type") + ylab("CESD Scores") +
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


p10 <- p9 + 
  theme_bw() + 
  theme(
    panel.grid = element_blank(),  # 去掉背景网格线
    plot.title = element_text(family = "serif", size = 14, face = "bold", hjust = 0.5),  
    plot.subtitle = element_text(family = "serif", size = 10, face = "italic", hjust = 0.5),  
    axis.title.x = element_text(family = "serif", size = 12),  
    axis.title.y = element_blank(),  # 去掉y轴标题
    axis.text = element_text(family = "serif", size = 12),  
    legend.text = element_text(family = "serif", size = 11),  # 减小图例文字大小
    legend.title = element_blank(),  # 去掉图例标题
    legend.justification = c(0, 0),  # 图例左下角对齐点
    legend.position = c(1.02, 0),   # 图例位置
    legend.background = element_rect(fill = "white", color = NA),  # 图例背景
    legend.key.size = unit(1.1, "lines"),  # 减小图例符号大小
    plot.margin = unit(c(1, 5, 1, 1), "cm"),  # 增加右边距
    legend.spacing.y = unit(2, "lines")  # 减小图例项目间距
  ) +
  xlab("Group Type") + 
  ggtitle("(2018)Distribution of CESD Scores by Conflict Groups") +
  scale_color_manual(values = rev(mycolor2)) +  
  scale_fill_manual(values = rev(mycolor2)) +
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







##2020
kruskal.test(cesd20~conflict20_type,data=df)
#p<0.05，中位数有显著差异
df$conflict20_type <- factor(df$conflict20_type, levels = order_vec)
p <- ggplot(df,aes(x=conflict20_type,
                   y=cesd18,
                   fill=conflict20_type,
                   color=conflict20_type))
p

mycolor <- c("#72A9D0","#90C7C2","#E07B54")  
p0 <- p + scale_color_manual(values=rev(mycolor)) +
  scale_fill_manual(values=rev(mycolor))
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
p3<- p2+geom_point()
p3
#调整散点，得到p4：
# 检查和处理缺失值

# 将字符变量转换为数值型变量
df$conflict20_type_numeric <- ifelse(df$conflict20_type == "无冲突组",3,
                                     ifelse(df$conflict20_type == "中等冲突组",2,1))




# 重新绘制散点图
p4 <- p2 + geom_point(data = df,aes(x = conflict20_type_numeric - 0.1, y = cesd20, color = conflict20_type),
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
  xlab("冲突组别") + ylab("CESD值") +
  ggtitle("2020不同冲突水平CESD值云雨图")

p9

#step1
stat_data <- df %>%
  group_by(conflict20_type) %>%
  summarise( 
    n=n(),
    mean=mean(cesd20),
    sd=sd(cesd20),
    se=sd(cesd20)/sqrt(n)
  )
# step2 指定分组进行比较

#2020
kruskal.test(cesd20~conflict20_type,data=df)
#p<0.05，中位数有显著差异
#非正态性的数据，不太适合以均值及标准差的形式作为展示，可以使用中位数等代替。
#p<0.05，存在显著差异
# 绘制小提琴图
order_vec <- c("Strong-conflict groups", "Medium-conflict groups", "No-conflict groups")
df$conflict20_type <- factor(df$conflict20_type, levels = order_vec)  # 改为2020
p <- ggplot(df, aes(x=conflict20_type,
                    y=cesd20,  # 改为2020
                    fill=conflict20_type,
                    color=conflict20_type)) 
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
df$conflict20_type_numeric <- ifelse(df$conflict20_type == "No-conflict groups", 3,
                                     ifelse(df$conflict20_type == "Medium-conflict groups", 2, 1))
# 重新绘制散点图
p4 <- p2 + geom_point(data = df, aes(x = conflict20_type_numeric - 0.1, y = cesd20, color = conflict20_type),  # 改为2020
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

# 再去掉背景网格线，修改x轴标签、y轴标签和图形标题（改为2020）
p9 <- p8 + theme_bw() + theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5)) +
  xlab("Group Type") + ylab("CESD Scores") +
  ggtitle("2020 Cloud and Rain Chart of CESD Values for Different Conflict Groups")  # 改为2020

p9 + scale_x_discrete(labels = new_labels)  # 更改x轴标签

# Kruskal-Wallis Test
kruskal.test(cesd20 ~ conflict20_type, data = df)  # 改为2020

# 添加显著性标记
stat_data <- df %>%
  group_by(conflict20_type) %>%
  summarise( 
    n = n(),
    mean = mean(cesd20),  # 改为2020
    sd = sd(cesd20),
    se = sd(cesd20) / sqrt(n)
  )

# 指定分组进行比较（添加第三条误差线）
comparisons <- list(c("No-conflict groups", "Strong-conflict groups"), 
                    c("Medium-conflict groups", "Strong-conflict groups"),
                    c("No-conflict groups", "Medium-conflict groups"))
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

# 使用 Times New Roman 字体并修改图例标题为 "Group Type"
p10 <- p10 + 
  theme_bw() + 
  theme(
    panel.grid = element_blank(),  # 去掉背景网格线
    plot.title = element_text(family = "Times New Roman", size = 14, face = "bold", hjust = 0.5),  # 主标题字体调整
    plot.subtitle = element_text(family = "Times New Roman", size = 10, face = "italic", hjust = 0.5),  # 副标题字体调整
    axis.title.x = element_text(family = "Times New Roman", size = 12),  # x轴标签字体
    axis.title.y = element_text(family = "Times New Roman", size = 12),  # y轴标签字体
    axis.text = element_text(family = "Times New Roman", size = 10),  # 轴刻度字体
    legend.text = element_text(family = "Times New Roman", size = 10),  # 图例文本字体
    legend.title = element_text(family = "Times New Roman", size = 12)  # 图例标题字体
  ) +
  xlab("Group Type") + 
  ylab("CESD Scores") +
  ggtitle("(2020)Distribution of CESD Scores by Conflict Groups")  # 修改标题和副标题

# 修改图例标题为 "Group Type"
p10 <- p10 + 
  scale_color_manual(name = "Group Type", values = rev(mycolor2)) +  # 修改图例颜色并设置标题
  scale_fill_manual(name = "Group Type", values = rev(mycolor2))  # 修改图例填充并设置标题

# 显示最终图形
p10
test <- "wilcox.test"
# step3 指定检验方法，支持t.test, wilcox.test等
test <- "wilcox.test"

# step4 

p12 <- p9+geom_signif(
  comparisons = comparisons, 
  test = test,
  map_signif_level = TRUE, 
  textsize = 4,
  # 避免绘制显著性结果时发生重合
  step_increase = 0.2
)
p12

p13 <- p10+p11+p12
p13




p13 <- p9 + 
  theme_bw() + 
  theme(
    panel.grid = element_blank(),  # 去掉背景网格线
    plot.title = element_text(family = "serif", size = 14, face = "bold", hjust = 0.5),  
    plot.subtitle = element_text(family = "serif", size = 10, face = "italic", hjust = 0.5),  
    axis.title.x = element_text(family = "serif", size = 12),  
    axis.title.y = element_blank(),  # 去掉y轴标题
    axis.text = element_text(family = "serif", size = 12),  
    legend.text = element_text(family = "serif", size = 11),  # 减小图例文字大小
    legend.title = element_blank(),  # 去掉图例标题
    legend.justification = c(0, 0),  # 图例左下角对齐点
    legend.position = c(1.02, 0),   # 图例位置
    legend.background = element_rect(fill = "white", color = NA),  # 图例背景
    legend.key.size = unit(1.1, "lines"),  # 减小图例符号大小
    plot.margin = unit(c(1, 5, 1, 1), "cm"),  # 增加右边距
    legend.spacing.y = unit(2, "lines")  # 减小图例项目间距6+
  ) +
  xlab("Group Type") + 
  ggtitle("(2020)Distribution of CESD Scores by Conflict Groups") +
  scale_color_manual(values = rev(mycolor2)) +  
  scale_fill_manual(values = rev(mycolor2)) +
  geom_signif(
    comparisons = comparisons, 
    test = test,
    map_signif_level = TRUE, 
    textsize = 4,
    step_increase = 0.2
  ) + 
  scale_x_discrete(labels = new_labels)

# 显示最终图形
p13