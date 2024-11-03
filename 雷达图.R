install.packages("fmsb")
install.packages("RadarChart")
library(fmsb)
library(readr)

##2020
df <- read_csv("C:/Users/阿龟/Desktop/《我的大学》/科研助理/父母抑郁与子女抑郁/R绘图/4.CESD三维雷达图20.csv")
max_min <- data.frame(抑郁情绪 = c(8, 0),躯体症状 = c(8, 0), 积极情绪 = c(8, 0))
rownames(max_min) <- c("Max", "Min")
mean1=c(mean(df$抑郁情绪,na.rm=TRUE),mean(df$躯体症状,na.rm=TRUE),mean(df$积极情绪,na.rm=TRUE))
df1 <- df[c("抑郁情绪", "躯体症状", "积极情绪")]
df <- rbind(mean1,df1)
df <- rbind(max_min, df)

rayder_data <- df[c("Max", "Min", "1"), ]
radarchart(
  rayder_data, axistype = 1,
  # Customize the polygon
  pcol = "#00AFBB", pfcol = scales::alpha("#00AFBB", 0.5), plwd = 2, plty = 1,
  # Customize the grid
  cglcol = "grey", cglty = 4, cglwd = 0.6,
  # Customize the axis
  axislabcol = "grey", 
  # Variable labels
  vlcex = 1.1, vlabels = colnames(rayder_data),
  caxislabels = c(0, 2, 4, 6,8))

title(main = "2020CESD三维度雷达图", col.main = "black", cex.main = 1.2)


##2018
df <- read_csv("C:/Users/阿龟/Desktop/《我的大学》/科研助理/父母抑郁与子女抑郁/R绘图/4.CESD三维雷达图18.csv")
max_min <- data.frame(抑郁情绪 = c(8, 0),躯体症状 = c(8, 0), 积极情绪 = c(8, 0))
rownames(max_min) <- c("Max", "Min")
mean2=c(mean(df$抑郁情绪,na.rm=TRUE),mean(df$躯体症状,na.rm=TRUE),mean(df$积极情绪,na.rm=TRUE))
df1 <- df[c("抑郁情绪", "躯体症状", "积极情绪")]
df <- rbind(mean2,df1)
df <- rbind(max_min, df)

rayder_data <- df[c("Max", "Min", "1"), ]
radarchart(
  rayder_data, axistype = 1,
  # Customize the polygon
  pcol = "#CE97B0", pfcol = scales::alpha("#CE97B0", 0.5), plwd = 2, plty = 1,
  # Customize the grid
  cglcol = "grey", cglty = 4, cglwd = 0.6,
  # Customize the axis
  axislabcol = "grey", 
  # Variable labels
  vlcex = 1.1, vlabels = colnames(rayder_data),
  caxislabels = c(0, 2, 4, 6,8))

title(main = "2018CESD三维度雷达图", col.main = "black", cex.main = 1.2)

##2016
df <- read_csv("C:/Users/阿龟/Desktop/《我的大学》/科研助理/父母抑郁与子女抑郁/R绘图/4.CESD三维雷达图16.csv")
max_min <- data.frame(抑郁情绪 = c(8, 0),躯体症状 = c(8, 0), 积极情绪 = c(8, 0))
rownames(max_min) <- c("Max", "Min")
mean3=c(mean(df$抑郁情绪,na.rm=TRUE),mean(df$躯体症状,na.rm=TRUE),mean(df$积极情绪,na.rm=TRUE))
df1 <- df[c("抑郁情绪", "躯体症状", "积极情绪")]
df <- rbind(mean3,df1)
df <- rbind(max_min, df)

rayder_data <- df[c("Max", "Min", "1"), ]
radarchart(
  rayder_data, axistype = 1,
  # Customize the polygon
  pcol ="#F4A9A8", pfcol = scales::alpha("#F4A9A8", 0.5), plwd = 2, plty = 1,
  # Customize the grid
  cglcol = "grey", cglty = 4, cglwd = 0.6,
  # Customize the axis
  axislabcol = "grey", 
  # Variable labels
  vlcex = 1.1, vlabels = colnames(rayder_data),
  caxislabels = c(0, 2, 4, 6,8))

title(main = "2016CESD三维度雷达图", col.main = "black", cex.main = 1.2)

##多组雷达图
mean <- data.frame(mean1,mean2,mean3)
mean <- t(mean)
colnames(mean) <- c("抑郁情绪", "躯体症状", "积极情绪")
rownames(mean) <- c("2020", "2018", "2016")
mean <- rbind(max_min, mean)

colors_border<-c("#00AFBB", "#CE97B0", "#FC4E07")
colors_in <-c("#00AFBB", "#CE97B0", "#FC4E07")

  
radarchart(
  mean, axistype = 1,
  # Customize the polygon
  pcol = colors_border, pfcol = adjustcolor(colors_in,alpha.f = 0.2), plwd = 2, plty = 1,
  # Customize the grid
  cglcol = "grey", cglty = 4, cglwd = 0.6,
  # Customize the axis
  axislabcol = "grey", 
  # Variable labels
  vlcex = 1.1, vlabels = colnames(mean),
  caxislabels = c(0, 2, 4,6, 8))

# 添加图例
legend("right", legend = c("2020", "2018", "2016"), 
       fill = c("#00AFBB", "#CE97B0", "#FC4E07"), cex = 0.8)
title(main = "CESD多组雷达图", col.main = "black", cex.main = 1.2)