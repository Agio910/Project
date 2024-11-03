setwd("C:/Users/阿龟/Desktop/父母抑郁与子女抑郁/R绘图")
library(ggplot2)
install.packages("ComplexHeatmap")
install.packages("devtools")
library(usethis)
library(devtools)
install_github("jokergoo/ComplexHeatmap")
library(ComplexHeatmap)
library(readxl)
install.packages("psych")
library(psych)


data1<-read.csv("0406全变量.csv")
data1<-as.data.frame(data1)
# rownames(data1)<-data1$...1
# data1<-data1[,-1]
View(data1)
colnames(data1)
data1<-data1[,5:16]
result<-corr.test(data1)
View(result$r)
View(result$p)
rzhi<-result$r
pzhi<-result$p
pzhi_star <- ifelse(pzhi < 0.001, "***",
                    ifelse(pzhi < 0.01, "**",
                           ifelse(pzhi < 0.05, "*", "")))
View(pzhi_star)
rzhi<-as.data.frame(rzhi)
pzhi_star<-as.data.frame(pzhi_star)

#View(rzhi)
#View(pzhi_star)
color<-circlize::colorRamp2(c(-1,-0.1,0,0.1,1), c("#f5b378","#f5f0de","white", "#ffeaea",'#F78074'))


p1heat<-Heatmap(rzhi,name=" ",col=color,column_title=NA,row_names_side = "left",cluster_columns = FALSE,cluster_rows = FALSE,column_names_rot = 0,column_names_centered=TRUE,rect_gp=gpar(col="white",lwd=1),cell_fun = function(j,i,x,y,width,height,fill) {

  grid.text(paste0(sprintf("%.2f", rzhi[i, j]),"\n", pzhi_star[i, j]), x, y, gp = gpar(fontsize = 10,fontfamily="serif"))
}

)

p1heat


split_names <- function(names) {
  sapply(names, function(name) {
    if (nchar(name) > 4) {
      paste(substr(name, 1, 4), substr(name, 5, nchar(name)), sep = "\n")
    } else {
      name
    }
  })
}

new_names <- c(
  cesd16 = "CESD\n(2016)",
  cesd18 = "CESD\n(2018)",
  cesd20 = "CESD\n(2020)",
  cesd_f16 = "CESDF\n(2016)",
  cesd_f18 = "CESDF\n(2018)",
  cesd_f20 = "CESDF\n(2020)",
  cesd_m16 = "CESDM\n(2016)",
  cesd_m18 = "CESDM\n(2018)",
  cesd_m20 = "CESDM\n(2020)",
  conflict_16 = "Conflict\n(2016)",
  conflict_18 = "Conflict\n(2018)",
  conflict_20 = "Conflict\n(2020)"
)

library(ComplexHeatmap)
library(grid)
library(circlize)

# 应用新的变量名
colnames(rzhi) <- new_names[colnames(rzhi)]
rownames(rzhi) <- new_names[rownames(rzhi)]

p1heat <- Heatmap(rzhi,
                  name = " ",
                  col = color,
                  column_title = NA,
                  row_title = NA,
                  row_names_side = "left",
                  cluster_columns = FALSE,
                  cluster_rows = FALSE,
                  column_names_rot = 0,
                  column_names_centered = TRUE,
                  rect_gp = gpar(col = "white", lwd = 1),
                  cell_fun = function(j, i, x, y, width, height, fill) {
                    grid.text(paste0(sprintf("%.2f", rzhi[i, j]), "\n", pzhi_star[i, j]), 
                              x, y, gp = gpar(fontsize = 10, fontfamily = "serif"))
                  },
                  heatmap_legend_param = list(
                    legend_height = unit(4, "cm"),  # 增加图例高度
                    grid_width = unit(0.5, "cm")    # 调整图例宽度
                  ),
                  row_names_gp = gpar(fontsize = 10,fontfamily = "serif"),
                  column_names_gp = gpar(fontsize = 10, vjust = -2, lineheight=0.6,fontfamily = "serif")
)

# 显示热图
p1heat












# 第二张图

library(ggplot2)
library(ComplexHeatmap)
library(readxl)
library(psych)


data1<-read.csv("数据1.csv")
data1<-as.data.frame(data1)
# rownames(data1)<-data1$...1
# data1<-data1[,-1]
View(data1)
rownames(data1)<-data1$X
data1<-data1[,-1]
data1<-t(data1)

data2<-read.csv("文本1.csv")
data2<-as.data.frame(data2)
rownames(data2)<-data2$X
data2<-data2[,-1]
View(data2)
stars_df <- data2

# 使用 apply 而不是 lapply，保证返回的是矩阵/数据框
stars_df <- as.data.frame(apply(data2, 2, function(x) {
  # 如果包含星号，则保留星号；否则，返回空字符串
  gsub("[^*]", "", x)
}))

# 查看提取后的 stars_df
#View(stars_df)




library(ComplexHeatmap)
library(grid)
library(circlize)

data1<-read.csv("数据1.csv")
data1<-as.data.frame(data1)
# rownames(data1)<-data1$...1
# data1<-data1[,-1]
View(data1)
rownames(data1)<-data1$X
data1<-data1[,-1]
data1<-t(data1)

data2<-read.csv("文本1.csv")
data2<-as.data.frame(data2)
rownames(data2)<-data2$X
data2<-data2[,-1]
View(data2)
stars_df <- data2

# 使用 apply 而不是 lapply，保证返回的是矩阵/数据框
stars_df <- as.data.frame(apply(data2, 2, function(x) {
  # 如果包含星号，则保留星号；否则，返回空字符串
  gsub("[^*]", "", x)
}))

# 查看提取后的 stars_df
#View(stars_df)


# rownames(stars_df)<-data_total$Indicators
# stars_df<-stars_df
# 从原始数据中移除星号，保留数值
# 使用 apply 来移除数据中的星号，保留数值
data2 <- as.data.frame(apply(data2, 2, function(x) {
  # 使用 gsub 去掉所有的星号，保留数字
  gsub("\\*+", "", x)
}), stringsAsFactors = FALSE)

# 查看结果
View(data2)
# 将 data2 中的所有列转换为数值类型
data2[] <- lapply(data2, function(x) as.numeric(as.character(x)))

color<-circlize::colorRamp2(c(-0.15,-0.05,0,0.05,0.15), c("#90c7c2","#f4ffff","white", "#eef4ff",'#72A9D0'))
color
View(data2)
p1heat<-Heatmap(data2,name=" ",col=color,column_title=NA,row_names_side = "left",cluster_columns = FALSE,cluster_rows = FALSE,column_names_rot = 0,column_names_centered=TRUE,rect_gp=gpar(col="white",lwd=1),cell_fun = function(j,i,x,y,width,height,fill) {
  grid.text(paste0(sprintf("%.2f", data2[i, j]),"\n", stars_df[i, j]), x, y, gp = gpar(fontsize = 10,fontfamily="serif"))
}
)

p1heat


# ... (previous code remains the same)

# Define new row and column names
new_names2 <- c(
  C16 = "CESD\n(2016)", C18 = "CESD\n(2018)", C20 = "CESD\n(2020)",
  F16 = "CESDF\n(2016)", F18 = "CESDF\n(2018)", F20 = "CESDF\n(2020)",
  M16 = "CESDM\n(2016)", M18 = "CESDM\n(2018)", M20 = "CESDM\n(2020)",
  Con16 = "Conflict\n(2016)", Con18 = "Conflict\n(2018)", Con20 = "Conflict\n(2020)"
)

new_names3 <- c(
  F_edu = "Father\neducation", F_marriage = "Father\nmarriage",
  Gender = "Gender", M_edu = "Mother\neducation", M_marraige = "Mother\nmarriage"
)

# Update row and column names
colnames(data2) <- new_names2[colnames(data2)]
rownames(data2) <- new_names3[rownames(data2)]

# Update row and column names for stars_df as well
colnames(stars_df) <- new_names2[colnames(stars_df)]
rownames(stars_df) <- new_names3[rownames(stars_df)]

# Create the heatmap with updated names and centered column and row names
p1heat <- Heatmap(data2, 
                  name = " ", 
                  col = color, 
                  column_title = NA, 
                  row_title = NA,
                  row_names_side = "left",
                  cluster_columns = FALSE, 
                  cluster_rows = FALSE, 
                  column_names_rot = 0,
                  column_names_centered = TRUE,
                  row_names_centered = TRUE,
                  column_names_gp = gpar(fontsize = 10, vjust = -0.5, lineheight=0.6,fontfamily = "serif"),
                  row_names_gp = gpar(fontsize = 10, fontfamily = "serif"),
                  rect_gp = gpar(col = "white", lwd = 1),
                  cell_fun = function(j, i, x, y, width, height, fill) {
                    grid.text(paste0(sprintf("%.2f", data2[i, j]), "\n", stars_df[i, j]), 
                              x, y, gp = gpar(fontsize = 10, fontfamily = "serif"))
                  },
                  heatmap_legend_param = list(
                    legend_height = unit(4, "cm"),  # 增加图例高度
                    grid_width = unit(0.5, "cm")    # 调整图例宽度
                  )
)

# Display the heatmap
p1heat




