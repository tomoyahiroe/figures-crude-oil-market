#### Import libraries ####
library(ggplot2)
library(tidyverse)
library(openxlsx)
library(vars)
library(grid)
library(gridExtra)

#### Load modules in other files ####
source("modules/trivar.R")
source("modules/make_matrix.R")

#### data import ####
data <- make_matrix("data/data.txt")

#### VARモデルによる指定と結果の取得 ####
result_list <- trivar(data)
Ehat <- result_list$Ehat

#### なぜかEhatの各行の最初の1,2要素の平均とEhatの各行を結合している ####
q1 <- c(mean(Ehat[1, 1:2]), Ehat[1, ]) 
    # なぜEhat[1,1:2]で最初の二つの平均を取っている？
    # そして、Ehat[1,]と結合している？？
q2 <- c(mean(Ehat[2, 1:2]), Ehat[2, ])
q3 <- c(mean(Ehat[3, 1:2]), Ehat[3, ])

#### それぞれのデータの必要な長さを定義 ####
time <- 1975:2007
q1a <- numeric(length(time))
q2a <- numeric(length(time))
q3a <- numeric(length(time))

#### なんか平均を上記で定義した配列に格納している ####
for (i in 1:length(time)) {
  start_index <- 12 * (i - 1) + 1
  end_index <- 12 * i
  # なんで以下のようなことが必要なのか??
  q1a[i] <- mean(q1[start_index:end_index])
  q2a[i] <- mean(q2[start_index:end_index])
  q3a[i] <- mean(q3[start_index:end_index])
}

#### データフレームの作成 ####
dfq1a <- data.frame(time = time, q1a = q1a)
dfq2a <- data.frame(time = time, q1a = q2a)
dfq3a <- data.frame(time = time, q1a = q3a)

#### ggplotでプロット ####
figure2_ploter <- function(df, y_name, title) {
  result <- ggplot(df, aes(x = time, y = y_name)) +
    geom_line(col = 4) +
    geom_hline(yintercept = 0, color = 3) +
    ggtitle(title) +
    xlim(1975, 2007) +
    ylim(-1, 1)
  return(result)
}
pl1 <- figure2_ploter(dfq1a, q1a, "Oil Supply Shock")
pl2 <- figure2_ploter(dfq1a, q2a, "Aggregate Demand Shock")
pl3 <- figure2_ploter(dfq1a, q3a, "Oil-Apecific Demand Shock")

gridExtra::grid.arrange(
  pl1, 
  pl2, 
  pl3, 
  nrow = 3, 
  top=textGrob("Figure 2: The Historical Evolution of the Structural Shocks\n 1975-2007")
)


