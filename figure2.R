# 必要なライブラリの読み込み
library(ggplot2)
library(tidyverse)
library(openxlsx)
library(vars)
library(grid)
library(gridExtra)

# 別ファイルに定義した関数の読み込み
source("modules/trivar.R")
source("modules/make_matrix.R")

# データの読み込み
data <- make_matrix("data/data.txt")

# VARモデルによる指定と結果の取得
result_list <- trivar(data)
Ehat <- result_list$Ehat

# Ehatの各行の最初の1,2要素の平均と結合
q1 <- c(mean(Ehat[1, 1:2]), Ehat[1, ]) 
q2 <- c(mean(Ehat[2, 1:2]), Ehat[2, ])
q3 <- c(mean(Ehat[3, 1:2]), Ehat[3, ])

# 各データの必要な長さを定義
time <- 1975:2007
q1a <- numeric(length(time))
q2a <- numeric(length(time))
q3a <- numeric(length(time))

# 平均を上記で定義した配列に格納
for (i in 1:length(time)) {
  start_index <- 12 * (i - 1) + 1
  end_index <- 12 * i
  q1a[i] <- mean(q1[start_index:end_index])
  q2a[i] <- mean(q2[start_index:end_index])
  q3a[i] <- mean(q3[start_index:end_index])
}

# データフレームの作成
dfq1a <- data.frame(time = time, q1a = q1a)
dfq2a <- data.frame(time = time, q1a = q2a)
dfq3a <- data.frame(time = time, q1a = q3a)

# ggplotでプロットする関数
figure2_plotter <- function(df, y_name, title) {
  result <- ggplot(df, aes(x = time, y = y_name)) +
    geom_path(col = 4) +
    geom_hline(yintercept = 0, color = 3) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlim(1975, 2007) +
    ylim(-1, 1)
  return(result)
}

# 定義した関数を用いてプロット
pl1 <- figure2_plotter(dfq1a, q1a, "Oil Supply Shock")
pl2 <- figure2_plotter(dfq2a, q2a, "Aggregate Demand Shock")
pl3 <- figure2_plotter(dfq3a, q3a, "Oil-Specific Demand Shock")

# プロットをグリッド状に配置
gridExtra::grid.arrange(
  pl1, 
  pl2, 
  pl3, 
  nrow = 3, 
  top = textGrob(expression(bold("Figure 2: The Historical Evolution of the Structural Shocks 1975-2007")),
                 gp = gpar(col = "black", fontsize = 10)
  )
)