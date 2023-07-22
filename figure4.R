# 必要なライブラリの読み込み
library(vars)
library(ggplot2)
library(grid)
library(gridExtra)

# 別ファイルに定義した関数の読み込み
source("modules/trivar.R")   # inputは行列データ、outputはvar_model, Uhat, Ehat, SIGMA, time, h, p
source("modules/make_matrix.R")   # inputはファイルパス、outputは行列化されたデータ

# 読み込んだ関数の実行
data_matrix <- make_matrix("data/data.txt")
trivar_res <- trivar(data_matrix)

# varsパッケージのirf関数で、インパルス応答関数を得る
IRF <- irf(
  trivar_res$var_model,
  impulse = colnames(trivar_res$Uhat),
  response = colnames(trivar_res$Uhat),
  boot = FALSE,
  n.ahead = length(trivar_res$time) - trivar_res$p - 1
)

# 構造的ショックEhatを計算
Ehat <- solve(t(chol(trivar_res$SIGMA))) %*% t(trivar_res$Uhat)

# 応答変数の予測値をそれぞれ時系列順に格納
yhat1 <- yhat2 <- yhat3 <- rep(0, length(trivar_res$time) - trivar_res$p)
for(i in 1:(length(trivar_res$time) - trivar_res$p)){
  yhat1[i] <- sum(IRF$irf$V1[1:i,1] * Ehat[1, i:1])  
  yhat2[i] <- sum(IRF$irf$V2[1:i,2] * Ehat[2, i:1])  
  yhat3[i] <- sum(IRF$irf$V3[1:i,3] * Ehat[3, i:1])  
}

# データをプロット
# プロットするためにデータフレームを作成
plot_data1 <- data.frame(
  time = trivar_res$time[(trivar_res$p + 1):length(trivar_res$time)],
  yhat = yhat1
)
plot_data2 <- data.frame(
  time = trivar_res$time[(trivar_res$p + 1):length(trivar_res$time)],
  yhat = yhat2
)
plot_data3 <- data.frame(
  time = trivar_res$time[(trivar_res$p + 1):length(trivar_res$time)],
  yhat = yhat3
)

# プロットのロジックを関数化
figure4_plotter <- function(plot_data, yhat, subtitle) {
  result <- ggplot(plot_data, aes(x = time)) +
    geom_path(aes(y = yhat), color = 'blue', linewidth = 1) +
    ylim(-100, 100) +
    labs(title = subtitle,
         y = "",                   
         x = "") +
    theme(plot.title = element_text(hjust = 0.5))
  return(result)
}

# 定義した関数を実行
pl1 <- figure4_plotter(plot_data1, yhat1, "Cumulative Effect of Oil Supply Shock on Real Price of Crude Oil")
pl2 <- figure4_plotter(plot_data2, yhat2, "Cumulative Effect of Aggregate Demand Shock on Real Price of Crude Oil")
pl3 <- figure4_plotter(plot_data3, yhat3, "Cumulative Effect of Oil-Market Specific Demand Shock on Real Price of Crude Oil")

# 縦一列にプロット
gridExtra::grid.arrange(
  pl1, 
  pl2, 
  pl3, 
  nrow = 3, 
  top = textGrob(
    expression(
      bold("Historical Decomposition of Real Price of Oil 1976.1-2007.12")),
    gp = gpar(col = "black", fontsize = 20)
  )
)
