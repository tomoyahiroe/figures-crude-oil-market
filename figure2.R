#### Import libraries ####
library(ggplot2)
library(tidyverse)
library(openxlsx)
library(vars)
library(gridExtra)

#### data import ####
beagdp <- read.table("./data/beagdp.txt") # beaはアメリカ商務省
cpi <- read.table("./data/cpi.txt")
data <- read.table("./data/data.txt")
y <- as.matrix(data)

## 意訳版
# Time lineの作成
time <- seq(as.Date("1973-02-01"), as.Date("2007-12-01"), by = "1 month")
# インパルス応答の時間ホライズンとVARのラグオーダーの設定
h <- 15
p <- 24
# VARモデルを推定
var_model <- VAR(y, p = p, type = "const") # ラグオーダー p=24、定数項ありのVARモデルを推定
# 結果の表示
summary(var_model)
# A(係数行列)をどう求めるのか模索中
A <- coefficients(var_model)
Av1 <- var_model$varresult$V1$coefficients
Av2 <- var_model$varresult$V2$coefficients
Av3 <- var_model$varresult$V3$coefficients
A <- cbind(Av1, Av2, Av3)
A <- as.matrix(A)
class(A)
# 係数行列の取得
coefficients <- coef(var_model$varresult)
A <- coefficients[c("V1", "V2", "V3")]
Uhat <- residuals(var_model)
SIGMA <- cov(Uhat)
Ehat <- solve(t(chol(SIGMA))) %*% t(Uhat)

q1 <- c(mean(Ehat[1, 1:2]), Ehat[1, ])
q2 <- c(mean(Ehat[2, 1:2]), Ehat[2, ])
q3 <- c(mean(Ehat[3, 1:2]), Ehat[3, ])

time <- 1975:2007
q1a <- numeric(length(time))
q2a <- numeric(length(time))
q3a <- numeric(length(time))

for (i in 1:length(time)) {
  start_index <- 12 * (i - 1) + 1
  end_index <- 12 * i
  q1a[i] <- mean(q1[start_index:end_index])
  q2a[i] <- mean(q2[start_index:end_index])
  q3a[i] <- mean(q3[start_index:end_index])
}

q1

# データフレームの作成
dfq1a <- data.frame(time = time, q1a = q1a)
dfq2a <- data.frame(time = time, q1a = q2a)
dfq3a <- data.frame(time = time, q1a = q3a)

# ggplotでプロット
pl1 <- ggplot(dfq1a, aes(x = time, y = q1a)) +
  geom_line(col = 4) +
  geom_hline(yintercept = 0, color = 3) +
  labs(title = "Oil Supply Shock") +
  xlim(1975, 2007) +
  ylim(-1, 1)

# ggplotでプロット
pl2 <- ggplot(dfq1a, aes(x = time, y = q2a)) +
  geom_line(col = 4) +
  geom_hline(yintercept = 0, color = 3) +
  labs(title = "Aggregate Demand Shock") +
  xlim(1975, 2007) +
  ylim(-1, 1)

# ggplotでプロット
pl3 <- ggplot(dfq1a, aes(x = time, y = q3a)) +
  geom_line(col = 4) +
  geom_hline(yintercept = 0, color = 3) +
  labs(title = "Oil-Specific Demand Shock") +
  xlim(1975, 2007) +
  ylim(-1, 1)

gridExtra::grid.arrange(pl1, pl2, pl3, nrow = 3)
