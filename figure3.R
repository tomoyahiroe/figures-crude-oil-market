library(openxlsx)
setwd("C:/Users/nagi0/Downloads")
oilrowdata <- read.xlsx("Data_oil_1.xlsx")
​
# githubから拾ってきたoildata
oildata <- oilrowdata[2:420, c(3,4,6)]
date <- seq(as.Date("1975-12-01"), by = "month", length.out = 396)
# oildata$date <- date
masterdata <- cbind(oildata, data)
​
## 求める方向性が違いそう
# インパルス応答の時間ホライズンとVARのラグオーダーの設定
h <- 15
p <- 24
​
# VARモデルを推定
var_modelf3 <- VAR(masterdata, p = p, type = "const")
​
Uhatf3 <- residuals(var_modelf3)
SIGMAf3 <- cov(Uhatf3)
​
Ehatf3 <- solve(t(chol(SIGMAf3))) %*% t(Uhatf3)
​
#　そもそもここから怪しい
wti <- c(mean(Ehatf3[1, 1:2]), Ehatf3[1, ])
oilpd <- c(mean(Ehatf3[2, 1:2]), Ehatf3[2, ])
kildx <- c(mean(Ehatf3[3, 1:2]), Ehatf3[3, ])
q1f3 <- c(mean(Ehatf3[4, 1:2]), Ehatf3[4, ])
q2f3 <- c(mean(Ehatf3[5, 1:2]), Ehatf3[5, ])
q3f3 <- c(mean(Ehatf3[6, 1:2]), Ehatf3[6, ])
​
​
## わけわからんものが求まる
## そもそもインパルスリ応答はランダムが絡むので運要素あり
## ただそれでもおかしいと思われる
​
## CI 0.68と0.95両方図に入れたいけど、片方しか入らん
# oilsupply-oilpid インパルス応答を求めるコード
irf_result <- irf(var_modelf3, impulse = "V1",
                  response = "Oil_Prod",
                  ortho = TRUE,
                  n.ahead =  10, 
                  ci = 0.95,
                  cummulative = TRUE,
                  runs = 300)
​
plot(irf_result)
​
# agredemand-oilpid インパルス応答を求めるコード
irf_result2 <- irf(var_modelf3, impulse = "V2",
                   response = "Oil_Prod",
                   ortho = TRUE,
                   n.ahead =  10, 
                   ci = 0.68,
                   cummulative = TRUE,
                   runs = 300)
​
plot(irf_result2)
​
# spedemand-oilpid インパルス応答を求めるコード
irf_result3 <- irf(var_modelf3, impulse = "V3",
                   response = "Oil_Prod",
                   ortho = TRUE,
                   n.ahead =  10, 
                   ci = 0.68,
                   cummulative = TRUE,
                   runs = 300)
​
plot(irf_result3)