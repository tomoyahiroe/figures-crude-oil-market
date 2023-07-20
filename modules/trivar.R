#### trivar関数の定義 ####
trivar <- function(matrix) {
  
  # Time lineの作成
  time <- seq(as.Date("1973-02-01"), as.Date("2007-12-01"), by = "1 month")
  
  #### インパルス応答の時間ホライズンとVARのラグオーダーの設定 ####
  h <- 15 # インパルス応答の時間ホライズン
  p <- 24 # ラグ変数
  
  #### VARモデルを推定 ####
  var_model <- VAR(matrix, p = p, type = "const") # ラグオーダー p=24、定数項ありのVARモデルを推定
  
  #### 標準化された残差Ehatを求める ####
  Uhat <- residuals(var_model) # 残差
  SIGMA <- cov(Uhat) # 残差の共分散行列
  Ehat <- solve(t(chol(SIGMA))) %*% t(Uhat) # 標準化された残差
  
  #### 推定結果や変数のリストをreturn ####
  return(list(var_model=var_model, Uhat = Uhat, Ehat = Ehat, SIGMA = SIGMA, time=time, h=h, p=p))
}
