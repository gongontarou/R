
# ARIMAモデルによる株価の予測 | Logics of Blue
# http://logics-of-blue.com/stock-price-forecast-by-arima/
# 2017年7月14日
# 馬場真哉

# -----------------------------------------------------------------------
# 株価の取得
# -----------------------------------------------------------------------

# 株価取得関数の読み込み
#source("http://logics-of-blue.com/wp-content/uploads/2017/07/getstockData.txt")

# みずほHGの株価を取得する
mizuho1 <- read.csv("/Users/nakamuratatsuya/Documents/GitHub/R/ARIMAモデル/8411_2016.csv",header = TRUE)
mizuho2 <- read.csv("/Users/nakamuratatsuya/Documents/GitHub/R/ARIMAモデル/8411_2017.csv",header = TRUE)
mizuho2<- mizuho2[-1,]

mizuho<- rbind(mizuho1,mizuho2)

#class(mizuho)
# install.packages("quantmod")
library(quantmod)

# xts形式に変換(1列目は証券コードなので削除)
mizuho_xts <- as.xts(read.zoo(mizuho))

# ローソク足のグラフを描く
chartSeries(
  mizuho_xts, 
  type="candlesticks"
)


# 対数差分系列にする
log_diff <- diff(log(mizuho_xts$close))[-1]

# 訓練データとテストデータに分ける
train <- log_diff["::2017-06-30"]

test <- log_diff["2017-07-01::2017-07-13"]

# 終値
#train$close
#test$close

# -----------------------------------------------------------------------
# ARIMAモデルによる予測
# -----------------------------------------------------------------------

# install.packages("forecast")
library(forecast)

# ARIMAモデルの推定
model_arima <- auto.arima(
  train,ic="aic",
  stepwise=T,
  approximation=T,
  max.p=10,
  max.q=10,
  max.order=20,
  parallel=T,
  num.cores=2)



# 推定結果
model_arima

# 予測
f_arima <- forecast(model_arima, h=9)

# ナイーブな予測も併せて作成
f_rw <- rwf(train)
f_mean <- meanf(train)

# 予測精度の比較
accuracy(f_arima, test)
accuracy(f_rw, test)
accuracy(f_mean, test)

# 図示
f_arima_xts <- xts(f_arima$mean, order.by = index(test))

plot(log_diff["2017-06::"], main="みずほHG終値の対数差分系列")
lines(f_arima_xts, col=2, lwd=2)

# -----------------------------------------------------------------------
# 1期先を連続して予測する
# -----------------------------------------------------------------------

# データを入れると、1期先の予測をしてくれる関数
calcForecast  <- function(data){
  model <- Arima(data, order=c(1,0,0))
  return(forecast(model,h=1)$mean)
}

# 訓練データの長さ
length(train)

## 一期先予測
f_arima_2 <- rollapply(log_diff, 367, calcForecast)
# 1期先の予測値なので、実際の日付とずれている。
# lagを使って実際の日付に合わせる
f_arima_2 <- lag(f_arima_2)
# NAを消す
f_arima_2<- f_arima_2[!is.na(f_arima_2)]

# 平均値予測
f_mean_2 <- rollapply(log_diff, 367, mean)
# 1期先の予測値なので、実際の日付とずれている。
# lagを使って実際の日付に合わせる
f_mean_2 <- lag(f_mean_2)
# NAを消す
f_mean_2 <- f_mean_2[!is.na(f_mean_2)]
f_mean_2

# 1期前予測
f_rw_2 <- lag(log_diff["2017-06-30::"])
f_rw_2<- f_rw_2[!is.na(f_rw_2)]


# 予測精度
accuracy(as.ts(f_arima_2), test)
accuracy(as.ts(f_mean_2), test)
accuracy(as.ts(f_rw_2), test)


# 図示

# 描画の文字設定をmac用に変更
par(family = "HiraKakuProN-W3")

plot(log_diff["2017-06::"], main="みずほHG終値の対数差分系列")
lines(f_arima_2, col=2, lwd=2)
lines(f_mean_2, col=4, lwd=2)
lines(f_rw_2, col=5, lwd=2)





