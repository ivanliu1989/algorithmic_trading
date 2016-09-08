rm(list = ls()); gc()
library(data.table)
set.seed(1234)

data <- fread("data/EUvarsD1.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
data <- fread("C:/Users/sky_x/Desktop/ANZ Option Model/Model from Felipe/AUD DATA.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
names(data) <- gsub(" ", "_", names(data))
pri <- data$Australian_Dollar_Spot


generateFeatures <- function(pri){
  library(TTR)
  library(zoo)
  # 1-day log return
  logReturn1 <- log(shift(pri, 1, type = "lag")) - log(pri)
  logReturn5 <- log(shift(pri, 5, type = "lag")) - log(pri)
  logReturn10 <- log(shift(pri, 10, type = "lag")) - log(pri)
  logReturn20 <- log(shift(pri, 20, type = "lag")) - log(pri)
  
  # - Trend deviation: the logarithm of the closing price divided by the lowpass filtered price
  trendDeviarion <- function(x, n){
    y = x / SMA(x,n)
    return(y)
  }
  zscore <- function(x, n){
    y = SMA(x, n) / rollapplyr(x, n, sd, fill = NA, partial = F, align = "right")
  }
  trendPriceSlow <- log(trendDeviarion(pri, 100))
  trendPriceFast <- log(trendDeviarion(pri, 20))
  trendPrice <- trendPriceFast - trendPriceSlow
  trendNorm <- rollapplyr(pri, 50, scale, fill = NA, partial = F, align = "right")
  trehdPriceZscore <- zscore(pri, 50)
  
  # - Momentum: the price today relative to the price x days ago, normalized by the standard deviation of daily price changes.
  momPrice3
  momPrice5
  momPrice10
  
  ### - ATR: the average true range of the price series
  
  # - Velocity: a one-step ahead linear regression forecast on closing prices
  velocityPrice3 <- rollapplyr(pri, 3,
                               FUN = function(x){
                                 fit = lm(x~index(x))
                                 tail(predict(fit), 1)
                               }, partial = F, align = "right")
  velocityPrice5 <- rollapplyr(pri, 5,
                               FUN = function(x){
                                 fit = lm(x~index(x))
                                 tail(predict(fit), 1)
                               }, partial = F, align = "right")
  velocityPrice10 <- rollapplyr(pri, 10,
                               FUN = function(x){
                                 fit = lm(x~index(x))
                                 tail(predict(fit), 1)
                               }, partial = F, align = "right")
  
  # - Linear forecast deviation: the difference between the most recent closing price and the closing price predicted by a linear regression line
  # - Price variance ratio: the ratio of the variance of the log of closing prices over a short time period to that over a long time period.
  # - Delta price variance ratio: the difference between the current value of the price variance ratio and its value x periods ago.
  # - The Market Meanness Index: A measure of the likelihood of the market being in a state of mean reversion, created by the Financial Hacker.
  # - MMI deviation: The difference between the current value of the Market Meanness Index and its value x periods ago.
  # - The Hurst exponenet
  # - ATR ratio: the ratio of an ATR of a short (recent) price history to an ATR of a longer period.
  # - Delta ATR ratio: the difference between the current value of the ATR ratio and the value xbars ago.
  # - Bollinger width: the log ratio of the standard deviation of closing prices to the mean of closing prices, that is a moving standard deviation of closing prices relative to the moving average of closing prices.
  # - Delta bollinger width: the difference between the current value of the bollinger width and its value x bars ago.
  # - Exogenous variables, such as economic indicators, the price histories of other instruments and the like
}


# linear - slope, 3,5,10
# diff linear
# momentum - 3, 5, 10
# diff momentum
# price / MA
# linear forcast
# abs changes
# variance - 50
# diff variance - 50
# Bollinger width
# MMI, Hurst exponent, 