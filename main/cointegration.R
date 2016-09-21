rm(list=ls());gc()
library(quantmod)
library(TTR)
library(data.table)
library(PearsonDS)
library(PerformanceAnalytics)
library(fUnitRoots)
library(FGN)
library(urca)
source("main/functions.R")


# 0. Load data and calculate correlations ---------------------------------
getSymbols("EWA")
getSymbols("EWC")
long = EWC$EWC.Adjusted # high
short = EWA$EWA.Adjusted # low

getMetals("XAU")
getMetals("XAG")
short = XAUUSD/100 #gold
long = XAGUSD #silver



# 1. Check Correlations ---------------------------------------------------
chart.Correlation(merge(long,short))

# 2. Stationary -----------------------------------------------------------
# ADF
summary(ur.df(long, type = "drift", lags = 1))
summary(ur.df(short, type = "drift", lags = 1))
summary(ur.df(diff(long)[-1], type = "drift", lags = 1))
summary(ur.df(diff(short)[-1], type = "drift", lags = 1))

# 3. Cointegration --------------------------------------------------------
summary(ca.jo(cbind(long, short), type="trace", ecdet="none", K=2))

basicTest <- basicTests(long, short)
hedgeRatio <- zoo(basicTest$hedgeRatio, index(long))
half.life <- round(basicTest$half.life)

# 4. Price Diff -----------------------------------------------------------
OLShedge <- function(long, short, window){
  pair = merge(long, short)
  colnames(pair) <- c("long","short")
  
  dolm <- function(x) coef(lm(long ~ ., data = as.data.frame(x)))
  hedgeRatio <- rollapplyr(pair, window, dolm, by.column = FALSE)
  colnames(hedgeRatio) <- c("intercept", "hedgeRatio")
  
  dt <- merge(pair, hedgeRatio)
  dt$pred.long <- dt$intercept + dt$hedgeRatio * dt$short
  
  dt$spread <- dt$long-dt$hedgeRatio*dt$short
  
  dt$mean = rollapplyr(dt$spread, window, mean)
  dt$std = rollapplyr(dt$spread, window, sd)
  dt$BBhigh = dt$mean + dt$std
  dt$BBlow = dt$mean - dt$std
  return(dt[-c(1:(2*window)),])
}

dt <- OLShedge(long, short, half.life)

# chart.TimeSeries(dt[,c("spread", "mean", "BBhigh", "BBlow")])
# 1.spreadprice大于7.814597时，卖空差价，即卖空long，买入short。
# 2.spreadprice小于4.693253时，买入差价，即买入long，卖空short。
# 3.spreadprice靠近零时，平仓


# 5. Strategy -------------------------------------------------------------
dif <- lag(dt,1)
dif[1,] <- 0
longsEntry= dif$spread < dif$BBlow 
longsExit= dif$spread > dif$BBhigh #| dif < 0.1
shortsEntry= dif$spread > dif$BBhigh
shortsExit= dif$spread < dif$BBlow #| dif < 0.1

numUnitesLong = rep(NA, length(dt$long))
numUnitesShort = rep(NA, length(dt$long))
numUnitesLong[1] = 0
numUnitesLong[longsEntry] = 1
numUnitesLong[longsExit] = 0
numUnitesLong <- fillMissingData(numUnitesLong)

numUnitesShort[1] = 0
numUnitesShort[shortsEntry] = 1
numUnitesShort[shortsExit] = 0
numUnitesShort <- fillMissingData(numUnitesShort)

capital = 100000

positions = merge(numUnitesLong*dt$long, dt$hedgeRatio * numUnitesShort*dt$short)
colnames(positions) = c("long", "short")
backTests(dt$long, dt$short, positions, Sys.Date()-100000, Sys.Date())
