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
getMetals("XAU")
getMetals("XAG")
short = XAUUSD/100 #gold
long = XAGUSD #silver

# 1. Check Correlations ---------------------------------------------------
chart.Correlation(merge(long,short))
chart.TimeSeries(long)
chart.TimeSeries(short)


# 2. 基于价格比率的均值 ------------------------------------------------------------
pair <- merge(long,short)
colnames(pair) <- c("long", "short")
pair$pairRatio = pair$long / pair$short
pair$pairRatioMean = mean(pair$pairRatio)
pair$pairRatioHighSigma = mean(pair$pairRatio) + sd(pair$pairRatio)
pair$pairRatioLowSigma = mean(pair$pairRatio) - sd(pair$pairRatio)
chart.TimeSeries(pair[,c(1,2)])
chart.TimeSeries(pair[,-c(1,2)])
# 理想操作：在比价波动超过1个标准差时，买入1份“便宜”股票，卖空同等金额比价异常高的股票


# 2.2 Performance ---------------------------------------------------------
dif <- lag(long/short)
dif[1] <- 0
longsEntry= dif < unique(pair$pairRatioLowSigma)
longsExit= dif > unique(pair$pairRatioHighSigma) | abs(long-short) - unique(pair$pairRatioMean) < 0.1
shortsEntry= dif > unique(pair$pairRatioLowSigma)
shortsExit= dif < unique(pair$pairRatioLowSigma) | abs(long-short) - unique(pair$pairRatioMean) < 0.1

numUnitesLong = rep(NA, length(long))
numUnitesShort = rep(NA, length(long))
numUnitesLong[1] = 0
numUnitesLong[longsEntry] = 1
numUnitesLong[longsExit] = 0
numUnitesLong <- fillMissingData(numUnitesLong)

numUnitesShort[1] = 0
numUnitesShort[shortsEntry] = 1
numUnitesShort[shortsExit] = 0
numUnitesShort <- fillMissingData(numUnitesShort)

numUnits = numUnitesLong + numUnitesShort

positions = merge(numUnitesLong*long, numUnitesShort*short)
colnames(positions) = c("long", "short")
backTests(long, short, positions, Sys.Date()-100000, Sys.Date())



# 3. 基于价格之间的协整关系 ----------------------------------------------------------
# 3.2 Stationary -----------------------------------------------------------
# ADF
summary(ur.df(long, type = "drift", lags = 1))
summary(ur.df(short, type = "drift", lags = 1))
summary(ur.df(diff(long)[-1], type = "drift", lags = 1))
summary(ur.df(diff(short)[-1], type = "drift", lags = 1))

# 3.3 Cointegration --------------------------------------------------------
summary(ca.jo(cbind(long, short), type="trace", ecdet="none", K=2)) # no cointegration


# 3.4 hedge ratio ---------------------------------------------------------
mean=mean(long-short)
std=sd(long-short)
S1 = mean
S2 = mean + std
S3 = mean - std
chart.TimeSeries(cbind(long-short, S1, S2, S3))
# 1.spreadprice大于7.814597时，卖空差价，即卖空long，买入short。
# 2.spreadprice小于4.693253时，买入差价，即买入long，卖空short。
# 3.spreadprice靠近零时，平仓

# 3.5 Strategy ------------------------------------------------------------
dif <- lag(long-short)
dif[1] <- 0
longsEntry= dif < S3 
longsExit= dif > S2 | abs(abs(long-short) - mean) < 0.1
shortsEntry= dif > S2
shortsExit= dif < S3 | abs(abs(long-short) - mean)< 0.1

numUnitesLong = rep(NA, length(long))
numUnitesShort = rep(NA, length(long))
numUnitesLong[1] = 0
numUnitesLong[longsEntry] = 1
numUnitesLong[longsExit] = 0
numUnitesLong <- fillMissingData(numUnitesLong)

numUnitesShort[1] = 0
numUnitesShort[shortsEntry] = 1
numUnitesShort[shortsExit] = 0
numUnitesShort <- fillMissingData(numUnitesShort)

numUnits = numUnitesLong + numUnitesShort

positions = merge(numUnitesLong*long, numUnitesShort*short)
colnames(positions) = c("long", "short")
backTests(long, short, positions, Sys.Date()-100000, Sys.Date())
