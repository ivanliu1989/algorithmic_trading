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
johansen(cbind(long, short), L=1) 
"johansen"<- function(x, L = 2){ 
  #Johansen Test of cointegration for multivariate time series x 
  #Returns vector of eigenvalues after that you are on your own. 
  #This is a modified version for R, in which rts is substituted by ts. 
  x <- ts(x) 
  n <- nrow(x) 
  p <- ncol(x) 
  Ly <- lag(x[, 1], -1) 
  D <- diff(x[, 1]) 
  for(i in 1:p) { 
    if(i > 1) { 
      D <- ts.intersect(D, diff(x[, i])) 
      Ly <- ts.intersect(Ly, lag(x[, i], -1)) 
    } 
    if(L > 0) 
      for(j in 1:L) 
        D <- ts.intersect(D, lag(diff(x[, i]),  - j)) 
  } 
  iys <- 1 + (L + 1) * (0:(p - 1)) 
  Y <- D[, iys] 
  X <- D[,  - iys] 
  Ly <- ts.intersect(Ly, D)[, 1:p] 
  ZD <- lm(Y ~ X)$resid 
  ZL <- lm(Ly ~ X)$resid 
  df <- nrow(X) - ncol(X) - 1 
  S00 <- crossprod(ZD)/df 
  S11 <- crossprod(ZL)/df 
  S01 <- crossprod(ZD, ZL)/df 
  M <- solve(S11) %*% t(S01) %*% solve(S00) %*% S01 
  eigen(M)$values 
} 



# 4. Price Diff -----------------------------------------------------------
mean=mean(long-short)
std=sd(long-short)
S1 = mean
S2 = mean + std
S3 = mean - std
chart.TimeSeries(cbind(long-short, S1, S2, S3))
# 1.spreadprice大于7.814597时，卖空差价，即卖空long，买入short。
# 2.spreadprice小于4.693253时，买入差价，即买入long，卖空short。
# 3.spreadprice靠近零时，平仓


# 5. Strategy -------------------------------------------------------------
longsEntry= long-short < S3 
longsExit= long-short > S2 | abs(long-short) < 0.1
shortsEntry= long-short > S2
shortsExit= long-short < S3 | abs(long-short) < 0.1

numUnitesLong = rep(NA, length(long))
numUnitesShort = rep(NA, length(long))
numUnitesLong[1] = 0
numUnitesLong[longsEntry] = 1
numUnitesLong[longsExit] = 0
numUnitesLong <- fillMissingData(numUnitesLong)

numUnitesShort[1] = 0
numUnitesShort[shortsEntry] = -1
numUnitesShort[shortsExit] = 0
numUnitesShort <- fillMissingData(numUnitesShort)

numUnits = numUnitesLong + numUnitesShort

positions = merge(numUnits*long, -numUnits*short)
colnames(positions) = c("long", "short")
backTests(long, short, positions, Sys.Date()-100000, Sys.Date())
