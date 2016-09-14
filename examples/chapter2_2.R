rm(list=ls());gc()
library(quantmod)
library(TTR)
library(data.table)
library(PearsonDS)
library(PerformanceAnalytics)
library(fUnitRoots)
library(FGN)

getFX("AUD/USD",
      from = Sys.Date() - 365*2,
      to = Sys.Date())
plot(AUDUSD)


# 1. CADF Test for Cointegration ------------------------------------------


