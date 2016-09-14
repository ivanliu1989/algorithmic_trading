rm(list=ls());gc()
library(quantmod)
library(TTR)
library(data.table)
library(PearsonDS)
library(PerformanceAnalytics)
library(fUnitRoots)
library(FGN)

getSymbols("EWA")
getSymbols("EWC")
plot(EWA)
plot(EWC)

# 1. CADF Test for Cointegration ------------------------------------------
EWA <-EWA$EWA.Adjusted
index(EWA) <- as.Date(index(EWA))
EWC <-EWC$EWC.Adjusted
index(EWC) <- as.Date(index(EWC))
Pairs <- merge(EWA, EWC)
colnames(Pairs) <- c("EWA", "EWC")
# Time-series
chart.TimeSeries(Pairs, ylog=TRUE,cex.legend=1.25,
                          colorset=c("cadetblue","darkolivegreen3"))
# Correlation
chart.Correlation(Pairs, ylog=TRUE,cex.legend=1.25,
                  colorset=c("cadetblue","darkolivegreen3"))
# Residuals of Linear Regression