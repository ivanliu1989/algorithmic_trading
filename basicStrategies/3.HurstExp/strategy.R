rm(list=ls());gc()
library(quantmod)
library(TTR)
library(data.table)
library(PearsonDS)
library(PerformanceAnalytics)
library(fUnitRoots)
library(FGN)
library(urca)
source("1.Kalman Filter/kalmanFilter.R")

# 1. Load data and calculate correlations ---------------------------------
getFX("AUD/USD")
getFX("CAD/USD")
getFX("JPY/USD")
getSymbols("EWA")
getSymbols("EWC")
long = EWC$EWC.Adjusted
short = EWA$EWA.Adjusted
chart.Correlation(merge(short, long)); correlationTest(short, long)

lookback = 20
retLong <- ROC(long, n=1, type="discrete")
retShort <- ROC(short, n=1, type="discrete")
hurstKlong <- apply.rolling(retLong, FUN="HurstK", width = lookback)
hurstKshort <- apply.rolling(retShort, FUN="HurstK", width = lookback)

serialcorr <- runCor(cbind(coredata(hurstKlong)),cbind(index(hurstKlong)),n=lookback)
serialcorr <- as.xts(serialcorr,order.by=index(hurstKlong))
autoreg <- runCor(hurstKlong,lag(hurstKlong,k=1),n=lookback)
colnames(serialcorr) <- "SerialCorrelation"
colnames(autoreg) <- "AutoRegression"
signalUpTrend <- runMean(hurstKlong+serialcorr+autoreg,n=lookback/2) + (long/runMean(long,n=lookback)-1)*10

chart.TimeSeries(signalUpTrend)

signalUpTrend <- lag(signalUpTrend,k=1)

retSys <- merge(ifelse(signalUpTrend > 1, 1, 0) * retLong,retLong)
colnames(retSys) <- c("HurstUp System","Raw")
charts.PerformanceSummary(retSys,ylog=TRUE,cex.legend=1.25,
                          colorset=c("cadetblue","darkolivegreen3"))
