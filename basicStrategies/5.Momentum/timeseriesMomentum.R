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

getFX("AUD/USD")
y = AUDUSD
for(lookback in c(1, 5, 10, 25, 60, 120)){
  for(holddays in c(1, 5, 10, 25, 60, 120)){
    ret_lag = ROC(y, n = lookback)
    ret_fut = (lag(y, -lookback)-y)/y
    badDates = is.na(ret_lag) | is.na(ret_fut)
    ret_lag = ret_lag[!badDates] 
    ret_fut = ret_fut[!badDates] 
    
    if(lookback >= holddays){
      indepSet = lookback:length(ret_lag)
    }else{
      indepSet = holddays:length(ret_lag)
    }
    
    ret_lag=ret_lag[indepSet]
    ret_fut=ret_fut[indepSet]
    
    corr = correlationTest(ret_lag, ret_fut)
    cc = corr@test$estimate
    p.value = corr@test$p.value[1]
    cat("\nlookback: ", lookback, " | holddays: ", holddays, " | Correlation: ", cc, " | pvalue: ", p.value)
    # ret_lag = momentum(y, n = lookback)
  } 
}

lookback = 250
holddays = 20

longs = y > lag(y, lookback)
shorts = y < lag(y, lookback)
positions = rep(0, length(y))

for(h in 0:(holddays-1)){
  long_lag = lag(longs, h)
  long_lag[is.na(long_lag)] <- FALSE
  short_lag = lag(shorts, h)
  short_lag[is.na(short_lag)] <- FALSE
  
  positions[long_lag] = positions[long_lag] + 1
  positions[short_lag] = positions[short_lag] -1
}

ret <- lag(positions, 1) * ROC(y) / holddays
yret <- ROC(y)
retSys <- cbind(as.vector(ret), as.vector(yret))[-1,]
colnames(retSys) <- c("Momentum", "Raw")
retSys <- zoo(retSys,index(y)[-1])
charts.PerformanceSummary(retSys,ylog=T,cex.legend=1.25,
                          colorset=c("cadetblue","darkolivegreen3"))
