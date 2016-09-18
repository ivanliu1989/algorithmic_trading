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
getFX("CAD/USD")
y=merge(AUDUSD,CADUSD)
trainlen = 250
lookback = 20
hedgeRatio = rep(NA, length(AUDUSD))
numUnits = rep(NA, length(AUDUSD))
long = y[,1]
short = y[,2]

for(t in (trainlen+1):nrow(y)){
  jo_t <- ca.jo(y[(t-trainlen):(t-1),], type="trace", ecdet="none", K=2) #Johansen test
  hedgeRatio[t] <- jo_t@V[2,1] # eigenvector
  
  # market value of a unit portfolio
  yport = y[(t-lookback+1):t, 1] * hedgeRatio[t] + y[(t-lookback+1):t, 2]
  ma = mean(yport)
  mstd = sd(yport)
  zScore = (yport[length(yport)] - ma)/mstd
  numUnits[t] = -(yport[length(yport)]-ma)/mstd
}

# positions are market values of AUDUSD and CADUSD in portfolio expressed
positions = numUnits * hedgeRatio * y

# daily P&L
pnl = rowSums(lag(positions, 1)*(y-lag(y,1))/lag(y,1))
ret = pnl/rowSums(abs(lag(positions, 1)))
ret[is.na(ret)] <- 0
APR <- prod(1+ret)**(252/length(ret)) - 1
Sharpe <- sqrt(252)*mean(ret)/sd(ret)
cat(paste0("APR=", APR, " Sharpe=",Sharpe))

yret <- as.vector(ROC(long, type = "discrete"))
yret[is.na(yret)] = 0
xret <- as.vector(ROC(short, type = "discrete"))
xret[is.na(xret)] = 0
retSys <- cbind(yret, xret, ret)[-1,]
colnames(retSys) <- c("short Daily Returns","long Daily Returns","Strategy Based")
retSys <- zoo(retSys,index(y)[-1])
retSys <- retSys[-c(1:trainlen)]
charts.PerformanceSummary(retSys,ylog=T,cex.legend=1.25,
                          colorset=c("cadetblue","darkolivegreen3", "red"))

