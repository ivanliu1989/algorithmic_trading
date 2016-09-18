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

# 2. Stationary test - ADF ------------------------------------------------
fit <- lm(long~short+1)
resi = long - coef(fit)[2] * short# - coef(fit)[3] * JPYUSD
adf_t <- ur.df(resi, type = "drift", lags = 1)
summary(adf_t) # 12%

# 3. Cointegration test ---------------------------------------------------
jo_t <- ca.jo(cbind(long, short), type="trace", ecdet="none", K=2) #test trace statistics
summary(jo_t) 

# 4. Plot cointegration pairs ---------------------------------------------
chart.TimeSeries(merge(long, short))

# 5. Spread of returns ----------------------------------------------------
spread_ret <- long - short
chart.TimeSeries(spread_ret, main = "Spred returns")
spread_zscore <- zscores(spread_ret)
chart.TimeSeries(merge(spread_zscore, 1, -1), main = "Spred returns")

# 6. Static Hedging -------------------------------------------------------
y = long
x = short
fit <- lm(y~x)
alpha <- coef(fit)[1]
beta <- coef(fit)[2]
spreads <- y - beta*x
retSys <- merge(spreads, long, short)
retSys <- diff(log(retSys))
colnames(retSys) <- c("Strategy based", "long", "short")
charts.PerformanceSummary(retSys,ylog=T,cex.legend=1.25,
                          colorset=c("red","cadetblue","darkolivegreen3"))

# 7. Kalman Filter Hedging ------------------------------------------------
kalman <- kalmanFilter(long, short)
hedgeRatio = kalman$kalman$beta[1,]
# plot(kalman$kalman$beta[2,], type = 'l') # intercept
# plot(kalman$kalman$beta[1,], type = 'l') # slope
# plot(kalman$kalman$e[-c(1,2)], type = "l")
# plot(sqrt(kalman$kalman$Q[-c(1,2)]), type = "l")

y2=cbind(kalman$short, kalman$long)

# variance
longsEntry=kalman$kalman$e < -sqrt(kalman$kalman$Q) # a long position means we should buy long
longsExit=kalman$kalman$e > -sqrt(kalman$kalman$Q)
shortsEntry=kalman$kalman$e > sqrt(kalman$kalman$Q)
shortsExit=kalman$kalman$e < sqrt(kalman$kalman$Q)

numUnitesLong = rep(NA, length(kalman$long))
numUnitesShort = rep(NA, length(kalman$long))
numUnitesLong[1] = 0
numUnitesLong[longsEntry] = 1
numUnitesLong[longsExit] = 0
numUnitesLong <- fillMissingData(numUnitesLong)

numUnitesShort[1] = 0
numUnitesShort[shortsEntry] = -1
numUnitesShort[shortsExit] = 0
numUnitesShort <- fillMissingData(numUnitesShort)

numUnits = numUnitesLong + numUnitesShort

positions = merge(numUnits*-hedgeRatio*kalman$short, numUnits*kalman$long)
# daily P&L of the strategy 
pnl=rowSums(lag(positions, 1)*(y2-lag(y2, 1))/lag(y2, 1)); 
pnl[is.na(pnl)]=0 # profit & loss
# return is P%L divided by gross market value of portfolio
ret = pnl/rowSums(abs(lag(positions, 1)), na.rm = T)
ret[is.na(ret)]=0
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
charts.PerformanceSummary(retSys,ylog=T,cex.legend=1.25,
                          colorset=c("cadetblue","darkolivegreen3", "red"))
