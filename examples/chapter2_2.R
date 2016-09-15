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
getSymbols("EWP")
plot(EWA)
plot(EWC)
AUDUSD <- read.csv("Mean-reversion-2/AUDUSD.csv")
NZDUSD <- read.csv("Mean-reversion-2/NZDUSD.csv")

# 1. CADF Test for Cointegration ------------------------------------------
EWA <-EWA$EWA.Adjusted
index(EWA) <- as.Date(index(EWA))
EWC <-EWC$EWC.Adjusted
index(EWC) <- as.Date(index(EWC))
EWP <-EWP$EWP.Adjusted
index(EWP) <- as.Date(index(EWP))
Pairs <- merge(EWA, EWC)
colnames(Pairs) <- c("EWA", "EWC")
# Time-series
chart.TimeSeries(Pairs, ylog=TRUE,cex.legend=1.25,
                          colorset=c("cadetblue","darkolivegreen3"))
# Correlation
chart.Correlation(Pairs, ylog=TRUE,cex.legend=1.25,
                  colorset=c("cadetblue","darkolivegreen3"))
# Residuals of ordinary least squares regression
fit <- lm(EWC~EWA + 1)
summary(fit)
hedgeRatio <- fit$coefficients[2]
resi <- EWC-hedgeRatio*EWA
chart.TimeSeries(resi)
## test stationarity of the spread
library(urca)
summary(ur.df(resi, type = "drift", lags = 1))

## total least squares regression
r <- princomp(~ EWC + EWA)
beta_TLS <- r$loadings[1,1] / r$loadings[2,1] # I think we also need a non-zero intercept in both cases

resid_TLS <- EWC - beta_TLS * EWA
colnames(resid_TLS) <- "residual"
chart.TimeSeries(resid_TLS)

summary(ur.df(resid_TLS, type = "drift", lags = 1))


# 2. Johansen test --------------------------------------------------------
jo_t <- ca.jo(cbind(EWC, EWA), type="trace", ecdet="none", K=2) #test trace statistics
jo_e <- ca.jo(cbind(EWC, EWA), type="eigen", ecdet="none", K=2) # eigenvalue test statistics
print(summary(jo_e))

## construct portfolio
spread <- EWC - 0.9074647 * EWA
chart.TimeSeries(spread)

## calculate half life of mean reversion
y <- spread
y.lag <- lag(y, -1)
delta.y <- diff(y)
df <- cbind(y, y.lag, delta.y)
df <- df[-1 ,] #remove first row with NAs
regress.results <- lm(delta.y ~ y.lag, data = df)
lambda <- summary(regress.results)$coefficients[2]
half.life <- log(2)/lambda
# backtest
# setting lookback to the halflife found above
lookback=round(half.life); 
#capital in number of shares invested in USDCAD. movingAvg and movingStd are functions from epchan.com/book2
mktVal=-(y-SMA(y, lookback))/runSD(y, lookback);
# daily P&L of the strategy 
pnl=lag(mktVal, 1)*(y-lag(y, 1))/lag(y, 1); 
pnl[is.na(pnl)]=0 # profit & loss
# Cumulative P&L
plot(cumsum(pnl))
retSys <- merge(delta.y, pnl)[-1,]
colnames(retSys) <- c("Daily Returns","Half-life Mean Reversion")
charts.PerformanceSummary(retSys,ylog=F,cex.legend=1.25,
                          colorset=c("cadetblue","darkolivegreen3"))



# 3. Mean reversion of a portfolio of more than two instruments -----------
## add third pair to portfolio
jo_3_t <- ca.jo(cbind(EWA, EWC, EWP), type="trace", ecdet="none", K=2)
print(summary(jo_3_t))
jo_3_e <- ca.jo(cbind(EWA, EWC, EWP), type="eigen", ecdet="none", K=2) 
print(summary(jo_3_e))

## check stationary and hedge ratio
spread3 <- EWA - 1.1486224 * EWC + 0.1471721 * EWP
plot.zoo(spread3,  col = 'darkgreen', xlab = 'Date', ylab = 'Spread from johansen eigenvector')

## calculate half life
y <- spread3
y.lag <- lag(y, -1)
delta.y <- diff(y)
df <- cbind(y, y.lag, delta.y)
df <- df[-1 ,] #remove first row with NAs
regress.results <- lm(delta.y ~ y.lag, data = df)
lambda <- summary(regress.results)$coefficients[2]
half.life <- log(2)/lambda
print(half.life)

# backtest
# setting lookback to the halflife found above
lookback=round(half.life); 
#capital in number of shares invested in USDCAD. movingAvg and movingStd are functions from epchan.com/book2
mktVal=-(y-SMA(y, lookback))/runSD(y, lookback);
# daily P&L of the strategy 
pnl=lag(mktVal, 1)*(y-lag(y, 1))/lag(y, 1); 
pnl[is.na(pnl)]=0 # profit & loss
# Cumulative P&L
plot(cumsum(pnl))
retSys <- merge(delta.y, pnl)[-1,]
colnames(retSys) <- c("Daily Returns","Half-life Mean Reversion")
charts.PerformanceSummary(retSys,ylog=F,cex.legend=1.25,
                          colorset=c("cadetblue","darkolivegreen3"))


## subset price data to reproduce Chan's results (Example 5.1 in Algorithmic Trading)
jo_3a_t <- ca.jo(cbind(EWA, EWC), type="trace", ecdet="none", K=2)
print(summary(jo_3a_t))

spread3a <- EWA - 1.101971 * EWC
plot.zoo(spread3a,  col = 'deeppink', xlab = 'Date', ylab = 'Spread from johansen eigenvector')

y <- spread3a
y.lag <- lag(y, -1)
delta.y <- diff(y)
df <- cbind(y, y.lag, delta.y)
df <- df[-1 ,] #remove first row with NAs
regress.results <- lm(delta.y ~ y.lag, data = df)
lambda <- summary(regress.results)$coefficients[2]
half.life <- log(2)/lambda
print(half.life)







