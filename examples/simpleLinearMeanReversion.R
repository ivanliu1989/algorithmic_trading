rm(list=ls());gc()
library(quantmod)
library(TTR)
library(data.table)
library(PearsonDS)
library(PerformanceAnalytics)
library(fUnitRoots)
library(FGN)
library(urca)

getSymbols("EWA")
getSymbols("EWC")
EWA <-EWA$EWA.Adjusted
index(EWA) <- as.Date(index(EWA))
EWC <-EWC$EWC.Adjusted
index(EWC) <- as.Date(index(EWC))

# determine half life -----------------------------------------------------
jo_t <- ca.jo(cbind(EWC, EWA), type="trace", ecdet="none", K=2) #test trace statistics
print(summary(jo_t))

## construct portfolio
spread <- EWC - 0.9067132 * EWA
chart.TimeSeries(spread)

## calculate half life of mean reversion
y <- spread
y.lag <- lag(y, 1)
delta.y <- diff(y)
df <- cbind(y, y.lag, delta.y)
df <- df[-1 ,] #remove first row with NAs
regress.results <- lm(delta.y ~ y.lag, data = df)
lambda <- summary(regress.results)$coefficients[2]
half.life <- -log(2)/lambda
# backtest
# setting lookback to the halflife found above
lookback=round(half.life)


# determine rolling hedge ratio -------------------------------------------
hedgeRatio = rep(1, length(EWA))

for(t in c(lookback:length(hedgeRatio))){
  regression_result <- lm(EWC[(t-lookback+1):t]~EWA[(t-lookback+1):t]+1)
  hedgeRatio[t] <- coef(regression_result)[2]
}
plot(hedgeRatio, type = 'l')

# The net market value of the portfolio
yport <- EWC * -hedgeRatio + EWA #spreads
yport <- yport[-c(1:lookback),]
EWA <- EWA[-c(1:lookback)]
EWC <- EWC[-c(1:lookback)]
charts.TimeSeries(yport)


# simple linear mean reversion strategy -----------------------------------
mktVal=-(yport-SMA(yport, lookback))/runSD(yport, lookback)
# positions = mktVal * -hedgeRatio[-c(1:lookback)] * EWA + mktVal * EWC
# daily P&L of the strategy 
pnl=lag(mktVal, 1)*(yport-lag(yport, 1))/lag(yport, 1); 
pnl[is.na(pnl)]=0 # profit & loss
# return is P%L divided by gross market value of portfolio
# ret = pnl/sum(abs(lag(positions, 1)), na.rm = T)
# ret[is.na(ret)]=0
# Cumulative P&L
plot(cumprod(1+ret)-1)
APR <- prod(1+ret)**(252/length(ret)) - 1
Sharpe <- sqrt(252)*mean(ret)/sd(ret)

plot(cumsum(pnl))
retSys <- merge(diff(yport), pnl)[-1,]
colnames(retSys) <- c("Daily Returns","Half-life Mean Reversion")
charts.PerformanceSummary(retSys,ylog=F,cex.legend=1.25,
                          colorset=c("cadetblue","darkolivegreen3"))



# bollinger band strategy -------------------------------------------------
entryZscore = 1
exitZscore = 0
zScore = (yport-SMA)

