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
y2 <- merge(EWA,EWC)
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
fillMissingData <- function(v){
  for(i in 1:length(v)){
    v[i]<-ifelse(is.na(v[i]), v[i-1], v[i])
  }
  return(v)
}
entryZscore = 1
exitZscore = 0
zScore = (yport-SMA(yport, lookback))/runSD(yport, lookback)
longsEntry = zScore < -entryZscore
longsExit = zScore >= -exitZscore
shortsEntry = zScore > entryZscore
shortsExit = zScore <= exitZscore

numUnitesLong = rep(NA, length(yport))
numUnitesShort = rep(NA, length(yport))
numUnitesLong[1] = 0
numUnitesLong[longsEntry] = 1
numUnitesLong[longsExit] = 0
numUnitesLong <- fillMissingData(numUnitesLong)

numUnitesShort[1] = 0
numUnitesShort[shortsEntry] = -1
numUnitesShort[shortsExit] = 0
numUnitesShort <- fillMissingData(numUnitesShort)

numUnits = numUnitesLong + numUnitesShort
positions = merge(numUnits*-hedgeRatio[-c(1:lookback)]*EWA, numUnits*EWC)
# daily P&L of the strategy 
pnl=rowSums(lag(positions, 1)*(y2-lag(y2, 1))/lag(y2, 1)); 
pnl[is.na(pnl)]=0 # profit & loss
# return is P%L divided by gross market value of portfolio
ret = pnl/rowSums(abs(lag(positions, 1)), na.rm = T)
ret[is.na(ret)]=0
# Cumulative P&L
plot(cumprod(1+ret)-1)
APR <- prod(1+ret)**(252/length(ret)) - 1
Sharpe <- sqrt(252)*mean(ret)/sd(ret)
cat(paste0("APR=", APR, " Sharpe=",Sharpe))

EWCret <- as.vector(ROC(EWC, type = "discrete"))
EWCret[is.na(EWCret)] = 0
EWAret <- as.vector(ROC(EWA, type = "discrete"))
EWAret[is.na(EWAret)] = 0
retSys <- cbind(EWAret, EWCret, ret)[-1,]
colnames(retSys) <- c("EWA Daily Returns","EWC Daily Returns","Strategy Based")
retSys <- zoo(retSys,index(EWA)[-1])
charts.PerformanceSummary(retSys,ylog=T,cex.legend=1.25,
                          colorset=c("cadetblue","darkolivegreen3", "red"))



# Kalman Filter Mean Reversion Strategy -----------------------------------
getSymbols("EWA")
getSymbols("EWC")
EWA <-EWA$EWA.Adjusted
index(EWA) <- as.Date(index(EWA))
EWC <-EWC$EWC.Adjusted
index(EWC) <- as.Date(index(EWC))

x = cbind(EWA, rep(1, length(EWA)))
y = EWC

delta=0.0001 # delta=1 gives fastest change in beta, delta=0.000....1 allows no change (like traditional linear regression).

yhat=rep(NA, length(EWC)) # measurement prediction
e=rep(NA, length(EWC)) # measurement prediction error
Q=rep(NA, length(EWC)) # measurement prediction error variance

# For clarity, we denote R(t|t) by P(t).
# initialize R, P and beta.
R=c(0,0);
P=c(0,0);
beta=matrix(NA, nrow = 2, ncol = nrow(x))
Vw=delta/(1-delta)*diag(2)
Ve=0.001

# Initialize beta(:, 1) to zero
beta[,1]=0

# Given initial beta and R (and P)
for(t in 1:length(y)){
  if(t>1){
    beta[,t] = beta[,t-1] # state prediction. Equation 3.7
    R=P+Vw # state covariance prediction. Equation 3.8
  }
  yhat[t]=x[t,]%*%beta[,t] # measurement prediction. Equation 3.9
  Q[t]=(x[t,]%*%R)%*%t(x[t,])+Ve # measurement variance prediction. Equation 3.10
  
  # Observe y[t]
  e[t]=y[t]-yhat[t] # measurement prediction error
  K=R*x[t,]/Q[t] # Kalman gain
  
  beta[,t]=beta[,t]+K*e[t] # State update. Equation 3.11
  P=R-K*x[t,]*R # State covariance update. Euqation 3.12
}
plot(beta[1,])
plot(beta[2,])
plot(e[-c(1,2)])
plot(sqrt(Q[-c(1,2)]))

y2=cbind(x[,1], y)

longsEntry=e < -sqrt(Q) # a long position means we should buy EWC
longsExit=e > -sqrt(Q)
shortsEntry=e > sqrt(Q)
shortsExit=e < sqrt(Q)












