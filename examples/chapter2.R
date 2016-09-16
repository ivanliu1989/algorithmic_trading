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

# 1. Augmented Dickey-Fuller Test -----------------------------------------
adfTest(AUDUSD, type="nc") # not stationary

## Lets get first data  for EWA and EWC from yahoo finance and extract adjusted close prices
getSymbols("EWA")
getSymbols("EWC")
ewaAdj=unclass(EWA$EWA.Adjusted)
ewcAdj=unclass(EWC$EWC.Adjusted)

## Now lets do linear regression where we assume drift is zero. Since we are not sure which security is dependent and independent, we need to apply following for both case

## EWC is dependent here 
reg=lm(ewcAdj~ewaAdj+0)
## And now lets use adf test on spread (which is actually residuals of regression above step)
adfTest(reg$residuals, type="nc")
summary(ur.df(ewaAdj, type = "drift", lags = 1))

## EWA is dependent here this time
reg=lm (ewaAdj~ewcAdj+0)
adfTest(reg$residuals, type="nc") 
summary(ur.df(ewcAdj, type = "drift", lags = 1))

### Main function
cointegrationTestLM_ADF <-function(A, B, startDate) {
  cat("Processing stock:",A ," and ", B, " start date:",startDate)
  
  aData=getSymbols(A,from=startDate,auto.assign = FALSE)
  aAdj=unclass(aData[,6])
  bData=getSymbols(B,from=startDate,auto.assign = FALSE)
  bAdj=unclass(bData[,6])
  lenA=length(aAdj)
  lenB=length(bAdj)
  N= min(lenA,lenB) 
  startA=0
  startB=0
  if (lenA!=N || lenB!=N){
    startA=lenA-N+1
    startB=lenB-N+1
  }
  cat("\nIndex start",A,":",startA," Length ",lenA )
  cat("\nIndex start",B,":",startB," Length ",lenB)
  aAdj=aAdj[startA:lenA,]
  bAdj=bAdj[startB:lenB,]
  
  regA=lm(aAdj~bAdj+0)
  
  summary(regA)
  regB=lm(bAdj~aAdj+0)
  summary(regB)
  
  coA <- adfTest(regA$residuals, type="nc")
  coB=adfTest(regB$residuals, type="nc")   
  
  
  cat("\n",A," p-value",coA@test$p.value," statistics:",coA@test$statistic)     
  cat("\n",B," p-value",coB@test$p.value," statistics:",coB@test$statistic)     
  
  
  # Lets choice most negative
  if (coA@test$statistic < coB@test$statistic){
    cat("\nStock ",A, " is dependent on stock ",B)
    cat("\np-value",coA@test$p.value," statistics:",coA@test$statistic)     
    p=coA@test$p.value
    s=coA@test$statistic
  }else {
    cat("\n Stock ",B, " is dependent on stock:",A)
    cat("\n p-value",coB@test$p.value," statistics:",coB@test$statistic)     
    p=coB@test$p.value
    s=coB@test$statistic    
  }   
  return(c(s,p))
}

res=cointegrationTestLM_ADF("EWA","EWC",'2007-01-01')

# 2. Johansen Test --------------------------------------------------------
library("urca") # For cointegration 

coRes=ca.jo(data.frame(ewaAdj,ewcAdj),type="trace",K=2,ecdet="none", spec="longrun")
summary(coRes)

###################### 
# Johansen-Procedure # 
###################### 



# 3. Hurst Exponent -------------------------------------------------------
ret <- ROC(AUDUSD, n = 1,type = "discrete", na.pad = F)
hurstKmonthly <- apply.rolling(ret, FUN="HurstK", width = 12)

library(pracma)
hurst <- hurstexp(log(AUDUSD)) # returns a list of various Hurst calculations


# 4. Variance Ratio Test --------------------------------------------------
N <- 12
varN <- diff(AUDUSD, N)
var <- diff(AUDUSD, 1)
var(var, na.rm = T) / (N * var(varN, na.rm = T))
library(vrtest)
Auto.VR(AUDUSD)


# 5. Half-Life for Mean Reversion -----------------------------------------
ylag <- lag(audnzd, -1)
deltaY <- diff(audnzd, 1)
# ylag <- ylag[-1]
# deltaY <- deltaY[-1]
reg <- lm(deltaY ~ ylag)
summary(reg)
# plot(reg)
halflife <- -log(2) / reg$coefficients[2]
halflife


# 6. Backtesting a Linear Mean-Reverting Trading Strategy -----------------
y <- audnzd
# setting lookback to the halflife found above
lookback=round(halflife); 
#capital in number of shares invested in USDCAD. movingAvg and movingStd are functions from epchan.com/book2
mktVal=-(y-SMA(y, lookback))/runSD(y, lookback);
# daily P&L of the strategy 
pnl=lag(mktVal, 1)*(y-lag(y, 1))/lag(y, 1); 
pnl[is.na(pnl)]=0 # profit & loss
# Cumulative P&L
plot(cumsum(pnl))

retSys <- merge(deltaY, pnl)
colnames(retSys) <- c("Daily Returns","Half-life Mean Reversion")
charts.PerformanceSummary(retSys,ylog=TRUE,cex.legend=1.25,
                          colorset=c("cadetblue","darkolivegreen3"))
                          

















