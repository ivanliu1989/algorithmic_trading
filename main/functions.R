library(quantmod)
library(TTR)
library(data.table)
library(PearsonDS)
library(PerformanceAnalytics)
library(fUnitRoots)
library(FGN)
library(urca)

zscores <- function(t){
  tz <- (t-mean(t))/sd(t)
  return(tz)
}

fillMissingData <- function(v){
  for(i in 1:length(v)){
    v[i]<-ifelse(is.na(v[i]), v[i-1], v[i])
  }
  return(v)
}

kalmanInit <- function(n){
  delta = 0.0001
  Vw = delta/(1-delta)*diag(2)
  Ve = 0.001
  beta = matrix(0, nrow = 2, ncol = n)
  P = matrix(0, 2, 2)
  R = NULL
  e = rep(NA, n)
  Q = rep(NA, n)
  yhat = rep(NA, n)
  kalmanOjt = list(
    delta = delta,
    Vw = Vw,
    Ve = Ve,
    beta = beta,
    P = P,
    R = R,
    e = e,
    Q = Q,
    yhat = yhat,
    K = NULL
  )
  return(kalmanOjt)
}

kalmanFilter <- function(long, short, sdnum = 1){
  n = length(long)
  y = long
  x = cbind(short, ones = rep(1, n))
  kalman <- kalmanInit(n)
  for(t in 1:n){
    if(!is.null(kalman$R)){
      kalman$R = kalman$P + kalman$Vw
      kalman$beta[,t] = kalman$beta[,t-1]
    }else{
      kalman$R = kalman$P
    }
    
    kalman$yhat[t]=x[t,]%*%kalman$beta[,t] # measurement prediction. Equation 3.9
    
    kalman$Q[t] = x[t, ]%*%kalman$R%*%t(x[t,])+kalman$Ve # measurement variance prediction. Equation 3.10
    
    kalman$e[t]=y[t]-kalman$yhat[t] # measurement prediction error
    
    kalman$K=kalman$R%*%t(x[t,])/kalman$Q[t] # Kalman gain
    
    kalman$beta[,t] = kalman$beta[,t]+as.vector(kalman$K)*kalman$e[t] # State update. Equation 3.11
    kalman$P=kalman$R-kalman$K%*%(x[t,]%*%kalman$R) # State covariance update. Euqation 3.12
  }
  
  longsEntry=kalman$e < -sqrt(kalman$Q)*sdnum # a long position means we should buy long
  longsExit=kalman$e > -sqrt(kalman$Q)*sdnum
  shortsEntry=kalman$e > sqrt(kalman$Q)*sdnum
  shortsExit=kalman$e < sqrt(kalman$Q)*sdnum
  
  numUnitesLong = rep(NA, length(long))
  numUnitesShort = rep(NA, length(long))
  numUnitesLong[1] = 0
  numUnitesLong[longsEntry] = 1
  numUnitesLong[longsExit] = 0
  numUnitesLong <- fillMissingData(numUnitesLong)
  
  numUnitesShort[1] = 0
  numUnitesShort[shortsEntry] = -1
  numUnitesShort[shortsExit] = 0
  numUnitesShort <- fillMissingData(numUnitesShort)
  
  numUnits = numUnitesLong + numUnitesShort
  
  positions = merge(numUnits*long, numUnits*-kalman$beta[1,]*short)
  colnames(positions) = c("long", "short")
  
  res <- list(
    kalman = kalman,
    long = long,
    short = short,
    hedgeRatio = kalman$beta[1,],
    longsEntry = longsEntry,
    longsExit = longsExit,
    shortsEntry = shortsEntry,
    shortsExit = shortsExit,
    numUnits = numUnits,
    positions = positions
  )
  return(res)
}

JohansenEigenvector <- function(long, short, trainlen = 250, lookback = 20){
  y=merge(long,short)
  hedgeRatio = rep(NA, length(long))
  numUnits = rep(NA, length(long))
  
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
  numUnits[is.na(numUnits)] = 0
  positions = numUnits * hedgeRatio * y
  colnames(positions) = c("long", "short")
  
  res = list(
    hedgeRatio = hedgeRatio,
    numUnits = numUnits,
    positions = positions
  )
  return(res)
}


tsMomentum <- function(long, short, hedgeRatio, lookback=250, holddays=20){
  y = long - hedgeRatio * short
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
  
  res <- list(
    long_lag = long_lag,
    short_lag = short_lag,
    positions = positions
  )
  return(res)
}

hurstExpSystem <- function(long, short, hedgeRatio, lookback=20){
  y = long - hedgeRatio * short
  retY <- ROC(y, n=1, type="discrete")
  retY[is.na(retY)] <- 0
  hurstKY <- apply.rolling(retY, FUN="HurstK", width = lookback)
  
  serialcorr <- runCor(cbind(coredata(hurstKY)),cbind(index(hurstKY)),n=lookback)
  serialcorr <- as.xts(serialcorr,order.by=index(hurstKY))
  autoreg <- runCor(hurstKY,lag(hurstKY,k=1),n=lookback)
  colnames(serialcorr) <- "SerialCorrelation"
  colnames(autoreg) <- "AutoRegression"
  
  signalUpTrend <- runMean(hurstKY+serialcorr+autoreg,n=lookback/2) + (long/runMean(long,n=lookback)-1)*10
  signalUpTrend <- lag(signalUpTrend,k=1)
  signalUpTrend[is.na(signalUpTrend)] <- 0
  
  numUnits <- ifelse(signalUpTrend > 1, 1, 0)
  positions = merge(numUnits*long, numUnits*-hedgeRatio*short)
  colnames(positions) = c("long", "short")
  
  res <- list(
    signalUpTrend = signalUpTrend,
    hurstKY = hurstKY,
    numUnits = signalUpTrend
  )
  return(res)
}


rebalance <- function(long, short, numUnits, hedgeRatio){
  y = merge(numUnits * long, - numUnits * hedgeRatio * short)
  colnames(y) <- c("long","short")
  return(y)
}

backTests <- function(long, short, positions, from, to){
  y2 = merge(long, short)
  # daily P&L of the strategy 
  pnl=rowSums(lag(positions, 1)*(y2-lag(y2, 1))/lag(y2, 1)); 
  pnl[is.na(pnl)]=0 # profit & loss
  # return is P%L divided by gross market value of portfolio
  ret = pnl/rowSums(abs(lag(positions, 1)), na.rm = T)
  ret[is.na(ret)]=0
  ret2 <- zoo(ret,index(y2))
  ret2 <- ret2[index(ret2) >= as.Date(from) & index(ret2) <= as.Date(to)]
  APR <- prod(1+ret2)**(252/length(ret2)) - 1
  Sharpe <- sqrt(252)*mean(ret2)/sd(ret2)
  cat(paste0("APR=", APR, " Sharpe=",Sharpe))
  
  yret <- as.vector(ROC(long, type = "discrete"))
  yret[is.na(yret)] = 0
  xret <- as.vector(ROC(short, type = "discrete"))
  xret[is.na(xret)] = 0
  retSys <- cbind(yret, xret, ret)[-1,]
  colnames(retSys) <- c("short Daily Returns","long Daily Returns","Strategy Based")
  retSys <- zoo(retSys,index(y2)[-1])
  retSys <- retSys[index(retSys) >= as.Date(from) & index(retSys) <= as.Date(to),]
  charts.PerformanceSummary(retSys,ylog=T,cex.legend=1.25,
                            colorset=c("cadetblue","darkolivegreen3", "red"))
}


basicTests <- function(long, short, trainlen = 250, lookback=20, sdnum = 2){
  n = length(long)
  Pairs <- merge(long, short)
  colnames(Pairs) <- c("long", "short")
  
  # 1. Augmented Dickey-Fuller Test Unit Root
  fit <- lm(long~short)
  hedgeRatio <- coef(fit)[2]
  resi <- long-hedgeRatio * short
  adf.test <- summary(ur.df(resi, type = "drift", lags = 1))
  
  # 2. Total least square regression
  r <- princomp(~ long + short)
  beta_TLS <- r$loadings[1,1] / r$loadings[2,1] # I think we also need a non-zero intercept in both cases
  resid_TLS <- long - beta_TLS * short
  colnames(resid_TLS) <- "residual"
  adf.test.TLS <- summary(ur.df(resid_TLS, type = "drift", lags = 1))
  
  # 3. Half-life
  y <- long[(n-trainlen):n] - hedgeRatio * short[(n-trainlen):n]
  y.lag <- lag(y, 1)
  delta.y <- diff(y)
  df <- cbind(y, y.lag, delta.y)
  df <- df[-1 ,] #remove first row with NAs
  regress.results <- lm(delta.y ~ y.lag, data = df)
  lambda <- summary(regress.results)$coefficients[2]
  half.life <- -log(2)/lambda
  lookback=round(half.life)
  
  # 4. Rolling hedge ratio
  hedgeRatio = rep(1, length(long))
  for(t in c(lookback:length(hedgeRatio))){
    regression_result <- lm(long[(t-lookback+1):t]~short[(t-lookback+1):t]+1)
    hedgeRatio[t] <- coef(regression_result)[2]
  }
  
  # 5. JO test
  jo.test <- summary(ca.jo(cbind(long, short), type="trace", ecdet="none", K=2)) #test trace statistics
  
  # 6. Bollinger band strategy
  yport <- long - hedgeRatio * short
  zScore <- zscores(yport)
  bollingerUp <- sdnum * runSD(yport, lookback)
  bollingerLow <- -sdnum * runSD(yport, lookback)
  bollingerUpZ <- sdnum * runSD(zScore, lookback)
  bollingerLowZ <- -sdnum * runSD(zScore, lookback)
  
  res <- list(
    adf.test = adf.test,
    adf.test.TLS = adf.test.TLS,
    jo.test = jo.test,
    hedgeRatio = hedgeRatio,
    half.life = half.life,
    yport = yport,
    zScore = zScore,
    bollingerUp = bollingerUp,
    bollingerLow = bollingerLow,
    bollingerUpZ = bollingerUpZ,
    bollingerLowZ = bollingerLowZ
  )
}


dashboardOne <- function(long, short, hedgeRatio, half.life, entryExit){
  par(mfcol=c(4,1))
  entryExit <- entryExit[-c(1:(half.life+1))]
  dates <- index(long)[-c(1:(half.life+1))]
  entryExitPoints <- dates[entryExit == 1 | entryExit == -1]
  entryExitColor <- ifelse(entryExitPoints %in% dates[entryExit == 1], "green", "orange")
  Pairs <- merge(long,short)
  colnames(Pairs) <- c("long", "short")
  yport <- long - hedgeRatio * short
  # Series
  chart.TimeSeries(Pairs[-c(1:(half.life+1)),], ylog=FALSE,cex.legend=1.25,main = "Raw Series",
                   legend.loc = "topleft",ylab = NA,
                   colorset=c("cadetblue","darkolivegreen3"))
  
  # Hedge ratio
  chart.TimeSeries(hedgeRatio[-c(1:(half.life+1)),], ylog=FALSE,cex.legend=1.25,main = "Rolling Hedge Ratio",
                   ylab = NA,colorset=c("darkblue"))
  
  # Series / hedge ratio
  PairsAjs <- cbind(Pairs, yport)
  PairsAjs[,2] <- -PairsAjs[,2] * hedgeRatio
  PairsAjs <- PairsAjs[-c(1:(half.life+1)),]
  colnames(PairsAjs) <- c("long", "short", "spread")
  chart.TimeSeries(PairsAjs, ylog=FALSE, cex.legend=1.25,main = "Long, Short & Spread",
                   legend.loc = "topleft",ylab = NA,
                   event.lines = entryExitPoints,event.color = entryExitColor,
                   colorset=c("cadetblue","darkolivegreen3","red"))
  
  # Spread
  PairsAjs2 <- PairsAjs
  PairsAjs2[,2] <- -PairsAjs2[,2]
  PairsAjs2 = ROC(PairsAjs2, n = 1, type = "discrete")[-1,]
  chart.CumReturns(PairsAjs2, ylog=FALSE, cex.legend=1.25,main = "CumReturns of Long, Short & Spread",
                   legend.loc = "topleft",ylab = NA,
                   colorset=c("cadetblue","darkolivegreen3","red"))
}