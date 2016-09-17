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
  kalmanOjt = list(
    delta = delta,
    Vw = Vw,
    Ve = Ve,
    beta = beta,
    P = P,
    R = R,
    e = rep(NA, n),
    Q = rep(NA, n)
  )
  return(kalmanOjt)
}

kalmanFilter <- function(long, short){
  n = length(long)
  y = long
  x = cbind(short, ones = rep(1, n))
  kalman <- kalmanInit(n)
  for(t in 1:n){
    if(!is.null(kalman$R)){
      kalman$R = kalman$P + kalman$Vw
    }else{
      kalman$R = kalman$P
    }
    yhat[t]=x[t,]%*%kalman$beta[,t] # measurement prediction. Equation 3.9
    kalman$Q[t] = x[t, ]%*%kalman$R%*%t(x[t,])+kalman$Ve # measurement variance prediction. Equation 3.10
    sqrt_Q = sqrt(kalman$Q[t])
    kalman$e[t]=y[t]-yhat[t] # measurement prediction error
    K=kalman$R%*%t(x[t,])/kalman$Q[t] # Kalman gain
    kalman$beta[, t] = kalman$beta[,t]+as.vector(K)*e[t] # State update. Equation 3.11
    kalman$P=kalman$R-K%*%(x[t,]%*%kalman$R) # State covariance update. Euqation 3.12
  }
  res <- list(
    kalman = kalman,
    long = long,
    short = short
  )
  return(res)
}

kalman <- kalmanFilter(AUDUSD,CADUSD)
kalman <- kalmanFilter(CADUSD,AUDUSD)

hedgeRatio = kalman$kalman$beta[1,]
# plot(hedgeRatio, type = "l")
# 
# plot(kalman$kalman$beta[1,])
# plot(kalman$kalman$beta[2,])
# plot(kalman$kalman$e[-c(1,2)])
# plot(sqrt(kalman$kalman$Q[-c(1,2)]))

y2=cbind(kalman$short, kalman$long)

longsEntry=e < -sqrt(kalman$kalman$Q) # a long position means we should buy CADUSD
longsExit=e > -sqrt(kalman$kalman$Q)
shortsEntry=e > sqrt(kalman$kalman$Q)
shortsExit=e < sqrt(kalman$kalman$Q)

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
# Cumulative P&L
# plot(cumprod(1+ret)-1)
APR <- prod(1+ret)**(252/length(ret)) - 1
Sharpe <- sqrt(252)*mean(ret)/sd(ret)
cat(paste0("APR=", APR, " Sharpe=",Sharpe))

CADUSDret <- as.vector(ROC(CADUSD, type = "discrete"))
CADUSDret[is.na(CADUSDret)] = 0
AUDUSDret <- as.vector(ROC(AUDUSD, type = "discrete"))
AUDUSDret[is.na(AUDUSDret)] = 0
retSys <- cbind(AUDUSDret, CADUSDret, ret)[-1,]
colnames(retSys) <- c("AUDUSD Daily Returns","CADUSD Daily Returns","Strategy Based")
retSys <- zoo(retSys,index(AUDUSD)[-1])
charts.PerformanceSummary(retSys,ylog=T,cex.legend=1.25,
                          colorset=c("cadetblue","darkolivegreen3", "red"))
