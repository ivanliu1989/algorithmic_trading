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

kalmanFilter <- function(long, short){
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
  res <- list(
    kalman = kalman,
    long = long,
    short = short
  )
  return(res)
}
