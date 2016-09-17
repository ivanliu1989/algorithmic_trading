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

kalman <- kalmanInit(length(y))

for(t in 1:length(y)){
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

plot(kalman$beta[1,])
plot(kalman$beta[2,])
plot(kalman$e[-c(1,2)])
plot(sqrt(kalman$Q[-c(1,2)]))

y2=cbind(x[,1], y)

longsEntry=e < -sqrt(Q) # a long position means we should buy CADUSD
longsExit=e > -sqrt(Q)
shortsEntry=e > sqrt(Q)
shortsExit=e < sqrt(Q)

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
