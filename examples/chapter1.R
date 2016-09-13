rm(list=ls());gc()
library(quantmod)
library(TTR)
library(data.table)
library(PearsonDS)

getFX("AUD/USD",
      from = Sys.Date() - 365*2,
      to = Sys.Date())
AUDUSD

# Example 1.1: Hypothesis Testing on a Futures Momentum Strategy ----------
ret <- diff(AUDUSD, 1)[-1]
mean(ret)/sd(ret)*sqrt(length(ret))

moments <- empMoments(as.vector(ret))
## Generate sample with given (theoretical) moments

for(sample in 1:10000){
  ret.sim <- rpearson(length(ret),moments=c(mean=moments[1],variance=moments[2],skewness=moments[3],kurtosis=moments[4]))
  cl.sim <- cumprod(1+ret.sim) - 1
}