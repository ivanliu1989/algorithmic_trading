rm(list=ls());gc()
library(quantmod)
library(TTR)
library(data.table)
library(PearsonDS)

getFX("AUD/USD",
      from = Sys.Date() - 365*2,
      to = Sys.Date())
AUDUSD
lookback=250;
holddays=25;

# Example 1.1: Hypothesis Testing on a Futures Momentum Strategy ----------
# 1. Gaussian hypothesis test
longs= AUDUSD > shift(AUDUSD, lookback);
shorts= AUDUSD < shift(AUDUSD, lookback);

pos=rep(0, length(AUDUSD));
for(h in 0:(holddays-1)){
  long_lag = shift(longs, h)
  long_lag[is.na(long_lag)] <- FALSE
  # long_lag <- ifelse(long_lag, 1, 0)
  
  short_lag = shift(shorts, h)
  short_lag[is.na(short_lag)] <- FALSE
  # short_lag <- ifelse(short_lag, 1, 0)
  
  pos[long_lag] <- pos[long_lag]+1;
  pos[short_lag] <- pos[short_lag]-1;
}

marketRet <- diff(AUDUSD, 1) / shift(AUDUSD, 1)
marketRet[is.na(marketRet)] <- 0
marketRet[is.infinite(marketRet)] <- 0

ret=shift(pos, 1)*marketRet/holddays;
ret[is.na(ret)] <- 0

mean(ret)/sd(ret)*sqrt(length(ret))

# 2. Randomized market returns hypothesis test
moments <- empMoments(as.vector(ret))
numSampleAvgretBetterOrEqualObserved <- 0
for(sample in 1:10000){
  marketRet.sim <- rpearson(length(ret),moments=c(mean=moments[1],variance=moments[2],skewness=moments[3],kurtosis=moments[4]))
  cl.sim <- cumprod(1+marketRet.sim) - 1
  
  longs.sim <- cl.sim > shift(cl.sim, lookback)
  shorts.sim <- cl.sim < shift(cl.sim, lookback)
  
  pos.sim <- rep(0, length(cl.sim))
  for(h in 0:(holddays-1)){
    long_sim_lag = shift(longs.sim, h)
    long_sim_lag[is.na(long_sim_lag)] <- FALSE
    
    short_sim_lag = shift(shorts.sim, h)
    short_sim_lag[is.na(short_sim_lag)] <- FALSE
    
    pos.sim[long_sim_lag] <- pos.sim[long_sim_lag]+1
    pos.sim[short_sim_lag] <- pos.sim[short_sim_lag]-1
  }
  
  ret.sim <- shift(pos.sim,1)*marketRet.sim/holddays
  ret.sim[is.infinite(ret.sim)] <- 0
  ret.sim[is.na(ret.sim)] <- 0
  
  if (mean(ret.sim)>= mean(ret))
    numSampleAvgretBetterOrEqualObserved <- numSampleAvgretBetterOrEqualObserved+1
}

cat(paste0("p-value: ", numSampleAvgretBetterOrEqualObserved/10000)) #0.4443

# 3. Randomized entry trades hypothesis test
numSampleAvgretBetterOrEqualObserved <- 0

for(sample in 1:100000){
  P=sample(1:length(longs), length(longs))
  longs_sim=longs[P]
  shorts_sim=shorts[P]
  
  pos_sim <- rep(0, length(AUDUSD))
  for(h in 0:(holddays-1)){
    long_sim_lag = shift(longs.sim, h)
    long_sim_lag[is.na(long_sim_lag)] <- FALSE
    
    short_sim_lag = shift(shorts.sim, h)
    short_sim_lag[is.na(short_sim_lag)] <- FALSE
    
    pos.sim[long_sim_lag] <- pos.sim[long_sim_lag]+1
    pos.sim[short_sim_lag] <- pos.sim[short_sim_lag]-1
  }
  
  ret.sim <- shift(pos.sim,1)*marketRet.sim/holddays
  ret.sim[is.infinite(ret.sim)] <- 0
  ret.sim[is.na(ret.sim)] <- 0
  
  if (mean(ret.sim)>= mean(ret))
    numSampleAvgretBetterOrEqualObserved <- numSampleAvgretBetterOrEqualObserved+1
}
cat(paste0("p-value: ", numSampleAvgretBetterOrEqualObserved/100000)) #0.4443