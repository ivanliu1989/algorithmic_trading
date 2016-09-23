rm(list=ls());gc()

source("lib/utilities_pipeline.R")


# 0. Load data and calculate correlations ---------------------------------
getFX("AUD/USD")
getFX("CAD/USD")
getFX("NZD/USD")


# 1. CAD vs AUD -----------------------------------------------------------
long = CADUSD
short = AUDUSD

context = initialize(long, short, capital = 1000000, window = 20, lookback = 500)
init.test <- handle_data(context)

res = backtesting(context, init.test)


# 2. NZD vs AUD -----------------------------------------------------------
long = NZDUSD 
short = AUDUSD 

context = initialize(long, short, 1000000, 20, 500)
init.test <- handle_data(context)

res = backtesting(context, init.test)



# 3. CAD vs NZD -----------------------------------------------------------
long = CADUSD 
short = NZDUSD 

context = initialize(long, short, 1000000, 20, 500)
init.test <- handle_data(context)

res = backtesting(context, init.test)
