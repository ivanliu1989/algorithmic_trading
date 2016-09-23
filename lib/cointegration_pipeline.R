rm(list=ls());gc()

source("lib/utilities_pipeline.R")


# 0. Load data and calculate correlations ---------------------------------
getFX("AUD/USD")
getFX("CAD/USD")
getFX("NZD/USD")


# 1. CAD vs AUD -----------------------------------------------------------
long = CADUSD # high
short = AUDUSD # low

context = initialize(long, short, 1000000, 20, 500)
init.test <- handle_data(context)

res = backtesting(context, init.test)
# entryExit <- ifelse(res$trade.summary$longsEntry == 1, 1, ifelse(res$trade.summary$shortsEntry == 1, -1, 0))
# dashboardSummary(long, short, init.test$hedgeRatio, 249, entryExit)


# 2. NZD vs AUD -----------------------------------------------------------
long = NZDUSD # high
short = AUDUSD # low

context = initialize(long, short, 1000000, 20, 500)
init.test <- handle_data(context)

backtesting(context, init.test)



# 3. CAD vs NZD -----------------------------------------------------------
long = CADUSD # high
short = NZDUSD # low

context = initialize(long, short, 1000000, 20, 500)
init.test <- handle_data(context)

backtesting(context, init.test)
