rm(list = ls()); gc()
library(data.table)
library(xgboost)

data <- fread('data/AUD DATA.csv')
names(data) <- gsub(" ", "_", names(data))
data[, AUDDiff := (AUD_1_Day_Lag - Australian_Dollar_Spot)>0]
data[, c(1,2,3,4) := NULL, with = F]
data <- data[!is.na(AUDDiff)]

train <- data.matrix(data[1:1000])
validation <- data.matrix(data[1001:1400])
test <- data.matrix(data[1401:1825])

dtrain <- xgb.DMatrix(train[, 1:20], label = train[,"AUDDiff"], missing = NaN)
dval <- xgb.DMatrix(validation[, 1:20], label = validation[,"AUDDiff"], missing = NaN)
dtest <- xgb.DMatrix(test[, 1:20], label = test[,"AUDDiff"], missing = NaN)

watchlist <- list(eval = dval, train = dtrain)

param <- list(
  max.depth = 6, 
  eta = 0.01,
  min_child_weight = 10,
  subsample = 0.5,
  colsample_bytree = 0.5,
  booster = "gbtree",
  objective="binary:logistic",
  eval_metric="auc")

bst <- xgb.train(param, dtrain, nrounds = 1000, verbose = 1, watchlist, 
                 print.every.n = 10,
                 early.stop.round = 50)

summary(train[, "AUDDiff"])
var(train[, "AUDDiff"])

pred <- predict(bst, dtest)
predictions <- ifelse(pred >= 0.5, 1, 0)
mean(predictions == test[,"AUDDiff"])

xgb.importance(names(data), model = bst)




# Get Rich ----------------------------------------------------------------
library(quantmod)
getSymbols(c("AMZN"))
barChart(AMZN,theme='white.mono',bar.type='hlc')

getSymbols(c("^GSPC"))
chartSeries(GSPC, subset='last 3 months')
addBBands(n = 20, sd = 2, ma = "SMA", draw = 'bands', on = -1)



Nasdaq100_Symbols <- c("AAPL", "ADBE", "ADI", "ADP", "ADSK", "AKAM", "ALXN", #"ALTR", "BRCM", "CTRX", "DTV", 
                       "AMAT", "AMGN", "AMZN", "ATVI", "AVGO", "BBBY", "BIDU", "BIIB",
                       "CA", "CELG", "CERN", "CHKP", "CHRW", "CHTR", "CMCSA",
                       "COST", "CSCO", "CTSH", "CTXS", "DISCA", "DISCK", "DISH",
                       "DLTR", "EBAY", "EQIX", "ESRX", "EXPD", "EXPE", "FAST",
                       "FB", "FFIV", "FISV", "FOXA", "GILD", "GMCR", "GOOG", "GOOGL",
                       "GRMN", "HSIC", "ILMN", "INTC", "INTU", "ISRG", "KLAC", "KRFT",
                       "LBTYA", "LLTC", "LMCA", "LMCK", "LVNTA", "MAR", "MAT", "MDLZ",
                       "MNST", "MSFT", "MU", "MXIM", "MYL", "NFLX", "NTAP", "NVDA",
                       "NXPI", "ORLY", "PAYX", "PCAR", "PCLN", "QCOM", "QVCA", "REGN",
                       "ROST", "SBAC", "SBUX", "SIAL", "SIRI", "SNDK", "SPLS", "SRCL",
                       "STX", "SYMC", "TRIP", "TSCO", "TSLA", "TXN", "VIAB", "VIP",
                       "VOD", "VRSK", "VRTX", "WDC", "WFM", "WYNN", "XLNX", "YHOO")
getSymbols(Nasdaq100_Symbols)

nasdaq100 <- data.frame(as.xts(merge(AAPL, ADBE, ADI, ADP, ADSK, AKAM, #ALTR, 
                                     ALXN, AMAT, AMGN, AMZN, ATVI, AVGO, BBBY, BIDU, BIIB,
                                     BRCM, CA, CELG, CERN, CHKP, CHRW, CHTR, CMCSA,
                                     COST, CSCO, CTRX, CTSH, CTXS, DISCA, DISCK, DISH,
                                     DLTR, DTV, EBAY, EQIX, ESRX, EXPD, EXPE, FAST,
                                     FB, FFIV, FISV, FOXA, GILD, GMCR, GOOG, GOOGL,
                                     GRMN, HSIC, ILMN, INTC, INTU, ISRG, KLAC, KRFT,
                                     LBTYA, LLTC, LMCA, LMCK, LVNTA, MAR, MAT, MDLZ,
                                     MNST, MSFT, MU, MXIM, MYL, NFLX, NTAP, NVDA,
                                     NXPI, ORLY, PAYX, PCAR, PCLN, QCOM, QVCA, REGN,
                                     ROST, SBAC, SBUX, SIAL, SIRI, SNDK, SPLS, SRCL,
                                     STX, SYMC, TRIP, TSCO, TSLA, TXN, VIAB, VIP,
                                     VOD, VRSK, VRTX, WDC, WFM, WYNN, XLNX, YHOO)))
head(nasdaq100[,1:12],2)