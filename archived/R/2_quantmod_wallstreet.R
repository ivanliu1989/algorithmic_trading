library(quantmod)

# display a simple bar chart
getSymbols(c("AMZN"))
barChart(AMZN,theme='white.mono',bar.type='hlc') 

# display a complex chart
getSymbols(c("^GSPC"))
chartSeries(GSPC, subset='last 3 months')
addBBands(n = 20, sd = 2, ma = "SMA", draw = 'bands', on = -1)

# get market data for all symbols making up the Nasdaq 100 index
Nasdaq100_Symbols <- c('AAL','AAPL','ADBE','ADI','ADP','ADSK','AKAM','ALXN','AMAT','AMGN','AMZN','ATVI',
                       'AVGO','BBBY','BIDU','BIIB','BMRN','CA','CELG','CERN','CHKP','CHTR','CMCSA','COST',
                       'CSCO','CSX','CTRP','CTSH','CTXS','DISCA','DISCK','DISH','DLTR','EA','EBAY','ESRX',
                       'EXPE','FAST','FB','FISV','FOX','FOXA','GILD','GOOG','GOOGL','HSIC','ILMN','INCY','INTC',
                       'INTU','ISRG','JD','KHC','LBTYA','LBTYK','LLTC','LRCX','LVNTA','MAR','MAT','MCHP','MDLZ',
                       'MNST','MSFT','MU','MXIM','MYL','NCLH','NFLX','NTAP','NTES','NVDA','NXPI','ORLY','PAYX',
                       'PCAR','PCLN','PYPL','QCOM','QVCA','REGN','ROST','SBAC','SBUX','SIRI','SRCL','STX','SWKS',
                       'SYMC','TMUS','TRIP','TSCO','TSLA','TXN','ULTA','VIAB','VOD','VRSK','VRTX','WBA','WDC','WFM',
                       'XLNX','XRAY','YHOO')
getSymbols(Nasdaq100_Symbols)

# merge them all together
nasdaq100 <- data.frame(as.xts(merge(AAL,AAPL,ADBE,ADI,ADP,ADSK,AKAM,ALXN,AMAT,AMGN,AMZN,ATVI,AVGO,BBBY,
                                     BIDU,BIIB,BMRN,CA,CELG,CERN,CHKP,CHTR,CMCSA,COST,CSCO,CSX,CTRP,CTSH,CTXS,
                                     DISCA,DISCK,DISH,DLTR,EA,EBAY,ESRX,EXPE,FAST,FB,FISV,FOX,FOXA,GILD,GOOG,GOOGL,
                                     HSIC,ILMN,INCY,INTC,INTU,ISRG,JD,KHC,LBTYA,LBTYK,LLTC,LRCX,LVNTA,MAR,MAT,MCHP,
                                     MDLZ,MNST,MSFT,MU,MXIM,MYL,NCLH,NFLX,NTAP,NTES,NVDA,NXPI,ORLY,PAYX,PCAR,PCLN,
                                     PYPL,QCOM,QVCA,REGN,ROST,SBAC,SBUX,SIRI,SRCL,STX,SWKS,SYMC,TMUS,TRIP,TSCO,TSLA,
                                     TXN,ULTA,VIAB,VOD,VRSK,VRTX,WBA,WDC,WFM,XLNX,XRAY,YHOO)))
head(nasdaq100[,1:12],2)

# set outcome variable
outcomeSymbol <- 'FISV.Close'

# shift outcome value to be on same line as predictors
library(xts)
nasdaq100 <- xts(nasdaq100,order.by=as.Date(rownames(nasdaq100)))
nasdaq100 <- as.data.frame(merge(nasdaq100, lm1=lag(nasdaq100[,outcomeSymbol],-1)))
nasdaq100$outcome <- ifelse(nasdaq100[,paste0(outcomeSymbol,'.1')] > nasdaq100[,outcomeSymbol], 1, 0)

# remove shifted down volume field as we don't care by the value
nasdaq100 <- nasdaq100[,!names(nasdaq100) %in% c(paste0(outcomeSymbol,'.1'))]

# cast date to true date and order in decreasing order
nasdaq100$date <- as.Date(row.names(nasdaq100))
nasdaq100 <- nasdaq100[order(as.Date(nasdaq100$date, "%m/%d/%Y"), decreasing = TRUE),]

# calculate all day differences and populate them on same row
GetDiffDays <- function(objDF,days=c(10), offLimitsSymbols=c('outcome'), roundByScaler=3) {
  # needs to be sorted by date in decreasing order
  ind <- sapply(objDF, is.numeric)
  for (sym in names(objDF)[ind]) {
    if (!sym %in% offLimitsSymbols) {
      print(paste('*********', sym))
      objDF[,sym] <- round(scale(objDF[,sym]),roundByScaler)
      
      print(paste('theColName', sym))
      for (day in days) {
        objDF[paste0(sym,'_',day)] <- c(diff(objDF[,sym],lag = day),rep(x=0,day)) * -1
      }
    }
  }
  return (objDF)
}

# call the function with the following differences
nasdaq100 <- GetDiffDays(nasdaq100, days=c(1,2,3,4,5,10,20), offLimitsSymbols=c('outcome'), roundByScaler=2)

# drop most recent entry as we don't have an outcome
nasdaq100 <- nasdaq100[2:nrow(nasdaq100),]

# take a peek at YHOO features:
dput(names(nasdaq100)[grepl('YHOO.',names(nasdaq100))])

# well use POSIXlt to add day of the week, day of the month, day of the year
nasdaq100$wday <- as.POSIXlt(nasdaq100$date)$wday
nasdaq100$yday <- as.POSIXlt(nasdaq100$date)$mday
nasdaq100$mon<- as.POSIXlt(nasdaq100$date)$mon

# remove date field and shuffle data frame
nasdaq100 <- subset(nasdaq100, select=-c(date))
nasdaq100 <- nasdaq100[sample(nrow(nasdaq100)),]

# let's model
library(xgboost)
predictorNames <- names(nasdaq100)[names(nasdaq100) != 'outcome']

set.seed(1234)
nasdaq100 <- nasdaq100[!is.na(nasdaq100$outcome), ]
split <- sample(nrow(nasdaq100), floor(0.7*nrow(nasdaq100)))
train <-nasdaq100[split,]
test <- nasdaq100[-split,]

dtrain <- xgb.DMatrix(as.matrix(train[, predictorNames]), label = train$outcome, missing = NaN)
dtest <- xgb.DMatrix(as.matrix(test[, predictorNames]), label = test$outcome, missing = NaN)

watchlist <- list(eval = dtest, train = dtrain)

param <- list(
  max.depth = 16, 
  eta = 0.01,
  min_child_weight = 10,
  subsample = 0.5,
  # gamma = 50,
  colsample_bytree = 0.5,
  booster = "gbtree",
  objective="binary:logistic",
  eval_metric="auc")

bst <- xgb.train(param, dtrain, nrounds = 1000, verbose = 1, watchlist, 
                 print.every.n = 5,
                 early.stop.round = 50)

xgb.importance(names(train), model = bst)
# 0.750421
pred = predict(bst, dtest)
predictions <- ifelse(pred >= 0.5, 1, 0)
mean(predictions == test$outcome)

library(pROC)
auc <- roc(test$outcome, predictions)
print(paste('AUC score:', auc$auc))
