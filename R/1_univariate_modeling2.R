rm(list = ls()); gc()
library(data.table)
library(TTR)
library(xgboost)
library(quantmod)
library(caret)
source("R/utilities.R")


# 1. Load the data --------------------------------------------------------
data <- fread("data/AUD DATA2.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
names(data) <- gsub(" ", "_", names(data))
# AUDPrices <- data[, .(Date, AUD_1_Day_Lag,Australian_Dollar_Spot)]
AUDPrices <- data[, -2, with = F]

rmcols <- c("Date", "AUD_1_Day_Lag", "Australian_Dollar_Spot", "Returns1", "Direction", "Year")
coln <- setdiff(names(AUDPrices), rmcols)
AUDPrices[, coln := lapply(.SD, function(x){
  diff(as.numeric(x))
}), .SDcols = coln, with = F]


# 2. Generate the target variable -----------------------------------------
AUDPrices[, Returns1 := log(AUD_1_Day_Lag) - log(Australian_Dollar_Spot)]
summary(AUDPrices$Returns, na.rm = T)
AUDPrices[, Direction := ifelse(Returns1 > 0, 1, 0)]
table(AUDPrices$Direction)


# 3. Feature engineering --------------------------------------------------
AUDPrices[, attr_cmo3 := CMO(Australian_Dollar_Spot, 3)]
AUDPrices[, attr_volatility := volatility(Australian_Dollar_Spot, n=10, calc="close")]
AUDPrices[, attr_cci5 := CCI(Australian_Dollar_Spot, n = 5, maType = SMA)]
AUDPrices[, attr_cci15 := CCI(Australian_Dollar_Spot, n = 15, maType = SMA)]
AUDPrices[, attr_cci30 := CCI(Australian_Dollar_Spot, n = 30, maType = SMA)]
AUDPrices[, Date := as.Date(Date, "%m/%d/%Y")]
AUDPrices[, Year := year(Date)]
all <- AUDPrices[!is.na(attr_cci30) & !is.na(Returns1)]


# 4. Feature cleaning -----------------------------------------------------
rmcols <- c("Date", "AUD_1_Day_Lag", "Australian_Dollar_Spot", "Returns1", "Direction", "Year")
coln <- setdiff(names(all), rmcols)
all[, coln := lapply(.SD, scale), .SDcols = coln, with = F]


# 5. Data split -----------------------------------------------------------
rmcols <- c("Date", "AUD_1_Day_Lag", "Australian_Dollar_Spot", "Returns1", "Year")
coln <- setdiff(names(all), rmcols)
train <- all[Year %in% c(2009:2014), coln, with = F]
valid <- all[Year %in% c(2014), coln, with = F]
test <- all[Year %in% c(2015:2016), ]


# 6. Modeling -------------------------------------------------------------
# dtrain <- xgb.DMatrix(data.matrix(train[, -"Direction", with = F]), label = train$Direction, missing = NaN)
# dval <- xgb.DMatrix(data.matrix(valid[, -"Direction", with = F]), label = valid$Direction, missing = NaN)
# dtest <- xgb.DMatrix(data.matrix(test[, -"Direction", with = F]), label = test$Direction, missing = NaN)
# 
# watchlist <- list(eval = dval, train = dtrain)
# 
# param <- list(
#   max.depth = 6, 
#   eta = 0.001,
#   min_child_weight = 10,
#   subsample = 0.6,
#   colsample_bytree = 0.6,
#   booster = "gbtree",
#   objective="binary:logistic",
#   eval_metric="rmse")
# 
# bst <- xgb.train(param, dtrain, nrounds = 1000, verbose = 1, watchlist, 
#                  print.every.n = 10,
#                  early.stop.round = 5)
# 
# pred <- predict(bst, dtest)
# predictions <- ifelse(pred > 0.5, 1, 0)

# GLM
fit <- glm(Direction~., data = train, family = "binomial")
pred <- predict(fit, test, type = "response")

# 7. Validation -----------------------------------------------------------
library(pROC)
r <- roc(test$Direction, pred)
plot(r)
threshold_long <- 0.5
threshold_short <- 0.5
trades <- ifelse(pred > threshold_long, 1, ifelse(pred < threshold_short, -1, 0))
trades <- ifelse(pred > threshold_long, 1, ifelse(pred < threshold_short, -1, 0))
test$trades <- trades
confusionMatrix <- table(test$Direction, test$trades)
confusionMatrix
# Evaluation
test$Decision <- ifelse(test$Direction == 1 & test$trades == 1, "Right buy",
                        ifelse(test$Direction == 0 & test$trades == -1, "Right sell", 
                               ifelse(test$trades == 0, "No action","Wrong decision")))

# 8. Sharpe ratio ---------------------------------------------------------
test$profit <- (test$AUD_1_Day_Lag - test$Australian_Dollar_Spot) * test$trades
# Sharpe ratio = (Mean portfolio return − Risk-free rate)/Standard deviation of portfolio return
accuracy <- (confusionMatrix[1,1] + confusionMatrix[2,2]) / sum(confusionMatrix)
cat(paste0("Trading accuracy: ", accuracy))
# cat(paste0("Sharpe ratio: ", sharpe.ratio))


# 9. Visualisation --------------------------------------------------------
# Buy & Sell 
ggplot(test, aes(Date, Australian_Dollar_Spot)) + geom_line() +
  geom_point(size=2, aes(color = as.character(trades)))
# Decision evaluation
ggplot(test, aes(profit, fill = Decision, colour = Decision)) + 
  geom_density(alpha = 0.1)

# xgb.importance(names(train[,-1, with = F]), model = bst)
imp <- data.frame(varImp(fit, scale = TRUE))
rownames(imp)[rev(order(imp$Overall))]
plotCorr(all, "Returns1", "Bloomberg_JPMorgan_Asia_Dollar", "Year")
plotCorr(all, "Returns1", "DOLLAR_INDEX_SPOT", "Year")
plotCorr(all, "Returns1", "S&P_500_INDEX", "Year")
plotCorr(all, "Returns1", "Australia_Sell_5Y_&_Buy_10Y_Bo", "Year")
plotCorr(all, "Returns1", "STXE_600_Û_Pr", "Year")






### 1. Univariate indicators define
### 1.2 Univariate indicators correlations

### 2. Bivariate financial series searching
### 2.2 Divergence & Convergence feature generations

### 3. 

### 4. Macroeconomics factors

### 5. Social media sentiment analysis

### 5. Other feature creations (season, daytime, hours etc.)
