rm(list = ls()); gc()
library(data.table)
library(TTR)
# library(xgboost)
library(quantmod)
library(caret)
library(pROC)
source("R/utilities.R")


# 1. Load the data --------------------------------------------------------
data <- fread("data/AUD DATA2.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
names(data) <- gsub(" ", "_", names(data))
# AUDPrices <- data[, .(Date, AUD_1_Day_Lag,Australian_Dollar_Spot)]
AUDPrices <- data[, -2, with = F]

rmcols <- c("Date", "AUD_1_Day_Lag", "Australian_Dollar_Spot", "Returns1", "Direction", "Year")
coln <- setdiff(names(AUDPrices), rmcols)
AUDPrices[, paste0(coln,1) := lapply(.SD, function(x){
  (as.numeric(x) - shift(as.numeric(x), n = 1, type = "lag")) / (as.numeric(x) + shift(as.numeric(x), n = 1, type = "lag"))
}), .SDcols = coln, with = F]

AUDPrices[, paste0(coln,3) := lapply(.SD, function(x){
  (as.numeric(x) - shift(as.numeric(x), n = 3, type = "lag")) / (as.numeric(x) + shift(as.numeric(x), n = 3, type = "lag"))
}), .SDcols = coln, with = F]

AUDPrices[, paste0(coln,10) := lapply(.SD, function(x){
  (as.numeric(x) - shift(as.numeric(x), n = 10, type = "lag")) / (as.numeric(x) + shift(as.numeric(x), n = 10, type = "lag"))
}), .SDcols = coln, with = F]

AUDPrices[, paste0(coln,"3_10") := lapply(.SD, function(x){
  ((as.numeric(x) - shift(as.numeric(x), n = 3, type = "lag")) / (as.numeric(x) + shift(as.numeric(x), n = 3, type = "lag")))
  -((as.numeric(x) - shift(as.numeric(x), n = 10, type = "lag")) / (as.numeric(x) + shift(as.numeric(x), n = 10, type = "lag")))
}), .SDcols = coln, with = F]


AUDPrices[, coln := NULL, with = F]

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
all[, coln := lapply(.SD, function(x) scale(x)), .SDcols = coln, with = F]


# 5. Data split -----------------------------------------------------------
rmcols <- c("Date", "AUD_1_Day_Lag", "Australian_Dollar_Spot", "Returns1", "Year")
coln <- setdiff(names(all), rmcols)
train <- all[Year %in% c(2013:2015), coln, with = F]
# valid <- all[Year %in% c(2013), coln, with = F]
test <- all[Year %in% c(2016), ]


# 6. Modeling -------------------------------------------------------------
# dtrain <- xgb.DMatrix(data.matrix(train[, -"Direction", with = F]), label = train$Direction, missing = NaN)
# dval <- xgb.DMatrix(data.matrix(valid[, -"Direction", with = F]), label = valid$Direction, missing = NaN)
# dtest <- xgb.DMatrix(data.matrix(test[, -"Direction", with = F]), label = test$Direction, missing = NaN)
# 
# watchlist <- list(eval = dval, train = dtrain)
# 
# param <- list(
#   max.depth = 12,
#   eta = 0.03,
#   min_child_weight = 10,
#   subsample = 0.5,
#   colsample_bytree = 0.5,
#   booster = "gbtree", # gblinear  gbtree
#   objective="binary:logistic",
#   lambda = 4,
#   lambda_bias = 8,
#   alpha = 4,
#   eval_metric="auc")
# 
# bst <- xgb.train(param, dtrain, nrounds = 1000, verbose = 1, watchlist,
#                  print.every.n = 10,
#                  early.stop.round = 500)
# 
# pred <- predict(bst, dtest) # AUC 0.5456 || ACC 0.542
# predictions <- ifelse(pred > 0.5, 1, 0)

# GLM
fit1 <- glm(Direction~., data = train, family = "binomial") # AUC 0.6205 || ACC 0.71
pred1 <- predict(fit1, test, type = "response")

# knn
gbmGrid <-  expand.grid(k = 12)
fitControl <- trainControl(
  method = "none",
  number = 5)
fit2 <-  train(as.factor(Direction) ~ ., data = train, 
              method = "knn", 
              trControl = fitControl,
              # verbose = TRUE,
              tuneGrid = gbmGrid)
pred2 <- predict(fit2, test, type = "prob")[,2]

# rf
gbmGrid <-  expand.grid(mtry = 6)
fitControl <- trainControl(
  method = "none",
  number = 5)
fit3 <-  train(as.factor(Direction) ~ ., data = train, 
               method = "rf", 
               trControl = fitControl,
               # verbose = TRUE,
               tuneGrid = gbmGrid)
pred3 <- predict(fit3, test, type = "prob")[,2]

pred <- (0.5*pred1 + 0.3*pred2 + 0.2*pred3) # AUC 0.6409 || ACC 0.68

# 7. Validation -----------------------------------------------------------
r <- roc(test$Direction, pred)
plot(r)
threshold_long <- 0.58
threshold_short <- 0.45
trades <- ifelse(pred > threshold_long, 1, ifelse(pred < threshold_short, -1, 0))
test$trades <- trades
confusionMatrix <- table(test$Direction, test$trades)
colnames(confusionMatrix) <- c("pred.short", "pred.flat", "pred.long")
row.names(confusionMatrix) <- c("act.short", "act.long")
confusionMatrix

# Evaluation
test$Decision <- ifelse(test$Direction == 1 & test$trades == 1, "Right buy",
                        ifelse(test$Direction == 0 & test$trades == -1, "Right sell", 
                               ifelse(test$trades == 0, "No action","Wrong decision")))

# 8. Sharpe ratio ---------------------------------------------------------
test$profit <- (test$AUD_1_Day_Lag - test$Australian_Dollar_Spot) * test$trades
# Sharpe ratio = (Mean portfolio return − Risk-free rate)/Standard deviation of portfolio return
accuracy <- (confusionMatrix[1,1] + confusionMatrix[2,3]) / (sum(confusionMatrix) - sum(confusionMatrix[,2]))
cat(paste0("Trading accuracy: ", accuracy))
# cat(paste0("Sharpe ratio: ", sharpe.ratio))


# 9. Visualisation --------------------------------------------------------
# Buy & Sell
test$tradesInd <- ifelse(trades == 1, "Long", ifelse(trades == -1, "Short", "Flat"))
ggplot(test, aes(Date, Australian_Dollar_Spot)) + geom_line() +
  geom_point(size=2, aes(color = tradesInd))
# Decision evaluation
ggplot(test, aes(profit, fill = Decision, colour = Decision)) + 
  geom_density(alpha = 0.1)

# xgb.importance(names(train[,-1, with = F]), model = bst)
imp <- data.frame(varImp(fit1, scale = TRUE))
rownames(imp)[rev(order(imp$Overall))]
plotCorr(all, "Returns1", "Australia_Sell_5Y_&_Buy_10Y_Bo10", "Year")
plotCorr(all, "Returns1", "STXE_600_Û_Pr10", "Year")
plotCorr(all, "Returns1", "attr_volatility", "Year")
plotCorr(all, "Returns1", "Australia_Sell_2Y_&_Buy_5Y_Bon10", "Year")
plotCorr(all, "Returns1", "CBOE_SPX_VOLATILITY_INDX10", "Year")
plotCorr(all, "Returns1", "Australia_Sell_2Y_&_Buy_5Y_Bon3", "Year")






### 1. Univariate indicators define
### 1.2 Univariate indicators correlations

### 2. Bivariate financial series searching
### 2.2 Divergence & Convergence feature generations

### 3. Unsupervised volatility clustering

### 4. Macroeconomics factors

