rm(list = ls()); gc()
library(data.table)
library(TTR)
library(caret)
library(deepnet)
library(foreach)
library(doParallel)
library(e1071)
library(quantmod)
library(reshape2)
source("R/utilities.R")


# 1. Load the data --------------------------------------------------------
data <- fread("data/AUD DATA.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
names(data) <- gsub(" ", "_", names(data))
AUDPrices <- data[, .(Date, AUD_1_Day_Lag,Australian_Dollar_Spot)]


# 2. Generate the target variable -----------------------------------------
AUDPrices[, Returns := log(AUD_1_Day_Lag) - log(Australian_Dollar_Spot)]
summary(AUDPrices$Returns, na.rm = T)
AUDPrices[, Direction := ifelse(Returns > 0, 1, 0)]
table(AUDPrices$Direction)


# 3. Generate Features ----------------------------------------------------
# Calculate features - lagged return and volatility values
periods <- c(1:5)
Returns <- AUDPrices$Returns
Volatility <- volatility(AUDPrices$Australian_Dollar_Spot, n = 14, calc = "close")
Direction <- AUDPrices$Direction

lagReturns <- data.frame(lapply(periods, function(x) Lag(Returns, x)))
colnames(lagReturns) <- c('Ret1', 'Ret2', 'Ret3', 'Ret4', 'Ret5')
lagVolatility <- data.frame(lapply(periods, function(x) Lag(Volatility, x)))
colnames(lagVolatility) <- c('Vol1', 'Vol2', 'Vol3', 'Vol4', 'Vol5')

# create data set based on lagged returns and volatility indicators with next day return and direction as targets
dat <- data.frame(Returns, Direction, lagReturns, lagVolatility)
dat <- dat[-c(1:14), ] #remove zeros from initial volatility calcs
dat <- na.omit(dat)

# preserve out of sample data
# Train <- dat[1:(nrow(dat)-500), ]
# Test <- dat[-(1:(nrow(dat)-500)), ]
Train <- dat # for using all data in training set


# logistic regression models ----------------------------------------------

direction <- 2 # column number of direction variable
features <- c(3,4,5,8,9,10) #feature columns
returns <- 1 # returns column
windows <- c(15, 20, 25, 30, 40, 50, 75, 100, 125, 150, 200, 250, 300, 350, 400, 450, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600)
windows <- c(15, 30)

modellist.lr <- list()
PFTrain <- vector()
SharpeTrain <- vector()
j <- 1
for (i in windows) {
  
  timecontrol <- trainControl(method = 'timeslice', initialWindow = i, horizon = 1, classProbs = TRUE,
                              returnResamp = 'final', fixedWindow = TRUE, savePredictions = 'final') 
  cl <- makeCluster(8)
  registerDoParallel(cl)
  set.seed(503)
  modellist.lr[[j]] <- train(Train[, features], Train[, direction],
                             method = 'glm', family = 'binomial',
                             trControl = timecontrol)
  
  #### comment out lines 63-65 and uncomment lines 67-93 to run in-sample performance module
  j <- j+1
  print(i)
  stopCluster(cl) } 

#   ctrl <- trainControl(method='none', classProbs = TRUE)
#   tradesCP <- list()
#   pfCP <- vector()
#   srCP <- vector()
#   indexes <- modellist.lr[[j]]$control$index
#   for(k in c(1:length(modellist.lr[[j]]$control$index)) )
#       {
#         model <-  train(Train[indexes[[k]], features], Train[indexes[[k]], direction], 
#                        method = 'glm', family = 'binomial',
#                        trControl = ctrl)
#         
#         predsCP <- predict(model, Train[indexes[[k]], features], type = 'prob')
#         th <- 0.5
#         
#         tradesCP[[k]] <- ifelse(predsCP$up > th, Train[indexes[[k]], returns], ifelse(predsCP$down > th,  Train[indexes[[k]], returns], 0))
#         pfCP[k] <- sum(tradesCP[[k]][tradesCP[[k]] > 0])/abs(sum(tradesCP[[k]][tradesCP[[k]] < 0]))
#         srCP[k] <- sqrt(252)*mean(tradesCP[[k]])/sd(tradesCP[[k]])
#         
#         cat("\niteration:",k, "PF:",round(pfCP[k], digits = 2), "SR:",round(srCP[k], digits = 2))
#       
#       }
#   PFTrain[j] <- mean(pfCP)
#   SharpeTrain[j] <- mean(srCP)
#   stopCluster(cl)
#   j <- j+1
#   cat("\n",i)
# }

# resampled performance - use class probabilities as a trade threshold

thresholds <- seq(0.50, 0.65, 0.005)
windowIndex <- c(1:length(windows))
pf.cv.th <- matrix(nrow = length(windowIndex), ncol = length(thresholds), dimnames = list(c(as.character(windows)), c(as.character(thresholds)))) # adjust matrix dimensions based on loop size
sr.cv.th <- matrix(nrow = length(windowIndex), ncol = length(thresholds), dimnames = list(c(as.character(windows)), c(as.character(thresholds))))
Trades.DF <- matrix(nrow = length((max(windows)+1):nrow(Train)), ncol = length(windowIndex) , dimnames = list(c(), c(as.character(windows))))

for(i in windowIndex)
{
  commonData <- c((max(windows)+1):nrow(Train))
  j <- 1
  for(th in thresholds)
  {
    trades <- ifelse(modellist.lr[[i]]$pred$up[(length(modellist.lr[[i]]$pred$up)-length(commonData)+1):length(modellist.lr[[i]]$pred$up)] > th, Train$Returns[commonData], ifelse(modellist.lr[[i]]$pred$down[(length(modellist.lr[[i]]$pred$down)-length(commonData)+1):length(modellist.lr[[i]]$pred$down)] > th, -Train$Returns[commonData], 0))
    plot(cumsum(trades), type = 'l', col = 'blue', xlab = 'day', ylab = 'cumP', main = paste0('Window: ',windows[i], ' Cl.Prob Thresh: ',th), ylim = c(-0.5, 1.0))
    lines(cumsum(Train$Returns[commonData]), col = 'red')
    pf.cv.th[i, j] <- sum(trades[trades>0])/abs(sum(trades[trades<0]))
    sr.cv.th[i, j] <- sqrt(252)*mean(trades)/sd(trades)
    if(th == 0.525) Trades.DF[, i] <- trades
    j <- j+1
  }
}


#### plot training vs cv performance - class probs
th <- 11 # threshold index 
plot(PFTrain, pf.cv.th[, th])
PF.Compare <- data.frame(windows, PFTrain, pf.cv.th[, th])
colnames(PF.Compare) <- c('window', 'IS', 'CV')
SR.Compare <- data.frame(windows, SharpeTrain, sr.cv.th[, th])
colnames(SR.Compare) <- c('window', 'IS', 'CV')

# base R plots
plot(windows, SharpeTrain, type = 'l', ylim = c(-0.9, 1.5), col = 'blue')
lines(windows, sr.cv.th[, th], col = 'red')
plot(windows, PFTrain, type = 'l', ylim = c(0.8, 1.5), col = 'blue')
lines(windows, pf.cv.th[, th], col = 'red')

# nice ggplots
pfMolten <- melt(PF.Compare, id = c('window'))
srMolten <- melt(SR.Compare, id = c('window'))

pfPlot <- ggplot(data=pfMolten,
                 aes(x=window, y=value, colour=variable)) +
  geom_line(size = 0.75) +
  scale_colour_manual(values = c("steelblue3","tomato3")) +
  theme(legend.title=element_blank()) +
  ylab('Profit Factor') +
  ggtitle("In-Sample and Cross-Validated Performance")

sharpePlot <- ggplot(data=srMolten,
                     aes(x=window, y=value, colour=variable)) +
  geom_line(size = 1.25) +
  scale_colour_manual(values = c("steelblue3","tomato3")) +
  theme(legend.title=element_blank()) +
  ylab('Sharpe Ratio') +
  ggtitle("In-Sample and Cross-Validated Performance")

# heatmap plots of performance and thresholds
srHeat <- melt(sr.cv.th)
colnames(srHeat) <- c('window', 'threshold', 'sharpe')
srHeat$window <- factor(srHeat$window, levels = c(windows))

pfHeat <- melt(pf.cv.th[-c(15:22), ])
colnames(pfHeat) <- c('window', 'threshold', 'pf')
pfHeat$window <- factor(pfHeat$window, levels = c(windows))

sharpeHeatMap <- ggplot(data=srHeat, aes(x = window, y = threshold)) + 
  geom_tile(aes(fill = sharpe), colour = "white") +
  scale_fill_gradient(low = "#fff7bc", high = "#e6550d", name = 'Sharpe Ratio') +
  scale_x_discrete(breaks = windows[seq(2,28,2)]) +
  xlab('Window Length (not to scale)') +
  ylab('Class Probability Threshold') +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

pfHeatMap <- ggplot(data=pfHeat, aes(x = window, y = threshold)) + 
  geom_tile(aes(fill = pf), colour = "white") +
  scale_fill_gradient(low = "#e5f5f9", high = "#2ca25f", name = 'Profit Factor') +
  xlab('Window Length (not to scale)') +
  ylab('Class Probability Threshold') +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())


# Equity curves
#colnames(Trades.DF) <- as.character(windows)
Trades.DF <- as.data.frame(Trades.DF)
Equity <- cumsum((Trades.DF[, as.character(c(15, 30, 500, 1000, 1500))]))
Equity$Underlying <- cumsum(Train$Returns[commonData])
Equity$Index <- c(1:nrow(Equity))
EquityMolten <- melt(Equity, id = 'Index')

equityPlot <- ggplot(data=EquityMolten, aes(x=Index, y=value, colour=variable)) +
  geom_line(size = 0.6) +
  scale_color_manual(name = c('Window\nLength'), values = c('darkorchid3', 'dodgerblue3', 'darkolivegreen4', 'darkred', 'goldenrod', 'grey35')) +
  ylab('Return') +
  xlab('Day') +
  ggtitle("Returns")

## Equity curves for a single window length
thresholds <- seq(0.50, 0.95, 0.01)
Index <- which(windows == 30)
Trades.win <- matrix(nrow = length((max(windows)+1):nrow(Train)), ncol = length(thresholds) , dimnames = list(c(), c(as.character(thresholds))))

commonData <- c((max(windows)+1):nrow(Train))
j <- 1
for(th in thresholds)
{
  trades <- ifelse(modellist.lr[[Index]]$pred$up[(length(modellist.lr[[Index]]$pred$up)-length(commonData)+1):length(modellist.lr[[Index]]$pred$up)] > th, Train$Returns[commonData], ifelse(modellist.lr[[Index]]$pred$down[(length(modellist.lr[[Index]]$pred$down)-length(commonData)+1):length(modellist.lr[[Index]]$pred$down)] > th, -Train$Returns[commonData], 0))
  Trades.win[, j] <- trades
  j <- j+1
}

Trades.win.DF <- as.data.frame(Trades.win)
Equity.win <- cumsum(Trades.win.DF[, seq(1, 31, 5)])
Equity.win$Underlying <- cumsum(Train$Returns[commonData])
Equity.win$Index <- c(1:nrow(Equity))
Equity.winMolten <- melt(Equity.win, id = 'Index')

singleWinPlot <- ggplot(data=Equity.winMolten,
                        aes(x=Index, y=value, colour=variable)) +
  geom_line(size = 0.6) +
  scale_colour_brewer(palette = 'Dark2', name = c('Class\nProbability\nThreshold')) +
  ylab('Return') +
  xlab('Day') +
  ggtitle("Returns")

