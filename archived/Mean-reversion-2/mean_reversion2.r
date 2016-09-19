library(quantmod)

AUDUSD <- read.csv("Mean-reversion-2/AUDUSD.csv")
NZDUSD <- read.csv("Mean-reversion-2/NZDUSD.csv")

AUDUSD <- xts(AUDUSD[,-1], as.Date(AUDUSD[,1], format="%d/%m/%Y"), src="csv", dateFormat = 'Date')
colnames(AUDUSD) <- paste(toupper(gsub("\\^", "", "AUDUSD"  )), 
                          c("Open", "High", "Low", "Close", "Volume", "Adjusted"), 
                          sep = ".")
NZDUSD <- xts(NZDUSD[,-1], as.Date(NZDUSD[,1], format="%d/%m/%Y"), src="csv", dateFormat = 'Date')
colnames(NZDUSD) <- paste(toupper(gsub("\\^", "", "NZDUSD"  )), 
                          c("Open", "High", "Low", "Close", "Volume", "Adjusted"), 
                          sep = ".")

closes <- merge.xts( AUDUSD[, 'AUDUSD.Close'], NZDUSD[, 'NZDUSD.Close'])
closes_df <- data.frame(closes$AUDUSD.Close, closes$NZDUSD.Close)

plot.zoo(closes, plot.type = 'single', col = c('red', 'blue'))
legend("bottomright", colnames(closes), lty = 1, col = c('red', 'blue'))

plot(closes_df$AUDUSD.Close, closes_df$NZDUSD.Close, col = 'navyblue')
lines(lowess(closes_df$AUDUSD.Close, closes_df$NZDUSD.Close), col = 'deeppink2')

## ordinary least squares regression
m <- lm(AUDUSD.Close ~ NZDUSD.Close, data = closes)
beta <- coef(m)[2]

resid <- closes$AUDUSD.Close - beta * closes$NZDUSD.Close
colnames(resid) <- "residual"
plot.zoo(resid, col = 'navyblue', xlab = 'Date', ylab = 'residual')

## test stationarity of the spread
library(urca)
summary(ur.df(resid, type = "drift", lags = 1))

## total least squares regression
r <- princomp(~ AUDUSD.Close + NZDUSD.Close, data=closes)
beta_TLS <- r$loadings[1,1] / r$loadings[2,1] # I think we also need a non-zero intercept in both cases

resid_TLS <- closes$AUDUSD.Close - beta_TLS * closes$NZDUSD.Close
colnames(resid_TLS) <- "residual"
plot.zoo(resid_TLS, col = 'blue')

summary(ur.df(resid_TLS, type = "drift", lags = 1))

## johansen test
jo_t <- ca.jo(cbind(AUDUSD$AUDUSD.Close, NZDUSD$NZDUSD.Close), type="trace", ecdet="none", K=2) #test trace statistics
jo_e <- ca.jo(cbind(AUDUSD$AUDUSD.Close, NZDUSD$NZDUSD.Close), type="eigen", ecdet="none", K=2) # eigenvalue test statistics
print(summary(jo_t))

## construct portfolio
spread <- closes$AUDUSD.Close - 3.41 * closes$NZDUSD.Close
plot.zoo(spread,  col = 'navyblue', xlab = 'Date', ylab = 'Spread from johansen eigenvector')

## calculate half life of mean reversion
y <- spread
y.lag <- lag(y, -1)
delta.y <- diff(y)

df <- cbind(y, y.lag, delta.y)
df <- df[-1 ,] #remove first row with NAs

regress.results <- lm(delta.y ~ y.lag, data = df)

lambda <- summary(regress.results)$coefficients[2]
half.life <- log(2)/lambda

## add third pair to portfolio
USDCAD <- read.csv("USDCAD.csv")
USDCAD <- xts(USDCAD[,-1], as.Date(USDCAD[,1], format="%d/%m/%Y"), src="csv", dateFormat = 'Date')
colnames(USDCAD) <- paste(toupper(gsub("\\^", "", "USDCAD"  )), 
                          c("Open", "High", "Low", "Close", "Volume", "Adjusted"), 
                          sep = ".")
closes <- merge.xts( AUDUSD[, 'AUDUSD.Close'], NZDUSD[, 'NZDUSD.Close'], 1/USDCAD[, 'USDCAD.Close'])
jo_3_t <- ca.jo(cbind(AUDUSD$AUDUSD.Close, NZDUSD$NZDUSD.Close, 1/USDCAD$USDCAD.Close), type="trace", ecdet="none", K=2)
print(summary(jo_3_t))
jo_3_e <- ca.jo(cbind(AUDUSD$AUDUSD.Close, NZDUSD$NZDUSD.Close, 1/USDCAD$USDCAD.Close), type="eigen", ecdet="none", K=2) #[, 'USDCAD.Close'])
print(summary(jo_3_e))

spread3 <- closes$AUDUSD.Close - 0.581 * closes$NZDUSD.Close - 1.124 * closes$USDCAD.Close
plot.zoo(spread3,  col = 'darkgreen', xlab = 'Date', ylab = 'Spread from johansen eigenvector')

y <- spread3
y.lag <- lag(y, -1)
delta.y <- diff(y)

df <- cbind(y, y.lag, delta.y)
df <- df[-1 ,] #remove first row with NAs

regress.results <- lm(delta.y ~ y.lag, data = df)

lambda <- summary(regress.results)$coefficients[2]
half.life <- log(2)/lambda
print(half.life)

## subset price data to reproduce Chan's results (Example 5.1 in Algorithmic Trading)

A <- AUDUSD["2009-12-18/2012-04-06"]
N <- NZDUSD["2009-12-18/2012-04-06"]
C <- USDCAD["2009-12-18/2012-04-06"]
jo_3a_t <- ca.jo(cbind(A$AUDUSD.Close, 1/C$USDCAD.Close), type="trace", ecdet="none", K=2)
print(summary(jo_3a_t))

spread3a <- A$AUDUSD.Close - 3.944 * C$USDCAD.Close
plot.zoo(spread3a,  col = 'deeppink', xlab = 'Date', ylab = 'Spread from johansen eigenvector')

y <- spread3a
y.lag <- lag(y, -1)
delta.y <- diff(y)

df <- cbind(y, y.lag, delta.y)
df <- df[-1 ,] #remove first row with NAs

regress.results <- lm(delta.y ~ y.lag, data = df)

lambda <- summary(regress.results)$coefficients[2]
half.life <- log(2)/lambda
print(half.life)


