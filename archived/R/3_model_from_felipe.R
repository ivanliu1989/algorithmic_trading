set.seed(1234)

data <- read.csv("data/AUD DATA.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

idx <- sample(1:nrow(data), 0.75*nrow(data))
train <- data[idx, ]
validation <- data[-idx, ]
idx2 <- sample(1:nrow(validation), 0.5*nrow(validation))
test <- validation[idx2, ]
validation <- validation[-idx2, ]

dim(train); dim(test); dim(validation)


input <- c("Australian.Dollar.Spot", "Australia.Sell.5Y...Buy.10Y.Bo", "Australia.Sell.2Y...Buy.5Y.Bon", "AUSUS.10YR",
           "Australia.U.S..2y.Yield.Spread", "STXE.600", "MSCI.EM", "MARKIT.ITRX.AUSTRALIA.06.21",
           "S.P.500.INDEX", "HANG.SENG.CHINA.ENT.INDX", "S.P.ASX.200.INDEX", "Generic.1st..HG..Future",
           "Generic.1st..GC..Future", "China.Domestic.Steel.Rebar.25", "Iron.Ore.delivered.to.Qingdao", "Generic.1st..XW..Future",
           "DOLLAR.INDEX.SPOT", "Citi.Terms.of.Trade.Index...Au", "Bloomberg.JPMorgan.Asia.Dollar", "AUD.USD.RR.25D.3M",
           "CBOE.SPX.VOLATILITY.INDX")

numeric.col <- c("Australian.Dollar.Spot", "Australia.Sell.5Y...Buy.10Y.Bo", "Australia.Sell.2Y...Buy.5Y.Bon", "AUSUS.10YR",
             "Australia.U.S..2y.Yield.Spread", "STXE.600", "MSCI.EM", "MARKIT.ITRX.AUSTRALIA.06.21",
             "S.P.500.INDEX", "HANG.SENG.CHINA.ENT.INDX", "S.P.ASX.200.INDEX", "Generic.1st..HG..Future",
             "Generic.1st..GC..Future", "China.Domestic.Steel.Rebar.25", "Iron.Ore.delivered.to.Qingdao", "Generic.1st..XW..Future",
             "DOLLAR.INDEX.SPOT", "Citi.Terms.of.Trade.Index...Au", "Bloomberg.JPMorgan.Asia.Dollar", "AUD.USD.RR.25D.3M",
             "CBOE.SPX.VOLATILITY.INDX")

target  <- "Australian.Dollar.Spot"



# Build the Decision Tree model.
library(rpart, quietly=TRUE)
fit <- rpart(Australian.Dollar.Spot ~ .,
               data=train[, c(input)],
               method="anova",
               parms=list(split="information"),
               control=rpart.control(usesurrogate=0, 
                                     maxsurrogate=0))
summary(fit)
print(fit)
printcp(fit)


# risk chart
library(ggplot2)
pr <- predict(fit, newdata=validation)
eval <- evaluateRisk(pr,validation$Australian.Dollar.Spot)
print(riskchart(pr, validation$Australian.Dollar.Spot,
                title="Performance Chart Decision Tree AUD DATA.csv [validate] ", show.lift=FALSE, show.precision=FALSE, legend.horiz=FALSE))


# nnet
library(nnet)
fit <- nnet(Australian.Dollar.Spot ~ .,
            data=train[, c(input)],
            size=10, linout=TRUE, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

cat(sprintf("A %s network with %d weights.\n",
            paste(fit$n, collapse="-"),
            length(fit$wts)))
cat(sprintf("Inputs: %s.\n",
            paste(fit$coefnames, collapse=", ")))
cat(sprintf("Output: %s.\n",
            names(attr(fit$terms, "dataClasses"))[1]))
cat(sprintf("Sum of Squares Residuals: %.4f.\n",
            sum(residuals(fit) ^ 2)))
cat("\n")
print(summary(fit))
cat('\n')


pr <- predict(fit, newdata=validation)
eval <- evaluateRisk(pr, validation$Australian.Dollar.Spot)
print(riskchart(pr, validation$Australian.Dollar.Spot,
                title="Performance Chart Decision Tree AUD DATA.csv [validate] ", show.lift=FALSE, show.precision=FALSE, legend.horiz=FALSE))
