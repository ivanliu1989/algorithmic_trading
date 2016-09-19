library(tseries)
library(quantmod)
library(data.table)
library(vrtest)

# 1. Load the data --------------------------------------------------------
data <- fread("data/AUD DATA2.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
names(data) <- gsub(" ", "_", names(data))
AUD <- data$Australian_Dollar_Spot

# ADF Test
adf.test(AUD, alternative = "stationary", k = 1)

# Variance Ratio Test
Lo.Mac(AUD, c(2, 5, 10))
