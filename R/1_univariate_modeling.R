rm(list = ls()); gc()
library(data.table)
library(TTR)
library(quantmod)
source("R/utilities.R")


# 1. Load the data --------------------------------------------------------
data <- fread("data/AUD DATA.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
names(data) <- gsub(" ", "_", names(data))
AUDPrices <- data[, .(Date, AUD_1_Day_Lag,Australian_Dollar_Spot)]


# 2. Generate the target variable -----------------------------------------
AUDPrices[, Growth := log(AUD_1_Day_Lag) - log(Australian_Dollar_Spot)]
summary(AUDPrices$Growth, na.rm = T)
AUDPrices[, target := ifelse(Growth > 0, 1, 0)]
table(AUDPrices$target)


# 3. Generate Features ----------------------------------------------------


