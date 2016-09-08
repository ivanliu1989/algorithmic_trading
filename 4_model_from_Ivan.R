rm(list = ls()); gc()
library(data.table)
library(TTR)
library(quantmod)
set.seed(1234)

data <- fread("C:/Users/sky_x/Desktop/ANZ Option Model/Model from Felipe/AUD DATA.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
names(data) <- gsub(" ", "_", names(data))
data
