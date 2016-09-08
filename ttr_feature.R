rm(list = ls()); gc()
library(data.table)
library(TTR)
set.seed(1234)
source("0_plotCorr.R")
source("0_ttrFeatires.R")
plotCorr(iris, "Sepal.Length", "Petal.Width", "Sepal.Width")



data <- fread("data/AUD DATA.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
names(data) <- gsub(" ", "_", names(data))
data$Day <- 1:nrow(data)
data$target <- log(data$AUD_1_Day_Lag) - log(data$Australian_Dollar_Spot)
plotCorr(data, "target", "China_Domestic_Steel_Rebar_25", "Day")


prices <- data$Australian_Dollar_Spot
acf(attr_cci10[!is.na(attr_cci10)])


