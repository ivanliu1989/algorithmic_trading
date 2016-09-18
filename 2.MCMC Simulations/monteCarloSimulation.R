rm(list=ls());gc()
library(quantmod)
library(TTR)
library(data.table)
library(PearsonDS)
library(PerformanceAnalytics)
library(fUnitRoots)
library(FGN)
library(urca)
source("1.Kalman Filter/kalmanFilter.R")

# 1. Load data and calculate correlations ---------------------------------
getFX("AUD/USD")
getFX("CAD/USD")

# 2. Moments --------------------------------------------------------------


