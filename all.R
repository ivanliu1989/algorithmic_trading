rm(list=ls());gc()
library(quantmod)
library(TTR)
library(data.table)
library(PearsonDS)
library(PerformanceAnalytics)
library(fUnitRoots)
library(FGN)
library(urca)
source("functions.R")


# 1. Load data and calculate correlations ---------------------------------
getSymbols("EWA")
getSymbols("EWC")
long = EWC$EWC.Adjusted
short = EWA$EWA.Adjusted

# 2. Kalman Filter Hedging ------------------------------------------------
kalman <- kalmanFilter(long, short, sdnum = 2)
kalman$numUnits

# 3. JohansenEigenvector --------------------------------------------------
JohansenEigen <- JohansenEigenvector(long, short, 250, 20)
JohansenEigen$numUnits
JohansenEigen$numUnits <- ifelse(JohansenEigen$numUnits > 1*sd(JohansenEigen$numUnits), 1,
                                 ifelse(JohansenEigen$numUnits < -1*sd(JohansenEigen$numUnits), -1, 0))
JohansenEigen$numUnits

# 4. Momentum -------------------------------------------------------------
momentums <- tsMomentum(long,short,kalman$hedgeRatio)
momentums$positions

# 5. HurstExpSystem -------------------------------------------------------
hurstExp = hurstExpSystem(long, short, kalman$hedgeRatio)
hurstExp$numUnits
hurstExp$numUnits <- ifelse(hurstExp$numUnits > 1*sd(hurstExp$numUnits), 1,
                            ifelse(hurstExp$numUnits < -1*sd(hurstExp$numUnits), -1, 0))
hurstExp$numUnits

# 6. Rebalance ------------------------------------------------------------
positions = rebalance(long, short, kalman$numUnits, kalman$hedgeRatio)  
positions = rebalance(long, short, JohansenEigen$numUnits, kalman$hedgeRatio)  
positions = rebalance(long, short, hurstExp$numUnits, kalman$hedgeRatio)  

# 7. Performance measurements ---------------------------------------------
backTests(long, short, positions, from = Sys.Date()-365, to = Sys.Date())
backTests(long, short, positions, from = Sys.Date()-30065, to = Sys.Date())

# 8. Dashboards -----------------------------------------------------------



