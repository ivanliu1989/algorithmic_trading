
# Plotting correlationships between two timeseries ------------------------
plotCorr <- function(df, xvar, yvar, col){
  library(ggplot2)
  
  df <- as.data.frame(df)
  t1<-theme(                              
    axis.title.x = element_text(face="bold", color="black", size=10),
    axis.title.y = element_text(face="bold", color="black", size=10),
    plot.title = element_text(face="bold", color = "black", size=12)
  )
  df[,xvar] <- as.numeric(df[,xvar])
  df[,yvar] <- as.numeric(df[,yvar])
  df[,col] <- as.numeric(df[,col])
  
  cor_p <- ggplot(df, aes(x = df[,xvar], y = df[,yvar])) +
    geom_point(size=2, aes(color = abs(df[,col]))) +
    geom_hline(yintercept = 0, color="black") +
    scale_color_continuous(name=col,   
                           breaks = with(df, c(min(abs(df[,col])), median(abs(df[,col])), max(abs(df[,col])))),
                           labels = c("min", "median", "max"),
                           low = "pink",                           
                           high = "red") +
    geom_smooth() + 
    theme_bw() + t1 + labs(x=xvar, y = yvar, title= paste0(xvar, ' vs ', yvar))
  
  return(cor_p)
}




# Function for feature engineering of single prices -----------------------
ttrFeature <- function(prices){
  # ADX
  
  # ATR
  
  # BBands
  
  # CCI
  attr_cci5 <- CCI(prices, n = 5, maType = SMA)
  attr_cci10 <- CCI(prices, n = 10, maType = SMA)
  attr_cci20 <- CCI(prices, n = 20, maType = SMA)
  attr_cci50 <- CCI(prices, n = 50, maType = SMA)
  
  # Chaikin Volatility
  # Close Location Value
  
  # Chande Momentum Oscillator
  attr_cmo10 <- CMO(prices, 10)
  attr_cmo20 <- CMO(prices, 20)
  attr_cmo50 <- CMO(prices, 50)
  
  # De-Trended Price Oscillator
  attr_dpo <- DPO(prices, n = 10, shift = 10/2 + 1, percent = TRUE)
  
  # DV Intermediate Oscillator
  attr_dvi <- DVI(prices, n = 252, wts = c(0.8, 0.2), smooth = 3, magnitude = c(5, 100, 5), 
                  stretch = c(10, 100, 2), exact.multiplier = 1)
  
  # Guppy Multiple Moving Averages
  attr_gmma <- GMMA(prices, short = c(3, 5, 8, 10, 12, 15), long = c(30, 35, 40, 45, 50, 60))
  
  # Know Sure Thing
  attr_kst <- KST(prices)
  attr_kst4ma <- KST(prices, maType=list(list(SMA),list(EMA),list(DEMA),list(WMA)))
  
  # MACD
  attr_MACD <- MACD(prices, nFast = 12, nSlow = 26, nSig = 9, 
                    maType = list(list(SMA),list(EMA),list(DEMA),list(WMA)), percent = TRUE)
  
  # PBands
  attr_pband <- PBands(prices, n = 20, maType = "SMA", sd = 2, fastn = 2,
                       centered = FALSE, lavg = FALSE)
  
  # Rate of Change / Momentum
  attr_ROC <- ROC(prices, n = 1, type = c("continuous", "discrete"), na.pad = TRUE)
  
  # Relative Strength Index
  attr_RSI <- RSI(prices, n = 14, maType = list(list(SMA),list(EMA),list(DEMA),list(WMA)))
  
  # Percent Rank over a Moving Window
  attr_prma <- runPercentRank(prices, n = 260, cumulative = FALSE, exact.multiplier = 0.5)
  
  # Stochastic Oscillator / Stochastic Momentum Index
  attr_stoch <- stoch(prices, nFastK = 14, nFastD = 3, nSlowD = 3, 
                      maType = list(list(SMA),list(EMA),list(DEMA),list(WMA)), 
                      bounded = TRUE, smooth = 1)
  attr_smi <- SMI(prices, n = 13, nFast = 2, nSlow = 25, nSig = 9, 
                  maType = list(list(SMA),list(EMA),list(DEMA),list(WMA)), 
                  bounded = TRUE)
  
  # Trend Detection Index
  attr_tdi <- TDI(prices, n = 20, multiple = 2)
  
  # Triple Smoothed Exponential Oscillator
  attr_trix <- TRIX(prices, n = 20, nSig = 9, maType = list(list(SMA),list(EMA),list(DEMA),list(WMA)), 
                    percent = TRUE)
  
  # Vertical Horizontal Filter
  attr_vhf <- VHF(prices, n = 28)
  
  # volatility
  attr_volatility <- volatility(prices, calc="close")
  
  
  
  
  # rolling
  # runSum
  # runMedian
  # runVar
  # runSD
  # runMAD
  # SMA
  # EMA
  # HMA
  # ...
  
  # lags, growth
  # growth
  # lags
  
  featSets <- data.table(prices = prices,
                         attr_cci5 = attr_cci5,
                         attr_cci10 = attr_cci10,
                         attr_cci20 = attr_cci20,
                         attr_cci50 = attr_cci50)
}
