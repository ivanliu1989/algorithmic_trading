#now let's take it out of sample to see how it works
getSymbols("^N225",from="1980-01-01",to=format(Sys.Date(),"%Y-%m-%d"))
N225 <- to.monthly(N225)[,4]
index(N225) <- as.Date(index(N225))
retN225<-ROC(N225,n=1,type="discrete")
index(retN225) <- as.Date(index(retN225))
hurstKmonthly <- apply.rolling(retN225, FUN="HurstK", width = 12)
colnames(hurstKmonthly) <- "HurstK.monthly"
index(hurstKmonthly) <- as.Date(index(hurstKmonthly))
serialcorr <- runCor(cbind(coredata(retN225)),cbind(index(retN225)),n=12)
serialcorr <- as.xts(serialcorr,order.by=index(retN225))
autoreg <- runCor(retN225,lag(retN225,k=1),n=12)
colnames(serialcorr) <- "SerialCorrelation.monthly"
colnames(autoreg) <- "AutoRegression.monthly"
signalUpTrend <- runMean(hurstKmonthly+serialcorr+autoreg,n=6) + (N225/runMean(N225,n=12)-1)*10
chart.TimeSeries(signalUpTrend)
signalUpTrend <- lag(signalUpTrend,k=1)
retSys <- merge(ifelse(signalUpTrend > 1, 1, 0) * retN225,retN225)
colnames(retSys) <- c("Nikkei 225 HurstUp System","Nikkei 225")
charts.PerformanceSummary(retSys,ylog=TRUE,cex.legend=1.25,
                          colorset=c("cadetblue","darkolivegreen3"))
###########################
