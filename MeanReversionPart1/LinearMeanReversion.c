//////// Simple Linear Mean Reversion Strategy //////////
/////////////////////////////////////////////////////////

//#define USEFILTER //uncomment to use filter based on rate of change of moving average

int lotsOpen() {
	string CurrentAsset = Asset;  
	int val = 0;  
	for(open_trades)    
	if(strstr(Asset,CurrentAsset) && TradeIsOpen)      
	val += TradeLots;  
	return val;
}

function run() {
	set(LOGFILE);
	BarPeriod = 1440;
	StartDate = 2008; 
	EndDate = 2015; 
	Spread =	Slippage = RollShort = RollLong = 0; // set transaction costs to zero
	PlotWidth = 750;

	int halfLife = 150; //311; 
	LookBack = halfLife+1;

	vars Close = series(priceClose());
	vars zScore = series(10*(-(Close[0] - SMA(Close, halfLife))/StdDev(Close, halfLife))); // multiply by 10 as minimum lot size is 1
	int openLots;
	
	// simple filter based on rate of change
	vars ma = series(SMA(Close, halfLife));
	vars delta = series(100 *(ma[0] - ma[1]));
	var threshold = 0.07; //0.045

#ifdef USEFILTER
if (abs(delta[0]) < threshold) {

	if (zScore[0] > 0) { //want to be long the asset
		exitShort();
		openLots = lotsOpen();	
		if (openLots < zScore[0]) {//need to buy more 
			Lots = zScore[0] - openLots;
			enterLong();
			}
		else if (openLots > zScore[0]) { 
			exitLong(0,0,(openLots - zScore[0])); //need to close some 
			}
	}	
	else if (zScore[0] < 0) { //want to be short the asset
		exitLong();	
		openLots = lotsOpen();
		if (openLots < abs(zScore[0])) { //need to sell more 
			Lots = abs(zScore[0]) - openLots;
			enterShort();
			}
		else if (openLots > abs(zScore[0])) { 
			exitShort(0,0,(openLots - abs(zScore[0])));	
			}
	}	
}

else if (abs(delta[0]) > threshold) { // exit open trades if rate of change exceeds threshold
	exitLong("*"); exitShort("*");
}

#else

if (zScore[0] > 0) { //want to be long the asset
		exitShort();
		openLots = lotsOpen();	
		if (openLots < zScore[0]) {//need to buy more 
			Lots = zScore[0] - openLots;
			enterLong();
			}
		else if (openLots > zScore[0]) { 
			exitLong(0,0,(openLots - zScore[0])); //need to close some 
			}
	}	
	else if (zScore[0] < 0) { //want to be short the asset
		exitLong();	
		openLots = lotsOpen();
		if (openLots < abs(zScore[0])) { //need to sell more 
			Lots = abs(zScore[0]) - openLots;
			enterShort();
			}
		else if (openLots > abs(zScore[0])) { 
			exitShort(0,0,(openLots - abs(zScore[0])));	
			}
	}	

#endif

	plot("zScore", zScore, NEW, BLUE);
	plot("MAve", SMA(Close, halfLife), NEW, GREEN);
	plot("MSD", StdDev(Close, halfLife), NEW, RED);
	plot("delta", delta, NEW, BLUE);

}