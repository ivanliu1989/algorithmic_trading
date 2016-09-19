int lotsOpen() {
	string CurrentAsset = Asset; // Asset is changed in the for loop  
	int val = 0;  
	for(open_trades)    
	if(strstr(Asset,CurrentAsset) && TradeIsOpen)      
	val += TradeLots;  
	return val;
}

function run() {
	set(LOGFILE|PARAMETERS);
	BarPeriod = 1440;
	StartDate = 2009; 
	EndDate = 20150708; 
	
	TradesPerBar = 3;
	PlotWidth = 650;
	
	int halfLife = 41; 
	LookBack = halfLife+1;
	
	var BetaY = 1.0;
	var BetaX = -0.581; 
	var BetaZ = -1.124;
	string asset_y = "AUD/USD"; 
	string asset_x = "NZD/USD"; 
	string asset_z = "USD/CAD";  
	asset(asset_y);
	Spread =	Slippage = RollShort = RollLong = 0;
	vars y = series(priceClose());
	asset(asset_x);
	Spread =	Slippage = RollShort = RollLong = 0;
	vars x = series(priceClose());
	asset(asset_z);
	Spread =	Slippage = RollShort = RollLong = 0;
	vars z = series(1/priceClose());
	
	vars sprd = series(BetaY*y[0] + BetaX*x[0] + BetaZ*z[0]);
	vars zScore = series(-(sprd[0] - SMA(sprd, halfLife))/StdDev(sprd, halfLife));

	var posValY = 100*BetaY*zScore[0];
	var posValX = 100*BetaX*zScore[0];
	var posValZ = 100*BetaZ*zScore[0];
	
	int targetLots_y = posValY;
	int targetLots_x = posValX;
	int targetLots_z = posValZ;
	
	asset(asset_y);
	int lotsOpen_y;
	
	if (targetLots_y > 0) { //want to be long asset_y
		exitShort();
		lotsOpen_y = lotsOpen();	
		if (lotsOpen_y < targetLots_y) {//need to buy more y
			Lots = targetLots_y - lotsOpen_y;
			enterLong();
			}
		else if (lotsOpen_y > targetLots_y) { 
			exitLong(0,0,(lotsOpen_y - targetLots_y)); //need to close some y
			}
	}	
	else if (targetLots_y < 0) { //want to be short asset_y
		exitLong();	
		lotsOpen_y = lotsOpen();
		if (lotsOpen_y < abs(targetLots_y)) { //need to sell more y
			Lots = abs(targetLots_y) - lotsOpen_y;
			enterShort();
			}
		else if (lotsOpen_y > abs(targetLots_y)) { 
			exitShort(0,0,(lotsOpen_y - abs(targetLots_y)));	
			}
	}	
	
	asset(asset_x);
	int lotsOpen_x;
	if (targetLots_x > 0) { //want to be long asset_x
		exitShort();
		lotsOpen_x = lotsOpen();	
		if (lotsOpen_x < targetLots_x) { //need to buy more x
			Lots = targetLots_x - lotsOpen_x;
			enterLong();
		}
		else if (lotsOpen_x > targetLots_x) exitLong(0,0,(lotsOpen_x-targetLots_x));
	}
	else if (targetLots_x < 0) { //want to be short asset_x
		exitLong();
		lotsOpen_x = lotsOpen();
		if (lotsOpen_x < abs(targetLots_x)) { //need to sell more x
			Lots = abs(targetLots_x) - lotsOpen_x;
			enterShort();
		}
		else if (lotsOpen_x > abs(targetLots_x)) exitShort(0,0,(lotsOpen_x - abs(targetLots_x)));
	}
	
	asset(asset_z);
	int lotsOpen_z;
	if (targetLots_z > 0) { //want to be long asset_z
		exitLong();
		lotsOpen_z = lotsOpen();	
		if (lotsOpen_z < targetLots_z) { //need to buy more z
			Lots = targetLots_z - lotsOpen_z;
			enterShort();
		}
		else if (lotsOpen_z > targetLots_z) exitShort(0,0,(lotsOpen_z-targetLots_z));
	}
	else if (targetLots_z < 0) { //want to be short asset_z
		exitShort();
		lotsOpen_z = lotsOpen();
		if (lotsOpen_z < abs(targetLots_z)) { //need to sell more z
			Lots = abs(targetLots_z) - lotsOpen_z;
			enterLong();
		}
		else if (lotsOpen_z > abs(targetLots_z)) exitLong(0,0,(lotsOpen_z - abs(targetLots_z)));
	}
	
	lotsOpen_z = lotsOpen();
	asset(asset_x);
	lotsOpen_x = lotsOpen();
	asset(asset_y);
	lotsOpen_y = lotsOpen();
	if (lotsOpen_y != abs(targetLots_y) or lotsOpen_x != abs(targetLots_x) or lotsOpen_z != abs(targetLots_z))
	printf("something is wrong: lots open is not equal to target lots");
	
	PlotHeight2 = 200;
//	plot("asset_x", x, MAIN, BLUE);
//	plot("asset_z", z, MAIN, YELLOW);
	ColorUp = ColorDn = ColorWin = ColorLoss = 0;
	plot("sprd", sprd, MAIN, BLACK);
	plot("zScore", zScore, NEW, RED);
}