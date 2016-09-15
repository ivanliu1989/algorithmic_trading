function run() {
	set(LOGFILE|PARAMETERS);
	BarPeriod = 1440;
	GapDays = 2;
	StartDate = 2009; 
	EndDate =   20150708; 
	BarPeriod = 1440;
	int halfLife = 41;
	LookBack = halfLife + 1;
	TradesPerBar = 3;
	PlotWidth = 650;
	Hedge = 2;
	
	var BetaY = 1.0;
	var BetaX = -0.581; 
	var BetaZ = -1.124;
	string asset_y = "AUD/USD"; 
	string asset_x = "NZD/USD"; 
	string asset_z = "USD/CAD"; 
		
	asset(asset_y);
	vars y = series(priceClose());
	asset(asset_x);
	vars x = series(priceClose());
	asset(asset_z);
	vars z = series(1/priceClose());
		
	vars sprd = series(BetaY*y[0] + BetaX*x[0] + BetaZ*z[0]);
	vars Zscore = series((sprd[0] - SMA(sprd, halfLife))/StdDev(sprd, halfLife));

//entries
var entryZscore1 = 2;
int entry1Lots = 100;

//exits
var exitZscore1 = 1;
int exit1Lots = 100;

//trade management
//static bool entered1, entered2;
static int entered1, entered2;
//if (is(INITRUN)) { entered1 = false; entered2 = false;}
if (is(INITRUN)) { entered1 = 0; entered2 = 0;}

//long unit portfolio entries
if (crossUnder(Zscore, -entryZscore1) and entered1 == 0) {
	entered1 += 1;
	asset(asset_y); enterLong(BetaY*entry1Lots);
	asset(asset_x); 
	if (BetaX < 0) enterShort(abs(BetaX)*entry1Lots); //need to account for the sign of the regression coefficient where positive B results in selling the asset when long the unit portfolio
	else if (BetaX > 0) enterLong(BetaX*entry1Lots);
	asset(asset_z);
	if (BetaZ < 0) enterLong(abs(BetaZ)*entry1Lots);
	else if (BetaZ > 0) enterShort(BetaZ*entry1Lots);
	
	}

//long unit portfolio exits
if (crossOver(Zscore, -exitZscore1) and entered1 == 1) {
	asset(asset_y); exitLong(0);
	asset(asset_x); 
	if (BetaX < 0) exitShort(0); 
	if (BetaX > 0) exitLong(0); 
	asset(asset_z); 
	if (BetaZ < 0) exitLong(0); 
	if (BetaZ > 0) exitShort(0);
	entered1 = 0;
	}
	
//short unit portfolio entries
if (crossOver(Zscore, entryZscore1) and entered2 == 0) {
	entered2 += 1;
	asset(asset_y); enterShort(BetaY*entry1Lots);
	asset(asset_x); 
	if (BetaX < 0) enterLong(abs(BetaX)*entry1Lots); //need to account for the sign of the regression coefficient where positive B results in selling the asset when long the unit portfolio
	else if (BetaX > 0) enterShort(BetaX*entry1Lots);
	asset(asset_z);
	if (BetaZ < 0) enterShort(abs(BetaZ)*entry1Lots);
	else if (BetaZ > 0) enterLong(BetaZ*entry1Lots);
	
}

//short unit portfolio exits
if (crossUnder(Zscore, exitZscore1) and entered2 == 1) {
	asset(asset_y); exitShort(0);
	asset(asset_x);
	if (BetaX < 0) exitLong(0); 
	if (BetaX > 0) exitShort(0); 
	asset(asset_z); 
	if (BetaZ < 0) exitShort(0);
	if (BetaZ > 0) exitLong(0); 
	entered2 = 0;
	}
	
	asset(asset_y);

//	plot("e1", entered1, NEW, RED);
//	plot("e2", entered2, 0, BLUE);
	plot("sprd", sprd, NEW, BLUE);
	plot("Zscore", Zscore, NEW, RED);
	plot("entryZscore", entryZscore1, 0, BLACK);
	plot("exitZscore", exitZscore1, 0, BLUE);
	plot("-entryZscore", -entryZscore1, 0, BLACK);
	plot("-exitZscore", -exitZscore1, 0, BLUE);
}