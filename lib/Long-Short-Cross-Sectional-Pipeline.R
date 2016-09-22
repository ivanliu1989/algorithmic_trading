# It looks at an N day window of M day returns on a basket of large
# cap stocks, then the cross-sectional average for each day is subtracted
# out. I then use the average of the result as a ranking for the universe,
# long the top and short the bottom in equal amounts.

compute.rank <- function(prices, window){
  # Getting the range of indexes so we can reindex later
  index = c(1:nrow(prices))
  
  # Calculated a shifted return
  prices = log(prices)
  R = diff(prices, lag = window)
  R[is.infinite(R)] = 0
  
  # Substracts the cross-sectional average out of each data point on each day
  ranks = rowMeans(t(t(R) - rowMeans(t(R))))
  
  # Fill in nan values so we can drop them later
  ranks = ranks[index, ]
  return(as.vector(ranks))
}



make.pipeline <- function(){
  # The number of days before/after an announcement that you want to
  # avoid an earnings for.
  avoid_earnings_days = 15
  
  # Create and apply a filter representing the top 1000 equities by MarketCap
  # every day.
  # mkt_cap = morningstar.valuation.market_cap.latest
  # mkt_cap_top = mkt_cap.top(1000)
  
  # Liquidity floor
  dollar_volume = AverageDollarVolume(window_length=30)
  
  # Set our screens
  pipe.set_screen((ne.isnan() | (ne > avoid_earnings_days) |
                     (pe > avoid_earnings_days)) &
                    (dollar_volume > 10**7) &
                    mkt_cap_top)
}

initialize <- list(
  longleverage = 1.0,
  shortleverage = -1.0,
  days.offset = 15,
  return.window = 50,
  lookback = 300,
  capit = 100000
)

trade <- function(context, data){
  # Order our securities and exit any stocks that are not a part
  # of our daily universe
  for(stock in context$shorts$index){
    if(stock %in% data){
      order_target_percent(stock, context.shortleverage/ len(context.shorts))
    }
  }
   
  for(stock in context$longs$index){
    if(stock %in% data){
      order_target_percent(stock, context.longleverage / len(context.longs))
    }
  }
  
  for(stock in context$portfolio$positions){
    if(!(stock %in% context$longs$index) & !(stock %in% context$short$index)){
      order_target(stock, 0)
    }
  }
}

before.trading.start <- function(context, data){
  # Get the top 7% and bottom 7% of performers and set them as our
  # longs and shorts
  results = diff(data)
  lower = quantile(results, probs = c(0.05, 0.95))[1]
  upper = quantile(results, probs = c(0.05, 0.95))[2]
  
  shorts = results[results['returns'] <= lower]
  shorts = shorts[shorts['article_sentiment']*shorts['impact_score'] < 5]
  
  longs = results[results['returns'] >= upper]
  longs = longs[longs['article_sentiment']*longs['article_sentiment'] > -5]
  update_universe(longs.index | shorts.index)
}

