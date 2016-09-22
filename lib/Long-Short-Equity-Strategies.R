current_factor_values = runif(n = 10000, min = -1, max = 1)
equity_names = paste0("Equity", 1:10000)
factor_data = data.frame(FactorValue = current_factor_values)
rownames(factor_data) = equity_names
head(factor_data)

future_returns = current_factor_values + runif(10000, -1, 1)
returns_data = data.frame(Returns = future_returns)
rownames(returns_data) = equity_names

data = cbind(returns_data,factor_data)
head(data)

# Rank the equities
ranked_data = data[order(data$FactorValue),]

# Compute the returns of each basket
# Baskets of size 500
number_of_baskets = 10000/500
basket_returns = rep(x = 0, number_of_baskets)

for(i in 1:number_of_baskets){
  start = i * 500
  end = i * 500 + 500
  basket_returns[i] = mean(ranked_data[start:end,"Returns"])
}

barplot(basket_returns)
basket_returns[number_of_baskets-1]-basket_returns[1]

# Ranking Systems
# 1. could be machine learning model to predict 1 month ahead and rank based on that
# 2. A price-based mean reversion may be predictive over a few days
# 3. While a value-based factor model may be predictive over many months.