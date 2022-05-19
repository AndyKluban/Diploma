####Только традиционные фин. активы####
returns <- read_excel("Desktop/Diploma/Данные/Workings/returns2020.xlsx", 
                      sheet = "Portfolio_NFT")

returns <- subset(returns, select = -c(Quarter))
returns_ALL <- returns
names(returns_ALL)
ALL <- subset(returns_ALL, select = -c(BTC, ETH, VC, Private_eqt))
names(ALL)
#1) NFT + BTC, ETH, VC, PE


#Optimal portfolio
mean_ret <- colMeans(ALL)
print(round(mean_ret, 11))
#Корр. матрица
corrplot::corrplot(cor(ALL), method = 'circle')
cov_mat <- cov(ALL) * 12
names(ALL)
#Веса
wts <- runif(n = 11)
print(wts)
print(sum(wts))
wts <- wts/sum(wts)
print(wts)
sum(wts)
port_returns <- (sum(wts * mean_ret) + 1)^12 - 1
port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))
print(port_risk)
#Risk Free = 2.76%
sharpe_ratio <- (port_returns-0.0276)/port_risk
print(sharpe_ratio)

#5000 random portfolios
# 1) Null-vectors for them:
num_port <- 5000
# Creating a matrix to store the weights
all_wts <- matrix(nrow = num_port,
                  ncol = 11)
# Creating an empty vector to store Portfolio returns
port_returns <- vector('numeric', length = num_port)
# Creating an empty vector to store Portfolio Standard deviation
port_risk <- vector('numeric', length = num_port)
# Creating an empty vector to store Portfolio Sharpe Ratio
sharpe_ratio <- vector('numeric', length = num_port)

# 2) Loop
for (i in seq_along(port_returns)) {
  wts <- runif(11)
  wts <- wts/sum(wts)
  # Storing weight in the matrix
  all_wts[i,] <- wts
  # Portfolio returns
  port_ret <- sum(wts * mean_ret)
  port_ret <- ((port_ret + 1)^4) - 1
  # Storing Portfolio Returns values
  port_returns[i] <- port_ret
  # Creating and storing portfolio risk
  port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
  port_risk[i] <- port_sd
  # Creating and storing Portfolio Sharpe Ratios
  # Assuming 0% Risk free rate
  sr <- (port_ret-0.0276)/port_sd
  sharpe_ratio[i] <- sr
}


# Storing the values in the table
portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)

# Converting matrix to a tibble and changing column names
all_wts <- tk_tbl(all_wts)
colnames(all_wts) <- colnames(ALL)
# Combing all the values together
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))
head(portfolio_values)
#lets look at the portfolios that matter the most
min_var <- portfolio_values[which.min(portfolio_values$Risk),]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]
View(min_var)

#the portfolio with the highest sharpe ratio.
p <- max_sr %>%
  gather(SP500:GBP_USD, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Активы', y = 'Доли', title = "Доли активов в портфеле с максимальным коэффициентом Шарпа") +
  scale_y_continuous(labels = scales::percent) 
ggplotly(p)

#the portfoliowith the minimum variance
p <- min_var %>%
  gather(SP500:GBP_USD, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Активы', y = 'Доли', title = "Доли активов в портфеле с минимальной дисперсией") +
  scale_y_continuous(labels = scales::percent) 
ggplotly(p)


####Всё, кроме Арт и NFT####
returns <- read_excel("Desktop/Diploma/Данные/Workings/returns2020.xlsx", 
                      sheet = "Portfolio_NFT")

returns <- subset(returns, select = -c(Quarter))
returns_ALL <- returns
names(returns_ALL)
ALL <- subset(returns_ALL)
names(ALL)
#1) NFT + BTC, ETH, VC, PE


#Optimal portfolio
mean_ret <- colMeans(ALL)
print(round(mean_ret, 15))
#Корр. матрица
corrplot::corrplot(cor(ALL), method = 'circle')
cov_mat <- cov(ALL) * 12
names(ALL)
#Веса
wts <- runif(n = 15)
print(wts)
print(sum(wts))
wts <- wts/sum(wts)
print(wts)
sum(wts)
port_returns <- (sum(wts * mean_ret) + 1)^12 - 1
port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))
print(port_risk)
#Risk Free = 2.76%
sharpe_ratio <- (port_returns-0.0276)/port_risk
print(sharpe_ratio)

#5000 random portfolios
# 1) Null-vectors for them:
num_port <- 5000
# Creating a matrix to store the weights
all_wts <- matrix(nrow = num_port,
                  ncol = 15)
# Creating an empty vector to store Portfolio returns
port_returns <- vector('numeric', length = num_port)
# Creating an empty vector to store Portfolio Standard deviation
port_risk <- vector('numeric', length = num_port)
# Creating an empty vector to store Portfolio Sharpe Ratio
sharpe_ratio <- vector('numeric', length = num_port)

# 2) Loop
for (i in seq_along(port_returns)) {
  wts <- runif(15)
  wts <- wts/sum(wts)
  # Storing weight in the matrix
  all_wts[i,] <- wts
  # Portfolio returns
  port_ret <- sum(wts * mean_ret)
  port_ret <- ((port_ret + 1)^4) - 1
  # Storing Portfolio Returns values
  port_returns[i] <- port_ret
  # Creating and storing portfolio risk
  port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
  port_risk[i] <- port_sd
  # Creating and storing Portfolio Sharpe Ratios
  # Assuming 0% Risk free rate
  sr <- (port_ret-0.0276)/port_sd
  sharpe_ratio[i] <- sr
}


# Storing the values in the table
portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)

# Converting matrix to a tibble and changing column names
all_wts <- tk_tbl(all_wts)
colnames(all_wts) <- colnames(ALL)
# Combing all the values together
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))
head(portfolio_values)
#lets look at the portfolios that matter the most
min_var <- portfolio_values[which.min(portfolio_values$Risk),]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]
View(min_var)

#the portfolio with the highest sharpe ratio.
p <- max_sr %>%
  gather(SP500:VC, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Активы', y = 'Доли', title = "Доли активов в портфеле с максимальным коэффициентом Шарпа") +
  scale_y_continuous(labels = scales::percent) 
ggplotly(p)

#the portfoliowith the minimum variance
p <- min_var %>%
  gather(SP500:VC, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Активы', y = 'Доли', title = "Доли активов в портфеле с минимальной дисперсией") +
  scale_y_continuous(labels = scales::percent) 
ggplotly(p)

####Средние доходности и риск по всем акктивам####
returns <- read_excel("Desktop/Diploma/Данные/Workings/returns2020.xlsx", 
                      sheet = "Portfolio_NFT")
returns <- subset(returns, select = -c(Quarter))
mean(returns$SP500)
