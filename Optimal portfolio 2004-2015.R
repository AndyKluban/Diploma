library(quantmod)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(ROI)
library(ROI.plugin.glpk)
library(DEoptim)
library(dygraphs)
library(tidyverse)
library(timetk)
library(plotly)

####Market Data####

quarter_returns <- function(ticker, base_year)
{
  stock <- getSymbols(ticker, src = "yahoo", from = "2004-01-01", to = "2015-07-01", auto.assign = FALSE) 
  stock <- na.omit(stock)
  stock <- stock[, 6]
  data <- periodReturn(stock, period = "quarterly", type = "log")
  assign(ticker, data, envir = .GlobalEnv)
}



#Акции
FTSE100 <- quarter_returns("^FTSE", 2004)
Nikkei225 <- quarter_returns("^IXIC", 2004)
SP500 <- quarter_returns("^RUT", 2004)
#Government bond market
Treasury5  <- quarter_returns("^FVX", 2004)
Treasury10  <- quarter_returns("^TNX", 2004)
Treasury30  <- quarter_returns("^TYX", 2004)
#Corporate bond market
Corporate  <- quarter_returns("LQD", 2004)
#Real Estate
RealEstate  <- quarter_returns("O", 2004)
#Commodity market (like oil etc)
Oil  <- quarter_returns("CL=F", 2004)
#Metals
Gold  <- quarter_returns("GC=F", 2004)
Silver  <- quarter_returns("SI=F", 2004)
Platinum  <- quarter_returns("PL=F", 2004)
#Currency
EUR_USD  <- quarter_returns("EURUSD=X", 2004)
GBP_USD  <- quarter_returns("GBPUSD=X", 2004)
#Private equity market
Private_eqt  <- quarter_returns("EQT", 2004)

returns <- merge.xts(FTSE100, Nikkei225, SP500, Treasury5, Treasury10, Treasury30,
                     Corporate, RealEstate, Oil, Gold, Silver, Platinum, EUR_USD,
                     GBP_USD, Private_eqt)

returns[3, 12] <- -0.049741401
returns[6, 12] <- 0.003135902
returns[6, 11] <- -0.015760555
returns[6, 10] <- -0.0449008537
returns[6, 9] <- -0.124697787
returns[39, 14] <- -0.0597262128
returns[39, 13] <- -0.0311301520
returns <- returns[-c(2, 5, 40, 50), ]

colnames(returns) <- c('FTSE100', 'Nikkei225', 'SP500', 'Treasury5', 'Treasury10', 'Treasury30',
                       'Corporate', 'RealEstate', 'Oil', 'Gold', 'Silver', 'Platinum', 'EUR_USD',
                       'GBP_USD', 'Private_eqt')
returns <- subset(returns, select =-c(FTSE100, Nikkei225, Treasury10))

###Building portfolio####

returns <- read_excel("Desktop/Diploma/Данные/Workings/returns.xlsx", sheet = 'Market_Adj')
Art <- read_excel("Desktop/Diploma/Данные/Workings/p5.xlsx", sheet = 'ART')
#returnsArt <- returns
returns$Art <- Art$Art_Index

returns$Quater
returns <- subset(returns, select = -c(Quater))
rownames(returns) <- c("2004-01-01","2004-04-01","2004-07-01","2004-12-01",
                       "2005-01-01","2005-04-01","2005-07-01","2005-12-01",
                       "2006-01-01","2006-04-01","2006-07-01","2006-12-01",
                       "2007-01-01","2007-04-01","2007-07-01","2007-12-01",
                       "2008-01-01","2008-04-01","2008-07-01","2008-12-01",
                       "2009-01-01","2009-04-01","2009-07-01","2009-12-01",
                       "2010-01-01","2010-04-01","2010-07-01","2010-12-01",
                       "2011-01-01","2011-04-01","2011-07-01","2011-12-01",
                       "2012-01-01","2012-04-01","2012-07-01","2012-12-01",
                       "2013-01-01","2013-04-01","2013-07-01","2013-12-01",
                       "2014-01-01","2014-04-01","2014-07-01","2014-12-01",
                       "2015-01-01","2015-04-01")

View(returns)

# Produce interactive chart of stock returns
dygraph(returns, main = "Market vs. Art") %>%
  dyAxis("y", label = "Return", valueRange = c(-1,4)) %>%
  dyRangeSelector(dateWindow = c("2004-01-01", "2015-07-01")) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set2")) 

#SP500
names(returns)
Sp500 <- subset(returns, select = c(SP500, Art))
rownames(Sp500) <- c("2004-01-01","2004-04-01","2004-07-01","2004-12-01",
                       "2005-01-01","2005-04-01","2005-07-01","2005-12-01",
                       "2006-01-01","2006-04-01","2006-07-01","2006-12-01",
                       "2007-01-01","2007-04-01","2007-07-01","2007-12-01",
                       "2008-01-01","2008-04-01","2008-07-01","2008-12-01",
                       "2009-01-01","2009-04-01","2009-07-01","2009-12-01",
                       "2010-01-01","2010-04-01","2010-07-01","2010-12-01",
                       "2011-01-01","2011-04-01","2011-07-01","2011-12-01",
                       "2012-01-01","2012-04-01","2012-07-01","2012-12-01",
                       "2013-01-01","2013-04-01","2013-07-01","2013-12-01",
                       "2014-01-01","2014-04-01","2014-07-01","2014-12-01",
                       "2015-01-01","2015-04-01")
dygraph(Sp500, main = "Sp500 vs. Art") %>%
  dyAxis("y", label = "Return", valueRange = c(-1,2)) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"), strokeWidth = 2, fillGraph=TRUE)

t.test(Sp500)

#Treasury
names(returns)
Treasury <- subset(returns, select = c(Treasury5, Art, Treasury30))
rownames(Treasury) <- c("2004-01-01","2004-04-01","2004-07-01","2004-12-01",
                     "2005-01-01","2005-04-01","2005-07-01","2005-12-01",
                     "2006-01-01","2006-04-01","2006-07-01","2006-12-01",
                     "2007-01-01","2007-04-01","2007-07-01","2007-12-01",
                     "2008-01-01","2008-04-01","2008-07-01","2008-12-01",
                     "2009-01-01","2009-04-01","2009-07-01","2009-12-01",
                     "2010-01-01","2010-04-01","2010-07-01","2010-12-01",
                     "2011-01-01","2011-04-01","2011-07-01","2011-12-01",
                     "2012-01-01","2012-04-01","2012-07-01","2012-12-01",
                     "2013-01-01","2013-04-01","2013-07-01","2013-12-01",
                     "2014-01-01","2014-04-01","2014-07-01","2014-12-01",
                     "2015-01-01","2015-04-01")
dygraph(Treasury, main = "Treasury vs. Art") %>%
  dyAxis("y", label = "Return", valueRange = c(-1,2)) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"), strokeWidth = 2,  
            drawPoints = TRUE, pointSize = 3)

t.test(Treasury$Treasury30)

#RealEstate
names(returns)
Real <- subset(returns, select = c(RealEstate, Art))
rownames(Real) <- c("2004-01-01","2004-04-01","2004-07-01","2004-12-01",
                     "2005-01-01","2005-04-01","2005-07-01","2005-12-01",
                     "2006-01-01","2006-04-01","2006-07-01","2006-12-01",
                     "2007-01-01","2007-04-01","2007-07-01","2007-12-01",
                     "2008-01-01","2008-04-01","2008-07-01","2008-12-01",
                     "2009-01-01","2009-04-01","2009-07-01","2009-12-01",
                     "2010-01-01","2010-04-01","2010-07-01","2010-12-01",
                     "2011-01-01","2011-04-01","2011-07-01","2011-12-01",
                     "2012-01-01","2012-04-01","2012-07-01","2012-12-01",
                     "2013-01-01","2013-04-01","2013-07-01","2013-12-01",
                     "2014-01-01","2014-04-01","2014-07-01","2014-12-01",
                     "2015-01-01","2015-04-01")
dygraph(Real, main = "Real vs. Art") %>%
  dyAxis("y", label = "Return", valueRange = c(-1,2)) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"), strokeWidth = 2, fillGraph=TRUE) 

t.test(Real$RealEstate)

#Metals
names(returns)
Metals <- subset(returns, select = c(Gold, Art, Silver, Platinum))
rownames(Metals) <- c("2004-01-01","2004-04-01","2004-07-01","2004-12-01",
                    "2005-01-01","2005-04-01","2005-07-01","2005-12-01",
                    "2006-01-01","2006-04-01","2006-07-01","2006-12-01",
                    "2007-01-01","2007-04-01","2007-07-01","2007-12-01",
                    "2008-01-01","2008-04-01","2008-07-01","2008-12-01",
                    "2009-01-01","2009-04-01","2009-07-01","2009-12-01",
                    "2010-01-01","2010-04-01","2010-07-01","2010-12-01",
                    "2011-01-01","2011-04-01","2011-07-01","2011-12-01",
                    "2012-01-01","2012-04-01","2012-07-01","2012-12-01",
                    "2013-01-01","2013-04-01","2013-07-01","2013-12-01",
                    "2014-01-01","2014-04-01","2014-07-01","2014-12-01",
                    "2015-01-01","2015-04-01")
dygraph(Metals, main = "Metals vs. Art") %>%
  dyAxis("y", label = "Return", valueRange = c(-1,6)) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"), strokeWidth = 2,  
            drawPoints = TRUE, pointSize = 3) 

t.test(Metals$Platinum)

#Oil
names(returns)
Oil <- subset(returns, select = c(Oil, Art))
rownames(Oil) <- c("2004-01-01","2004-04-01","2004-07-01","2004-12-01",
                      "2005-01-01","2005-04-01","2005-07-01","2005-12-01",
                      "2006-01-01","2006-04-01","2006-07-01","2006-12-01",
                      "2007-01-01","2007-04-01","2007-07-01","2007-12-01",
                      "2008-01-01","2008-04-01","2008-07-01","2008-12-01",
                      "2009-01-01","2009-04-01","2009-07-01","2009-12-01",
                      "2010-01-01","2010-04-01","2010-07-01","2010-12-01",
                      "2011-01-01","2011-04-01","2011-07-01","2011-12-01",
                      "2012-01-01","2012-04-01","2012-07-01","2012-12-01",
                      "2013-01-01","2013-04-01","2013-07-01","2013-12-01",
                      "2014-01-01","2014-04-01","2014-07-01","2014-12-01",
                      "2015-01-01","2015-04-01")
dygraph(Oil, main = "Oil vs. Art") %>%
  dyAxis("y", label = "Return", valueRange = c(-1,6)) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"), strokeWidth = 2,  
            drawPoints = TRUE, pointSize = 3) 

t.test(Oil)

#Currency
names(returns)
Currency <- subset(returns, select = c(EUR_USD, Art, GBP_USD))
rownames(Currency) <- c("2004-01-01","2004-04-01","2004-07-01","2004-12-01",
                   "2005-01-01","2005-04-01","2005-07-01","2005-12-01",
                   "2006-01-01","2006-04-01","2006-07-01","2006-12-01",
                   "2007-01-01","2007-04-01","2007-07-01","2007-12-01",
                   "2008-01-01","2008-04-01","2008-07-01","2008-12-01",
                   "2009-01-01","2009-04-01","2009-07-01","2009-12-01",
                   "2010-01-01","2010-04-01","2010-07-01","2010-12-01",
                   "2011-01-01","2011-04-01","2011-07-01","2011-12-01",
                   "2012-01-01","2012-04-01","2012-07-01","2012-12-01",
                   "2013-01-01","2013-04-01","2013-07-01","2013-12-01",
                   "2014-01-01","2014-04-01","2014-07-01","2014-12-01",
                   "2015-01-01","2015-04-01")
dygraph(Currency, main = "Currency vs. Art") %>%
  dyAxis("y", label = "Return", valueRange = c(-1,2)) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"), strokeWidth = 2,  
            drawPoints = TRUE, pointSize = 3) 

t.test(Currency$GBP_USD)

#Private_eqt
names(returns)
Private_eqt <- subset(returns, select = c(Private_eqt, Art))
rownames(Private_eqt) <- c("2004-01-01","2004-04-01","2004-07-01","2004-12-01",
                        "2005-01-01","2005-04-01","2005-07-01","2005-12-01",
                        "2006-01-01","2006-04-01","2006-07-01","2006-12-01",
                        "2007-01-01","2007-04-01","2007-07-01","2007-12-01",
                        "2008-01-01","2008-04-01","2008-07-01","2008-12-01",
                        "2009-01-01","2009-04-01","2009-07-01","2009-12-01",
                        "2010-01-01","2010-04-01","2010-07-01","2010-12-01",
                        "2011-01-01","2011-04-01","2011-07-01","2011-12-01",
                        "2012-01-01","2012-04-01","2012-07-01","2012-12-01",
                        "2013-01-01","2013-04-01","2013-07-01","2013-12-01",
                        "2014-01-01","2014-04-01","2014-07-01","2014-12-01",
                        "2015-01-01","2015-04-01")
dygraph(Private_eqt, main = "Private_eqt vs. Art") %>%
  dyAxis("y", label = "Return", valueRange = c(-1,10)) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"), strokeWidth = 2,  
            fillGraph = TRUE) 

t.test(Private_eqt$Private_eqt)


# Print last 5 rows of the data, rounded to 4 decimal places
round(tail(returns, n = 5), 4)

#Portfolio composition
corrplot::corrplot(cor(returns), method = 'circle')

###Building portfolio ART + Traditional###
names(returns)
Art_Traditional <- subset(returns, select = c(SP500, Art, Treasury5, Treasury10, Corporate, 
                                          RealEstate, Oil, Gold, EUR_USD))
rownames(Art_Traditional) <- c("2004-01-01","2004-04-01","2004-07-01","2004-12-01",
                           "2005-01-01","2005-04-01","2005-07-01","2005-12-01",
                           "2006-01-01","2006-04-01","2006-07-01","2006-12-01",
                           "2007-01-01","2007-04-01","2007-07-01","2007-12-01",
                           "2008-01-01","2008-04-01","2008-07-01","2008-12-01",
                           "2009-01-01","2009-04-01","2009-07-01","2009-12-01",
                           "2010-01-01","2010-04-01","2010-07-01","2010-12-01",
                           "2011-01-01","2011-04-01","2011-07-01","2011-12-01",
                           "2012-01-01","2012-04-01","2012-07-01","2012-12-01",
                           "2013-01-01","2013-04-01","2013-07-01","2013-12-01",
                           "2014-01-01","2014-04-01","2014-07-01","2014-12-01",
                           "2015-01-01","2015-04-01")
dygraph(Art_Traditional, main = "Traditional assets vs. Art") %>%
  dyAxis("y", label = "Return", valueRange = c(-1,2.5)) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(12, "Set1"), strokeWidth = 3,  
            drawPoints = TRUE, pointSize = 3) 



# Print last 5 rows of the data, rounded to 4 decimal places
round(tail(returns, n = 5), 4)

#Portfolio composition
corrplot::corrplot(cor(Art_Traditional), method = 'circle')


# Assign weights
Art_Traditional <- subset(returns, select = c(SP500, Treasury5, Treasury10, Corporate, 
                                              RealEstate, Oil, Gold, EUR_USD, Art))
rownames(Art_Traditional) <- c("2004-01-01","2004-04-01","2004-07-01","2004-12-01",
                               "2005-01-01","2005-04-01","2005-07-01","2005-12-01",
                               "2006-01-01","2006-04-01","2006-07-01","2006-12-01",
                               "2007-01-01","2007-04-01","2007-07-01","2007-12-01",
                               "2008-01-01","2008-04-01","2008-07-01","2008-12-01",
                               "2009-01-01","2009-04-01","2009-07-01","2009-12-01",
                               "2010-01-01","2010-04-01","2010-07-01","2010-12-01",
                               "2011-01-01","2011-04-01","2011-07-01","2011-12-01",
                               "2012-01-01","2012-04-01","2012-07-01","2012-12-01",
                               "2013-01-01","2013-04-01","2013-07-01","2013-12-01",
                               "2014-01-01","2014-04-01","2014-07-01","2014-12-01",
                               "2015-01-01","2015-04-01")

# Assign weights
wts <- c(1/3, 1/3, 1/3)

# Construct a portfolio using our returns object and weights
# Only select first three columns to isolate our individual stock data
portfolio_returns <- Return.portfolio(R = returns[,1:3], weights = wts, wealth.index = TRUE)

# Then isolate our S&P 500 data
benchmark_returns <- Return.portfolio(R = returns[,4], wealth.index = TRUE)

# Merge the two
comp <- merge.xts(portfolio_returns, benchmark_returns)
colnames(comp) <- c("Portfolio", "Benchmark")

# Build an interactive graph to compare performance
dygraph(comp, main = "Portfolio Performance vs. Benchmark") %>%
  dyAxis("y", label = "Amount ($)")




###############
names(Art_Traditional)
wts <- c(1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8)
sum(wts)
# Construct a portfolio using our returns object and weights
# Only select first three columns to isolate our individual stock data
portfolio_returns <- Return.portfolio(R = Art_Traditional[,1:8], weights = wts, 
                                      rebalance_on = NA)
?Return.portfolio
View(returns$Art)
# Then isolate our S&P 500 data
wts1 <- c(1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9)
benchmark_returns <- Return.portfolio(R = returns[,1:12], weights = wt1, wealth.index = TRUE)

# Merge the two
comp <- merge.xts(portfolio_returns, benchmark_returns)
colnames(comp) <- c("Portfolio", "Benchmark")

# Build an interactive graph to compare performance
dygraph(comp, main = "Portfolio Performance vs. Benchmark") %>%
  dyAxis("y", label = "Amount ($)")

ret <- (returns %*% wts + 1)
cumprod(ret)
ret1 <- (returns[,1:12] %*% wt1 + 1)
cumprod(ret1)
xts(ret, order.by = rownames(returns))


comp <- merge.xts(ret, ret1)
colnames(comp) <- c("Portfolio", "Benchmark")

# Assign weights
wts <- c(1/3, 1/3, 1/3)

# Construct a portfolio using our returns object and weights
# Only select first three columns to isolate our individual stock data
portfolio_returns <- Return.portfolio(R = Art_Traditional[,1:3],
                                      weights = wts, wealth.index = TRUE)

# Then isolate our S&P 500 data
benchmark_returns <- Return.portfolio(R = returns[,4], wealth.index = TRUE)

# Merge the two
comp <- merge.xts(portfolio_returns, benchmark_returns)
colnames(comp) <- c("Portfolio", "Benchmark")

# Build an interactive graph to compare performance
dygraph(comp, main = "Portfolio Performance vs. Benchmark") %>%
  dyAxis("y", label = "Amount ($)")


###############

returns <- read_excel("Desktop/Diploma/Данные/Workings/returns.xlsx", sheet = 'Portfolio')
Art <- read_excel("Desktop/Diploma/Данные/Workings/p5.xlsx", sheet = 'Portfolio_Art')
returns$Art <- Art$Art_Index
returns <- subset(returns, select = -c(Quater))

#returnsArt <- returns

#Info from https://towardsdatascience.com/building-and-testing-stock-portfolios-in-r-d1b7b6f59ac4

#Optimal portfolio
Art_Traditional <- subset(returns, select = c(SP500, Treasury5, Treasury10, Corporate, 
                                              RealEstate, Oil, Gold, EUR_USD, Art))
mean_ret <- colMeans(Art_Traditional)
print(round(mean_ret, 3))
names(Art_Traditional)
View(Art_Traditional)
#Корр. матрица
corrplot::corrplot(cor(Art_Traditional), method = 'number', type = 'lower', tl.col="black")
cov_mat <- cov(Art_Traditional) * 4

#Веса
wts <- runif(n = 9)
print(wts)
print(sum(wts))
wts <- wts/sum(wts)
print(wts)
sum(wts)
port_returns <- (sum(wts * mean_ret) + 1)^4 - 1
port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))
print(port_risk)
#Risk Free = 3.91%
sharpe_ratio <- (port_returns-0.0391/port_risk)
print(sharpe_ratio)

#5000 random portfolios
# 1) Null-vectors for them:
num_port <- 5000
# Creating a matrix to store the weights
all_wts <- matrix(nrow = num_port,
                  ncol = 9)
# Creating an empty vector to store Portfolio returns
port_returns <- vector('numeric', length = num_port)
# Creating an empty vector to store Portfolio Standard deviation
port_risk <- vector('numeric', length = num_port)
# Creating an empty vector to store Portfolio Sharpe Ratio
sharpe_ratio <- vector('numeric', length = num_port)

# 2) Loop
for (i in seq_along(port_returns)) {
  wts <- runif(9)
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
colnames(all_wts) <- colnames(Art_Traditional)
# Combing all the values together
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))
head(portfolio_values)
#lets look at the portfolios that matter the most
min_var <- portfolio_values[which.min(portfolio_values$Risk),]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]
View(min_var)
View(max_sr)
#the portfoliowith the minimum variance
p <- min_var %>%
  gather(SP500:Art, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 
ggplotly(p)

#the portfolio with the highest sharpe ratio.
p <- max_sr %>%
  gather(SP500:Art, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Tangency Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 
ggplotly(p)

#all the random portfolios and visualize the efficient frontier.
p <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Annualized Risk',
       y = 'Annualized Returns',
       title = "Portfolio Optimization & Efficient Frontier") +
  geom_point(aes(x = Risk,
                 y = Return), data = min_var, color = 'red') +
  geom_point(aes(x = Risk,
                 y = Return), data = max_sr, color = 'red') +
  annotate('text', x = 0.15, y = 0.18, label = "Tangency Portfolio") +
  annotate('text', x = 0.1, y = 0.01, label = "Minimum variance portfolio") +
  annotate(geom = 'segment', x = 0.08, xend = 0.07,  y = 0.02, 
           yend = 0.07, color = 'red', arrow = arrow(type = "open")) +
  annotate(geom = 'segment', x = 0.1, xend = 0.15,  y = 0.12, 
           yend = 0.17, color = 'red', arrow = arrow(type = "open"))

ggplotly(p)



####Art Index####
ArtIndex <- read_excel("Desktop/Diploma/Данные/Workings/p5.xlsx", 
    sheet = "ART")
View(ArtIndex)

#Нарисуем изменения индекса
BOD1 <- ArtIndex
BOD1$Quater <- factor(BOD1$Quater)
v <- ggplot(BOD1, aes(x=Quater, y=Art_Index, group=1)) + 
  geom_line(colour="dodgerblue2") + 
  geom_point(shape=21, size=3, fill="white") + 
  theme(axis.text = element_text(angle = 90, color="black", size=10, face=3)) +
  geom_smooth(method=lm, se=FALSE, col='darkred', size=0.5)

ggplotly(v)

#Optimization from https://www.codingfinance.com/post/2018-05-31-portfolio-opt-in-r/

