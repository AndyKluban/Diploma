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
library(aTSA)

####Market Data####
quarter_returns <- function(ticker, base_year)
{
  stock <- getSymbols(ticker, src = "yahoo", from = "2017-01-01", to = "2022-04-01", auto.assign = FALSE) 
  stock <- na.omit(stock)
  stock <- stock[, 6]
  data <- periodReturn(stock, period = "monthly", type = "log")
  assign(ticker, data, envir = .GlobalEnv)
}

#Акции
FTSE100 <- quarter_returns("^FTSE", 2017)
Nikkei225 <- quarter_returns("^IXIC", 2017)
SP500 <- quarter_returns("^RUT", 2017)
#Government bond market
Treasury5  <- quarter_returns("^FVX", 2017)
Treasury10  <- quarter_returns("^TNX", 2017)
Treasury30  <- quarter_returns("^TYX", 2017)
#Corporate bond market
Corporate  <- quarter_returns("LQD", 2017)
#Real Estate
RealEstate  <- quarter_returns("O", 2017)
#Commodity market (like oil etc)
Oil  <- quarter_returns("CL=F", 2017)
#Metals
Gold  <- quarter_returns("GC=F", 2017)
Silver  <- quarter_returns("SI=F", 2017)
Platinum  <- quarter_returns("PL=F", 2017)
#Currency
EUR_USD  <- quarter_returns("EURUSD=X", 2017)
GBP_USD  <- quarter_returns("GBPUSD=X", 2017)
#Private equity market
PE  <- quarter_returns("EQT", 2017)
#Добавим BTC
BTC  <- quarter_returns("BTC", 2017)
#Добавим ETH
ETH  <- quarter_returns("ETH", 2017)
#Добавим Venture Capital
VC  <- quarter_returns("LDVIX", 2017)

stock <- read_csv("Desktop/Diploma/Данные/Workings/LDVIX.csv")
stock <- na.omit(stock)
stock3 <- xts(stock$`Adj Close`, order.by = as.Date(stock$Date))
VC <- periodReturn(stock3, period = "monthly", type = "log")
View(VC)


############################################Общая табличка
returns <- merge.xts(FTSE100, Nikkei225, SP500, Treasury5, Treasury10, Treasury30,
                     Corporate, RealEstate, Oil, Gold, Silver, Platinum, EUR_USD,
                     GBP_USD, Private_eqt, BTC, ETH)

colnames(returns) <- c('FTSE100', 'Nikkei225', 'SP500', 'Treasury5', 'Treasury10', 'Treasury30',
                       'Corporate', 'RealEstate', 'Oil', 'Gold', 'Silver', 'Platinum', 'EUR_USD',
                       'GBP_USD', 'Private_eqt', 'BTC', 'ETH')
returns <- subset(returns, select =-c(FTSE100, Nikkei225, Treasury10))
View(returns)

############################################Отсчётный год
#Переделаем всё в excel и сложим логарифмические доходности
#write.xlsx(returns, 'returns2020.xlsx')
returns <- read_excel("Desktop/Diploma/Данные/Workings/returns2020.xlsx", 
                          sheet = "Market_Adj")


###Building portfolio####

returns <- read_excel("Desktop/Diploma/Данные/Workings/returns2020.xlsx", 
                      sheet = "Market_Adj")
returns <- subset(returns, select = -c(Monthly))

rownames(returns) <- c("2017-01-01","2017-02-01","2017-03-01","2017-04-01", "2017-05-01","2017-06-01","2017-07-01","2017-08-01",
                               "2017-09-01","2017-10-01","2017-11-01","2017-12-01",
                               "2018-01-01","2018-02-01","2018-03-01","2018-04-01", "2018-05-01","2018-06-01","2018-07-01","2018-08-01",
                               "2018-09-01","2018-10-01","2018-11-01","2018-12-01",
                               "2019-01-01","2019-02-01","2019-03-01","2019-04-01", "2019-05-01","2019-06-01","2019-07-01","2019-08-01",
                               "2019-09-01","2019-10-01","2019-11-01","2019-12-01",
                               "2020-01-01","2020-02-01","2020-03-01","2020-04-01", "2020-05-01","2020-06-01","2020-07-01","2020-08-01",
                               "2020-09-01","2020-10-01","2020-11-01","2020-12-01",
                               "2021-01-01","2021-02-01","2021-03-01","2021-04-01", "2021-05-01","2021-06-01","2021-07-01","2021-08-01",
                               "2021-09-01","2021-10-01","2021-11-01","2021-12-01",
                               "2022-01-01","2022-02-01","2022-03-01")

dygraph(returns, main = "Market") %>%
  dyAxis("y", label = "Return", valueRange = c(-2,5)) %>%
  dyRangeSelector(dateWindow = c("2017-01-01", "2022-03-01")) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set2")) 


####Добавим NFT####
#Меняем, так как меняется первый год
returns <- read_excel("Desktop/Diploma/Данные/Workings/returns2020.xlsx", 
                      sheet = "Market_Adj_NFT")
all_NFT <- read_excel("Desktop/Diploma/Данные/Workings/All indiices.xlsx", sheet = 'NFT')
returns <- subset(returns, select = -c(Monthly))

returns_NFT <- returns
returns_NFT$NFT <- all_NFT$ALL_NFT
rownames(returns_NFT) <- c("2017-06-01","2017-07-01","2017-08-01",
                           "2017-09-01","2017-10-01","2017-11-01","2017-12-01",
                           "2018-01-01","2018-02-01","2018-03-01","2018-04-01", "2018-05-01","2018-06-01","2018-07-01","2018-08-01",
                           "2018-09-01","2018-10-01","2018-11-01","2018-12-01",
                           "2019-01-01","2019-02-01","2019-03-01","2019-04-01", "2019-05-01","2019-06-01","2019-07-01","2019-08-01",
                           "2019-09-01","2019-10-01","2019-11-01","2019-12-01",
                           "2020-01-01","2020-02-01","2020-03-01","2020-04-01", "2020-05-01","2020-06-01","2020-07-01","2020-08-01",
                           "2020-09-01","2020-10-01","2020-11-01","2020-12-01",
                           "2021-01-01","2021-02-01","2021-03-01","2021-04-01", "2021-05-01","2021-06-01","2021-07-01","2021-08-01",
                           "2021-09-01","2021-10-01","2021-11-01","2021-12-01",
                           "2022-01-01","2022-02-01","2022-03-01")

# Produce interactive chart of stock returns
dygraph(returns_NFT, main = "Market") %>%
  dyAxis("y", label = "Return", valueRange = c(-2,8)) %>%
  dyRangeSelector(dateWindow = c("2017-06-01", "2022-03-01")) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(13, "Set1")) 


#PE
names(returns_NFT)
Private_eqt <- subset(returns_NFT, select = c(Private_eqt, NFT))
rownames(Private_eqt) <- c("2020-11-01","2020-12-01",
                      "2021-01-01","2021-02-01","2021-03-01","2021-04-01", "2021-05-01","2021-06-01","2021-07-01","2021-08-01",
                      "2021-09-01","2021-10-01","2021-11-01","2021-12-01",
                      "2022-01-01","2022-02-01","2022-03-01")

dygraph(Private_eqt, main = "Private_eqt vs NFT") %>%
  dyAxis("y", label = "Return", valueRange = c(-1,13)) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"), strokeWidth = 2,  
            fillGraph = TRUE)

t.test(Private_eqt$NFT)

#Venture Capital
names(returns_NFT)
Venture_capital <- subset(returns_NFT, select = c(VC, NFT))
rownames(Venture_capital) <- c("2020-11-01","2020-12-01",
                           "2021-01-01","2021-02-01","2021-03-01","2021-04-01", "2021-05-01","2021-06-01","2021-07-01","2021-08-01",
                           "2021-09-01","2021-10-01","2021-11-01","2021-12-01",
                           "2022-01-01","2022-02-01","2022-03-01")

dygraph(Venture_capital, main = "Venture_capital vs NFT") %>%
  dyAxis("y", label = "Return", valueRange = c(-1,13)) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"), strokeWidth = 2,  
            fillGraph = TRUE)

t.test(Venture_capital)

#Crypto
returns <- Data_for_NFT_graph <- read_excel("Desktop/Diploma/Данные/Workings/Data_for_NFT_graph.xlsx", 
                      sheet = "Market")
all_NFT <- read_excel("Desktop/Diploma/Данные/Workings/Data_for_NFT_graph.xlsx", sheet = 'NFT')
returns <- subset(returns, select = -c(Monthly))

returns_NFT <- returns
returns_NFT$NFT <- all_NFT$ALL_NFT

names(returns_NFT)
Crypto <- subset(returns_NFT, select = c(BTC, NFT, ETH))
rownames(Crypto) <- c("2020-11-01","2020-12-01",
                     "2021-01-01","2021-02-01","2021-03-01","2021-04-01", "2021-05-01","2021-06-01","2021-07-01","2021-08-01",
                     "2021-09-01","2021-10-01","2021-11-01","2021-12-01",
                     "2022-01-01","2022-02-01","2022-03-01")

dygraph(Crypto, main = "Криптовалюта и NFT") %>%
  dyAxis("y", label = "Накопленная доходность", valueRange = c(-1,15)) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"), strokeWidth = 2,  
            fillGraph = TRUE)

t.test(Crypto$NFT)

#S&P500
names(returns_NFT)
SP500 <- subset(returns_NFT, select = c(SP500, NFT))
rownames(SP500) <- c("2020-11-01","2020-12-01",
                      "2021-01-01","2021-02-01","2021-03-01","2021-04-01", "2021-05-01","2021-06-01","2021-07-01","2021-08-01",
                      "2021-09-01","2021-10-01","2021-11-01","2021-12-01",
                      "2022-01-01","2022-02-01","2022-03-01")

dygraph(SP500, main = "S&P500 и NFT") %>%
  dyAxis("y", label = "Накопленная доходность", valueRange = c(-1,15)) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"), strokeWidth = 2,  
            fillGraph = TRUE)

t.test(SP500$SP500)






####Добавим Art####
#Меняем, так как меняется первый год

all_NFT <- read_excel("Desktop/Diploma/Данные/Workings/All indiices.xlsx", sheet = 'NFT_ART')
all_Art <- read_excel("Desktop/Diploma/Данные/Workings/all_Art.xlsx", sheet = 'ART')

#ART+NFT
all_NFT <- read_excel("Desktop/Diploma/Данные/Workings/Data_for_NFT_graph.xlsx", sheet = 'NFT for ART')
all_Art <- read_excel("Desktop/Diploma/Данные/Workings/Data_for_NFT_graph.xlsx", sheet = 'ART')

returns_ALL <- all_NFT
returns_ALL <- subset(returns_ALL, select = -c(Monthly))
returns_ALL$ART <- all_Art$ALL_Art
names(returns_ALL)

ART_NFT <- subset(returns_ALL, select = c(ART, ALL_NFT))
rownames(ART_NFT) <- c("2020-11-01","2020-12-01",
                           "2021-01-01","2021-02-01","2021-03-01","2021-04-01", "2021-05-01","2021-06-01","2021-07-01","2021-08-01",
                           "2021-09-01")

dygraph(ART_NFT, main = "Art vs NFT") %>%
  dyAxis("y", label = "Return", valueRange = c(-1,9)) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"), strokeWidth = 2,  
            fillGraph = TRUE)

#Коинтеграция
#проверка на коинтеграцию с использоанием пакета
#тест Энгла-Грейнжера
library(aTSA)
names(ART_NFT)
View(ART_NFT)
ART_NFT1 <- ART_NFT[2:11,]
x <- ART_NFT1$ART
y <- ART_NFT1$ALL_NFT
coint.test(x, y, nlag = 3)

coint.test(y1, x1, nlag = 10)
View(y1)
View(y)
#нулевая гипотеза - ряды не коинтегрированы
#EG -≥ к нулю, если коинтегрированны



####Всё вместе####

names(returns_NFT)
Crypto <- subset(returns_NFT, select = c(BTC, NFT, ETH))

returns_ALL <- returns
returns_ALL$NFT <- all_NFT$ALL_NFT
returns_ALL$Art <- all_Art$ALL_Art
rownames(returns_ALL) <- c("2020-01-01","2020-02-01","2020-03-01","2020-04-01", "2020-05-01","2020-06-01","2020-07-01","2020-08-01",
                           "2020-09-01","2020-10-01","2020-11-01","2020-12-01",
                           "2021-01-01","2021-02-01","2021-03-01","2021-04-01", "2021-05-01","2021-06-01","2021-07-01","2021-08-01",
                           "2021-09-01")
# Produce interactive chart of stock returns
dygraph(returns_ALL, main = "Market") %>%
  dyAxis("y", label = "Return", valueRange = c(-2,5)) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1")) 



# Print last 5 rows of the data, rounded to 4 decimal places
round(tail(returns, n = 5), 4)

#Portfolio composition
corrplot::corrplot(cor(returns), method = 'circle')

####Art Index####

#Нарисуем изменения индексов

BOD1 <- Art[23:41,]
BOD1$Month <- factor(BOD1$Month)
v <- ggplot(BOD1, aes(x=Month, y=Art_Index, group=1)) + 
  geom_line(colour="dodgerblue2") + 
  geom_point(shape=21, size=3, fill="white") + 
  theme(axis.text = element_text(angle = 90, color="black", size=10, face=3)) +
  geom_smooth(method=lm, se=FALSE, col='darkred', size=0.5)
ggplotly(v)

BOD1 <- NFT[5:41,]
BOD1$Month <- factor(BOD1$Month)
v <- ggplot(BOD1, aes(x=Month, y=Art_Index, group=1)) + 
  geom_line(colour="dodgerblue2") + 
  geom_point(shape=21, size=3, fill="white") + 
  theme(axis.text = element_text(angle = 90, color="black", size=10, face=3)) +
  geom_smooth(method=lm, se=FALSE, col='darkred', size=0.5)
ggplotly(v)

# Produce interactive chart of stock returns
dygraph(all_Market, main = "Market") %>%
  dyAxis("y", label = "Return", valueRange = c(-8,8)) %>%
  dyRangeSelector(dateWindow = c("2020-31-01", "2021-09-01")) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set2"))


####Optimal portfolio_NFT and High Yield####
returns <- read_excel("Desktop/Diploma/Данные/Workings/returns2020.xlsx", 
                      sheet = "Portfolio_NFT")
all_NFT <- read_excel("Desktop/Diploma/Данные/Workings/All indiices.xlsx", sheet = 'Portfolio_NFT')

names(all_NFT)
returns <- subset(returns, select = -c(Quarter))
returns_ALL <- returns
returns_ALL$NFT <- all_NFT$ALL_NFT
names(returns_ALL)
NFT_HIGH_YIELD <- subset(returns_ALL, select = c(Private_eqt, BTC, ETH, VC, NFT))
names(NFT_HIGH_YIELD)
#1) NFT + BTC, ETH, VC, PE


#Optimal portfolio
mean_ret <- colMeans(NFT_HIGH_YIELD)
print(round(mean_ret, 5))
#Корр. матрица
corrplot::corrplot(cor(NFT_HIGH_YIELD), method = 'circle')
cov_mat <- cov(NFT_HIGH_YIELD) * 4
names(NFT_HIGH_YIELD)
#Веса
wts <- runif(n = 5)
print(wts)
print(sum(wts))
wts <- wts/sum(wts)
print(wts)
sum(wts)
port_returns <- (sum(wts * mean_ret) + 1)^4 - 1
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
                  ncol = 5)
# Creating an empty vector to store Portfolio returns
port_returns <- vector('numeric', length = num_port)
# Creating an empty vector to store Portfolio Standard deviation
port_risk <- vector('numeric', length = num_port)
# Creating an empty vector to store Portfolio Sharpe Ratio
sharpe_ratio <- vector('numeric', length = num_port)

# 2) Loop
for (i in seq_along(port_returns)) {
  wts <- runif(5)
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
View(all_wts)
all_wts <- tk_tbl(all_wts)
colnames(all_wts) <- colnames(NFT_HIGH_YIELD)
# Combing all the values together
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))
head(portfolio_values)
#lets look at the portfolios that matter the most
min_var <- portfolio_values[which.min(portfolio_values$Risk),]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]
View(max_sr)

#the portfoliowith the minimum variance
p <- min_var %>%
  gather(Private_eqt:NFT, key = Asset,
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
  gather(Private_eqt:NFT, key = Asset,
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

####Optimal portfolio_Art, NFT and all else####
returns <- read_excel("Desktop/Diploma/Данные/Workings/returns2020.xlsx", 
                      sheet = "Portfolio_ART")
all_NFT <- read_excel("Desktop/Diploma/Данные/Workings/All indiices.xlsx", sheet = 'Portfolio_NFT_ART')
all_ART <- read_excel("Desktop/Diploma/Данные/Workings/All_Art.xlsx", sheet = 'Portfolio_Art')

returns <- read_excel("Desktop/Diploma/Данные/Workings/returns2020_2.xlsx", 
                      sheet = "Portfolio_ART")
all_NFT <- read_excel("Desktop/Diploma/Данные/Workings/returns2020_2.xlsx", sheet = 'NFT')
all_ART <- read_excel("Desktop/Diploma/Данные/Workings/returns2020_2.xlsx", sheet = 'ART')

names(all_NFT)
returns <- subset(returns, select = -c(Monthly))
returns_ALL <- returns
returns_ALL$NFT <- all_NFT$All_NFT
returns_ALL$Art <- all_ART$All_ART
names(returns_ALL)
ALL <- subset(returns_ALL)
names(ALL)
#1) NFT + BTC, ETH, VC, PE


#Optimal portfolio
mean_ret <- colMeans(ALL)
print(round(mean_ret, 5))
#Корр. матрица
corrplot::corrplot(cor(ALL), method = 'circle')
cov_mat <- cov(ALL) * 12
names(ALL)
#Веса
wts <- runif(n = 17)
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
                  ncol = 17)
# Creating an empty vector to store Portfolio returns
port_returns <- vector('numeric', length = num_port)
# Creating an empty vector to store Portfolio Standard deviation
port_risk <- vector('numeric', length = num_port)
# Creating an empty vector to store Portfolio Sharpe Ratio
sharpe_ratio <- vector('numeric', length = num_port)

# 2) Loop
for (i in seq_along(port_returns)) {
  wts <- runif(17)
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

colnames(min_var) = c("SP500"     ,  "Treasury5" ,  "Treasury30" , "Corporate"  , "RealEstate" ,
                     "Oil"   ,      "Gold"    ,    "Silver"  ,    "Platinum"   , "EUR"    ,
                     "GBP"  ,   "PE" , "BTC"    ,     "ETH"   ,      "VC" ,        
                    "NFT"    ,     "Art"    ,     "Return"    ,  "Risk"    ,    "SharpeRatio")

#the portfoliowith the minimum variance
p <- min_var %>%
  gather(SP500:Art, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Активы', y = 'Доли', title = "Доли активов в портфеле с минимальной дисперсией") +
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
  labs(x = 'Активы', y = 'Доли', title = "Доли активов в портфеле с максимальным коэффициентом Шарпа") +
  scale_y_continuous(labels = scales::percent) 
ggplotly(p)

#all the random portfolios and visualize the efficient frontier.
p <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Годовой Риск',
       y = 'Годовая Доходность',
       title = "Эффективная граница портфелей") +
  geom_point(aes(x = Risk,
                 y = Return), data = min_var, color = 'red') +
  geom_point(aes(x = Risk,
                 y = Return), data = max_sr, color = 'red') +
  annotate('text', x = 0.55, y = 0.60, label = "Портфель с максимальным коэффициентом Шарпа") +
  annotate('text', x = 0.35, y = 0.01, label = "Портфель с минимальной дисперсией") +
  annotate(geom = 'segment', x = 0.50, xend = 0.55,  y = 0.49, 
           yend = 0.56, color = 'red', arrow = arrow(type = "open")) +
  annotate(geom = 'segment', x = 0.28, xend = 0.25,  y = 0.03, 
           yend = 0.09, color = 'red', arrow = arrow(type = "open"))

ggplotly(p)



#########

#Only market
Art_Marrket_NFT <- read_excel("Desktop/Diploma/Данные/Workings/Art_Marrket_NFT.xlsx", 
                              sheet = "Market")
returns <- Art_Marrket_NFT
returns <- subset(returns, select = -c(Monthly))

rownames(returns) <- c("2017-01-01","2017-02-01","2017-03-01","2017-04-01", "2017-05-01","2017-06-01","2017-07-01","2017-08-01",
                       "2017-09-01","2017-10-01","2017-11-01","2017-12-01",
                       "2018-01-01","2018-02-01","2018-03-01","2018-04-01", "2018-05-01","2018-06-01","2018-07-01","2018-08-01",
                       "2018-09-01","2018-10-01","2018-11-01","2018-12-01",
                       "2019-01-01","2019-02-01","2019-03-01","2019-04-01", "2019-05-01","2019-06-01","2019-07-01","2019-08-01",
                       "2019-09-01","2019-10-01","2019-11-01","2019-12-01",
                       "2020-01-01","2020-02-01","2020-03-01","2020-04-01", "2020-05-01","2020-06-01","2020-07-01","2020-08-01",
                       "2020-09-01","2020-10-01","2020-11-01","2020-12-01",
                       "2021-01-01","2021-02-01","2021-03-01","2021-04-01", "2021-05-01","2021-06-01","2021-07-01","2021-08-01",
                       "2021-09-01","2021-10-01","2021-11-01","2021-12-01",
                       "2022-01-01","2022-02-01","2022-03-01")

dygraph(returns, main = "Market") %>%
  dyAxis("y", label = "Return", valueRange = c(-2,1)) %>%
  dyRangeSelector(dateWindow = c("2017-01-01", "2022-03-01")) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set2")) 


#Market + Art
Art_Marrket_NFT <- read_excel("Desktop/Diploma/Данные/Workings/Art_Marrket_NFT.xlsx", 
                              sheet = "Art+Market")
returnsArt <- Art_Marrket_NFT
returnsArt <- subset(returnsArt, select = -c(Monthly))
View(returnsArt)

rownames(returnsArt) <- c("2020-01-01","2020-02-01","2020-03-01","2020-04-01", "2020-05-01","2020-06-01","2020-07-01","2020-08-01",
                       "2020-09-01","2020-10-01","2020-11-01","2020-12-01",
                       "2021-01-01","2021-02-01","2021-03-01","2021-04-01", "2021-05-01","2021-06-01","2021-07-01","2021-08-01",
                       "2021-09-01")

dygraph(returnsArt, main = "Market+Art") %>%
  dyAxis("y", label = "Return", valueRange = c(-2,2)) %>%
  dyRangeSelector(dateWindow = c("2020-01-01", "2021-09-01")) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set2")) 

#Market + NFT
Art_Marrket_NFT <- read_excel("Desktop/Diploma/Данные/Workings/Art_Marrket_NFT.xlsx", 
                              sheet = "NFT+Market")
returns <- Art_Marrket_NFT
returns <- subset(returns, select = -c(Monthly))

rownames(returns) <- c("2017-06-01","2017-07-01","2017-08-01",
                       "2017-09-01","2017-10-01","2017-11-01","2017-12-01",
                       "2018-01-01","2018-02-01","2018-03-01","2018-04-01", "2018-05-01","2018-06-01","2018-07-01","2018-08-01",
                       "2018-09-01","2018-10-01","2018-11-01","2018-12-01",
                       "2019-01-01","2019-02-01","2019-03-01","2019-04-01", "2019-05-01","2019-06-01","2019-07-01","2019-08-01",
                       "2019-09-01","2019-10-01","2019-11-01","2019-12-01",
                       "2020-01-01","2020-02-01","2020-03-01","2020-04-01", "2020-05-01","2020-06-01","2020-07-01","2020-08-01",
                       "2020-09-01","2020-10-01","2020-11-01","2020-12-01",
                       "2021-01-01","2021-02-01","2021-03-01","2021-04-01", "2021-05-01","2021-06-01","2021-07-01","2021-08-01",
                       "2021-09-01","2021-10-01","2021-11-01","2021-12-01",
                       "2022-01-01","2022-02-01","2022-03-01")

dygraph(returns, main = "Market+NFT") %>%
  dyAxis("y", label = "Return", valueRange = c(-2,6)) %>%
  dyRangeSelector(dateWindow = c("2017-01-01", "2022-03-01")) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set2")) 


#Market + NFT + Арт

Art_Marrket_NFT <- read_excel("Desktop/Diploma/Данные/Workings/Art_Marrket_NFT.xlsx", 
                              sheet = "Art+NFT+Market")
returns_ALL <- Art_Marrket_NFT
returns_ALL <- subset(returns_ALL, select = -c(Monthly))
View(returns_ALL)

rownames(returns_ALL) <- c("2020-01-01","2020-02-01","2020-03-01","2020-04-01", "2020-05-01","2020-06-01","2020-07-01","2020-08-01",
                          "2020-09-01","2020-10-01","2020-11-01","2020-12-01",
                          "2021-01-01","2021-02-01","2021-03-01","2021-04-01", "2021-05-01","2021-06-01","2021-07-01","2021-08-01",
                          "2021-09-01")

dygraph(returns_ALL, main = "Market+Art + NFT") %>%
  dyAxis("y", label = "Return", valueRange = c(-2,4)) %>%
  dyRangeSelector(dateWindow = c("2020-01-01", "2021-09-01")) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set2")) 



####Optimal portfolio####


#Optimal portfolio
mean_ret <- colMeans(returns_ALL)
print(round(mean_ret, 5))
#Корр. матрица
corrplot::corrplot(cor(returns_ALL), method = 'circle')
cov_mat <- cov(returns_ALL) * 4
names(returns_ALL)
#Веса
wts <- runif(n = 14)
print(wts)
print(sum(wts))
wts <- wts/sum(wts)
print(wts)
sum(wts)
port_returns <- (sum(wts * mean_ret) + 1)^4 - 1
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
                  ncol = 14)
# Creating an empty vector to store Portfolio returns
port_returns <- vector('numeric', length = num_port)
# Creating an empty vector to store Portfolio Standard deviation
port_risk <- vector('numeric', length = num_port)
# Creating an empty vector to store Portfolio Sharpe Ratio
sharpe_ratio <- vector('numeric', length = num_port)

# 2) Loop
for (i in seq_along(port_returns)) {
  wts <- runif(14)
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
View(all_wts)
all_wts <- tk_tbl(all_wts)
colnames(all_wts) <- colnames(returns_ALL)
# Combing all the values together
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))
head(portfolio_values)
#lets look at the portfolios that matter the most
min_var <- portfolio_values[which.min(portfolio_values$Risk),]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]
View(min_var)

#the portfoliowith the minimum variance
p <- min_var %>%
  gather(SP500:All_NFT, key = Asset,
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
  gather(SP500:All_NFT, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Tangency Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 
ggplotly(p)
