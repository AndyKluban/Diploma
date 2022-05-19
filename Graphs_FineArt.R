#Построение графиков
library(ggplot2)
library(plotly)
library(dplyr)
library(stargazer)

#Data statistics graphs
#1) Распределение транзакций по времени
#2) Распределение транзакций по стране
#3) Распределение транзакций по материалу
#4) Зависимость продаж от конкретного месяца продажи и от материала/века творчества


####Annual Returns####
#Annual Returns in Art
#Линейный график
#Stacked area graph <-  сделать позже

#Можно по полугодиям + продажи + закраска по веку творчества (данные почтии как в след.)

####Artists####
Data16 <- read_excel("Desktop/Diploma/Данные/Workings/Data16_Final.xlsx")
data <- Data16
View(Artists2)
str(data)
Artists <- subset(data, select=c(Artist, Price))
Artists <- na.omit(Artists)
artists <- Artists %>% group_by(Artist) %>%
  summarize_at(vars(Price), function(x) sum(x))
write.xlsx(artists, 'artists.xlsx')

Artists2 <- subset(data, select=c(Artist, Title))
Artists2$Title <- 1
Artists2 <- na.omit(Artists2)
artists2 <- Artists2 %>% group_by(Artist) %>%
  summarize_at(vars(Title), function(x) sum(x))
write.xlsx(artists, 'artists2.xlsx')

####Creaive Period####
#Распределение художников по веку творчества (Modern artists и тп <-  по Artprice.com)
#Можно с помощью кластеризации разделить художников по векам / по стилям


View(CreativePeriod)
str(data)
CreativePeriod <- subset(data, select=c(Born, Price))
CreativePeriod <- na.omit(CreativePeriod)
count(CreativePeriod)

CreativePeriod$Born <- as.numeric(CreativePeriod$Born)
CreativePeriod[,1] <- ifelse(CreativePeriod[,1] <= 1760, 'Старые мастера', 
                             ifelse(CreativePeriod[,1] > 1760 & CreativePeriod[,1] <= 1860, 'Искусство 19 века', 
                                    ifelse(CreativePeriod[,1] > 1860 & CreativePeriod[,1] <= 1920, 'Модернизм', 
                                           ifelse(CreativePeriod[,1] > 1920 & CreativePeriod[,1] <= 1945, 'Послевоенное искусство', 'Современное искусство'))))
#Число проданных билетов на каждый спектакль итого
creative <- CreativePeriod %>% group_by(Born) %>%
  summarize_at(vars(Price), function(x) sum(x))
sum(creative$Price)
#48 108 812 737 долларов за 11 лет
#Donut chart

#Для презентации убрали: textinfo='percent'
fig <- creative %>% plot_ly(labels = ~creative$Born, values = ~creative$Price,  textinfo='percent')
fig <- fig %>% add_pie(hole = 0.7)
fig <- fig %>% layout(title = "Распределение продаж по периоду творчества", showlegend = T,
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = F),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = F))
fig <- fig %>% layout(font=t)

fig

####Кластеризация####
#Кластеризация. Метод Главных компонент
#По вкладу компонент (характеристик) в анализ (влияниие на цену) = корреляции
#Можно сделать выборку 15 самых крупных артистов по выручке. 
#1) Отобрать высокие продажи > 100к
#2) Отобрать 15-20 самых крупных артистов по выручке
#3) Посмотреть корреляции и нарисовать PCA (2 графика)
#Как сделать PCA?

#Отберём данные
View(DC)
DC <- subset(DataIdeas1, select=-c(Dating))
DC <- na.omit(DC)
#1) Отобрать высокие продажи > 10к
DC <- filter(DC, DC$Price_USD_Adj>10000)
#2) Отобрать 106 самых крупных артистов
Hud <- table(DC$Artist)
Hud2 <- names(Hud)[Hud>200]
DC2 <- filter(DC, DC$Artist %in% Hud2)
names(DC2)
#Объединиим год и имя, чтобы легче работать
DC2$ArtistYear <- paste(DC2$Artist, DC2$Years, sep = '_')
#Таблица по turnover
DeathEffect <- DC2 %>% group_by(ArtistYear) %>%
  summarize_at(vars(Price_USD_Adj), function(x) sum(x))
sum(DeathEffect$Price_USD_Adj)
#Создадим Death Effect
DeathEffect <- DeathEffect %>% separate(ArtistYear, c('Artist', 'Years'),  sep = "_")
DeathEffect <- DeathEffect %>% separate(Years, c('Born', 'Die'),  sep = "-")
DeathEffect$Born <- as.numeric(DeathEffect$Born)
DeathEffect$Die <- as.numeric(DeathEffect$Die)
DeathEffect <- na.omit(DeathEffect)
DeathEffect$DeathEffect <- (DeathEffect$Die-DeathEffect$Born)
DeathEffect <- subset(DeathEffect, select=-c(Die, Born))
DeathEffect[,3] <- ifelse(DeathEffect[,3] <= 75, 'Early','Late')
DeathEffect$DeathEffect <- ifelse(DeathEffect[,3] == 'Early',1,0)
colnames(DeathEffect) <- c('Artist', 'Price', 'DeathEffect')

#PCA
df <- DeathEffect
library(dplyr)
to_clust <- df %>% select(Price, DeathEffect)
rownames(to_clust) <- df$Artist
m <- dist(scale(to_clust))
hc <- hclust(m, method = "ward.D2")
#Оптимальное число кластеров
library(factoextra)
fviz_nbclust(to_clust, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
#Оптимально брать 2 кластера. Но это не содержательно. 
#Тогда брать лучше или 3, или на крайний 4.
plot(hc, cex = 0.6)
rect.hclust(hc, k = 3)
groups4 <- cutree(hc, k = 3)
df$groups4 <- factor(groups4)

#Красивый График
library(ape)
colors = palette('Dark2')
clus4 = cutree(hc, 3)
plot(as.phylo(hc), type = "radial", label.offset = 0.1, cex = 0.4, 
     tip.color = colors[clus4])

#Содержательная интерпретация
to_clust %>% filter(groups4 == 1) 
to_clust %>% filter(groups4 == 2) 
to_clust %>% filter(groups4 == 3) 
#4 кластера. 1 - 36 c ранней смертью; 2 - 58 с обычной смертью;  3 - самые дорогие художники
#Посмотрим на все вместе по средним
table <- df %>% group_by(groups4) %>% 
  summarise_at(.vars = vars(Price, DeathEffect), .funs = funs(mean))
table$groups4 <- as.numeric(table$groups4)


View(table)




#Распределение транзакций по аукционным домам в зависимости от общих продаж <- Law of one price
####Корреляция####
rep_mod1$Signed <- class.ind(rep_mod1$Signed)
rep_mod1$Stamped <- class.ind(rep_mod1$Stamped)
rep_mod1$Inscribed <- class.ind(rep_mod1$Inscribed)
rep_mod1$Bill_Invoice <- class.ind(rep_mod1$Bill_Invoice)
rep_mod1$samehouse <- class.ind(rep_mod1$samehouse)
rep_mod1$int <- class.ind(rep_mod1$int)
names(D1_corr)

rep_mod1$Age_at_death <- (rep_mod1$Die - rep_mod1$Born)

Ta <- as.data.frame(table(rep_mod1$Medium))
ma <- filter(Ta, Ta$Freq>40)
rep_mod2 <- NULL
for(i in 1:length(ma$Var1)){
  rep_mod2<-rbind(rep_mod2, filter(rep_mod1, Medium==ma$Var1[i]))}
rep_mod2$Medium <- class.ind(rep_mod2$Medium)

la <- as.data.frame(table(rep_mod1$Auction_house))
ma <- filter(la, la$Freq>40)
rep_mod3 <- NULL
for(i in 1:length(ma$Var1)){rep_mod3<-rbind(rep_mod3, filter(rep_mod2, Auction_house==ma$Var1[i]))}

rep_mod3$Auction_house <- gsub("AB Stockholms Auktionsverk", "AB", rep_mod3$Auction_house)
rep_mod3$Auction_house <- gsub("Swann Galleries", "Sw", rep_mod3$Auction_house)
rep_mod3$Auction_house <- class.ind(rep_mod3$Auction_house)
Correlation <- rep_mod3
View(Correlation)
names(Correlation)
colnames(Correlation) = c('Artist', "Country_of_birth", 'Title', 'Medium','cm_wide', 'cm_long', 'Signed', 'Stamped', 
                          'Inscribed', 'Dating', 'Lot_Number', 'Transcation_Day',
                          'House', 'Bill', 'Currency', "Price", "Floor_Price", "Ceiling_Price", "Born", "Die",
                          "days", "samehouse", "int","scaledprice", "repfreq", "prevusd", "DeathEffect" )

Correlation1 <- subset(Correlation, select = -c(Artist, Country_of_birth, Title, Transcation_Day,
                                        Currency, Floor_Price, Ceiling_Price, samehouse,
                                        Die, cm_long, Stamped, Inscribed, repfreq, prevusd, Dating))
mcor <- cor(Correlation1)
corrplot(mcor, tl.col="black", tl.srt=45)


####NFT data graph####
# libraries
library(packcircles)
library(ggplot2)

names(DataNFT)

NFT_check <- subset(DataNFT, select=c(`Last sale`, `LAST SALE PRICE $`))
NFT_check <- na.omit(NFT_check)
colnames(NFT_check) = c('Date', 'Last_price')
View(NFT_check)

NFT_check1 <- NFT_check
NFT_check1 <- NFT_check1 %>% separate(Date, c('Date', 'Time'),  sep = ",")
NFT_check1 <- NFT_check1 %>% separate(Time, c('Hour', 'Del1', 'Del2'),  sep = ":")
NFT_check1 <- NFT_check1 %>% separate(Date, c('Del3', 'Month', 'Year'),  sep = "/")
NFT_check1 <- subset(NFT_check1, select = -c(Month, Hour))

NFT_check2017 <- subset(NFT_check1, NFT_check1$Year == '2017')
price2017 <- mean(NFT_check2017$Last_price)
NFT_check2018 <- subset(NFT_check1, NFT_check1$Year == '2018')
price2018 <- mean(NFT_check2018$Last_price)
NFT_check2019 <- subset(NFT_check1, NFT_check1$Year == '2019')
price2019 <- mean(NFT_check2019$Last_price)
NFT_check2020<- subset(NFT_check1, NFT_check1$Year == '2020')
price2020<- mean(NFT_check2020$Last_price)
NFT_check2021 <- subset(NFT_check1, NFT_check1$Year == '2021')
price2021 <- mean(NFT_check2021$Last_price)
NFT_check2022 <- subset(NFT_check1, NFT_check1$Year == '2022')
price2022 <- mean(NFT_check2022$Last_price)

mean_prices <- data.frame(price2017, price2018, price2019, price2020, price2021, price2022)
View(mean_prices)

nfts_check <- NFT_check1 %>% group_by(Year) %>%
  summarize_at(vars(Last_price), function(x) sum(x))
View(nfts_check)
change <- (nfts_check$Last_price[5]/nfts_check$Last_price[1])
change


sum(nfts_graph$`ACCUMULATED SALES $`)
colnames(nfts_graph) = c('Project', 'Accumulated_sales')



NFT_graph <- subset(DataNFT, select=c(PROJECT, `ACCUMULATED SALES $`))
NFT_graph <- na.omit(NFT_graph)
nfts_graph <- NFT_graph %>% group_by(PROJECT) %>%
  summarize_at(vars(`ACCUMULATED SALES $`), function(x) sum(x))
View(nfts_graph)
sum(nfts_graph$`ACCUMULATED SALES $`)
colnames(nfts_graph) = c('Project', 'Accumulated_sales')


# Create data
data <- data.frame(group=nfts_graph$Project, 
                   value=nfts_graph$Accumulated_sales)

# Generate the layout
packing <- circleProgressiveLayout(data$value, sizetype='area')
packing$radius <- 0.95*packing$radius
data <- cbind(data, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = data, aes(x, y, size=value, label = group), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_equal()

