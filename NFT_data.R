library(stargazer)
library(readxl)
library(MASS)

#NFT index
#Проблема <- многие считают просто по приросту средней цены (например, ежегодные отчёты). 
#Это не учитывает вообще никаких характеристик

DataNFT <- dir("~/Desktop/Diploma/Данные/NFT", full.names = T) %>% map_df(read_excel)

TurnoverNF <- subset(DataNFT, select=c(PROJECT, `ACCUMULATED SALES $`))
TurnoverNF <- na.omit(TurnoverNF)
turnovernf <- TurnoverNF %>% group_by(PROJECT) %>%
  summarize_at(vars(`ACCUMULATED SALES $`), function(x) sum(x))
sum(turnovernf$`ACCUMULATED SALES $`)
#1,735,548 наблюдений
#Оборот 9 032 402 562

#Поработаем с ценами
DataNFT$`FIRST SALE PRICE $` <- as.numeric(gsub(",",".", DataNFT$`FIRST SALE PRICE $`))
DataNFT$`FIRST SALE PRICE ETH` <- as.numeric(gsub(",",".", DataNFT$`FIRST SALE PRICE ETH`))
DataNFT$`LAST SALE PRICE $` <- as.numeric(gsub(",",".", DataNFT$`LAST SALE PRICE $`))
DataNFT$`LAST SALE PRICE ETH` <- as.numeric(gsub(",",".", DataNFT$`LAST SALE PRICE ETH`))
DataNFT$`ACCUMULATED SALES $` <- as.numeric(gsub(",",".", DataNFT$`ACCUMULATED SALES $`))
DataNFT$`UNIQUE OWNERS` <- as.numeric(DataNFT$`UNIQUE OWNERS`)
DataNFT$`SOLD COUNT` <- as.numeric(DataNFT$`SOLD COUNT`)

#Отберём отдельные альбомы
#Нудны только Collectibles, Art, Virtual Fashion, Music and Media, Gaming?
#1) Art <-  ArtBlocks, SuperRare, Doodle, CryptoKitties
#2) Collectibles <- BoredApe, CryptoPunks, Meebits, CyberKongz, CoolCats

View(nfts)
str(DataNFT)
NFTs <- subset(DataNFT, select=c(PROJECT, `LAST SALE PRICE $`))
NFTs <- na.omit(NFTs)
nfts <- NFTs %>% group_by(PROJECT) %>%
  summarize_at(vars(`LAST SALE PRICE $`), function(x) sum(x))
write.xlsx(nfts, 'nfts.xlsx')

NFTs2 <- subset(DataNFT, select=c(PROJECT, Name))
NFTs2$Name <- 1
NFTs2 <- na.omit(NFTs2)
nfts2 <- NFTs2 %>% group_by(PROJECT) %>%
  summarize_at(vars(Name), function(x) sum(x))
write.xlsx(nfts2, 'nfts2.xlsx')




####Kitties####
#First Blockchain Game Mania
#https://guide.cryptokitties.co/guide/tips/value-of-kitties
#Важно учесть:
#1) Gen0
#2) Fancy Cats - all of them.
#3) Low ID Number - Учитывается периодом продажи
#4) Exclusive Cats - нет данных (можно спарсить, но мало данных)
#5) Founder Cats - мало данных (всего 2)
#6) Misprints - учтём характеристики (Serpent eyes; onyx clor_main; pouty mouth; ganado color_pattern; frosdting color3; )

#Попробуем сделать датасет с одним альбомом, где мы отделим характеристики
#1) Найти альбом, где характеристики будут одинаковые
#2) Разделить и проверить их влияние 
Table <- as.data.frame(table(DataNFT$PROJECT))
Table <- table(DataNFT$PROJECT)
a <- names(Table)[Table>33000]
Kitty <- filter(DataNFT, PROJECT %in% a)
Table <- table(Kitty$PROJECT)
a <- names(Table)[Table<34000]
Kitty <- filter(Kitty, PROJECT %in% a)

############################################Attributes and Name
#write.xlsx(Kitty, 'Kitty.xlsx')
#13 столбцов надо
Kitty1 <- Kitty %>% separate(Attributes, c('Ch1', 'Ch2', 'Ch3', 'Ch4', 'Ch5', 'Ch6',
                                           'Ch7', 'Ch8', 'Ch9', 'Ch10', 'Ch11', 'Ch12'),  sep = "–")
Z <- table(Kitty1$Ch1)
z <- names(Z)[Z>4000]
Kitty1 <- filter(Kitty1, Ch1 %in% z)

#Переименуем столбцы и почистим всё=
Kitty2 <- Kitty1 %>% separate(Ch2, c('Del1', 'Gen'),  sep = "\\s+")
Kitty2 <- Kitty2 %>% separate(Ch3, c('Del2', 'Fancy'),  sep = "\\s+")
Kitty2 <- Kitty2 %>% separate(Ch5, c('Del3', 'Color'),  sep = "\\s+")
Kitty2 <- Kitty2 %>% separate(Ch6, c('Del4', 'Tertiary'),  sep = "\\s+")
Kitty2 <- Kitty2 %>% separate(Ch7, c('Del5', 'Pattern'),  sep = "\\s+")
Kitty2 <- Kitty2 %>% separate(Ch8, c('Del6', 'Exclusive'),  sep = "\\s+")
Kitty2 <- Kitty2 %>% separate(Ch10, c('Del7', 'Body'),  sep = "\\s+")
Kitty2 <- Kitty2 %>% separate(Ch11, c('Del8', 'Eyes'),  sep = "\\s+")
Kitty2 <- Kitty2 %>% separate(Ch12, c('Del9', 'Style'),  sep = "\\s+")
Kitty3 <- subset(Kitty2, select = -c(Ch1, Ch4, Ch9, Del1, Del2, Del3, Del4,
                                     Del5, Del6, Del7, Del8, Del9))
Kitty4 <- Kitty3 %>% separate(Name, c('Del1', 'Name'),  sep = "#")
Kitty4 <- subset(Kitty4, select = -c(Del1, `FIRST SALE PRICE ETH`, `LAST SALE PRICE ETH`))

############################################Monthly and Hour
Kitty5 <- Kitty4 %>% separate(`Last sale`, c('Date', 'Time'),  sep = ",")
Kitty5 <- Kitty5 %>% separate(Time, c('Hour', 'Del1', 'Del2'),  sep = ":")
Kitty5 <- Kitty5 %>% separate(Date, c('Del3', 'Month', 'Year'),  sep = "/")
Kitty5 <- subset(Kitty5, select = -c(Del1, Del2, Del3))

#Совместим период и год
Kitty5$Monthly <- paste(Kitty5$Year, Kitty5$Month)
#Удалим вспомогательные столбцы
Kitty5 <- subset(Kitty5, select = c(-Year, -Month))
#Проверим, что получилось
d <-  as.data.frame(table(Kitty5$Monthly))


############################################Переименуем
colnames(Kitty5) = c('Project', 'Name', 'First_price', 'Last_price', 'Owners', 'Accumulated_sales', 'Sold_count', 
                     'Color2', 'Gen', 'Mouth', 'Color_main', 'Color3', 'Color_pattern', 'Eye_Color', 'Body', 'Eyes',
                     'Transactions', 'Hour_UK', 'Monthly')

############################################Почистим Cattributes
Kitty6 <- data.frame(Kitty5)
names(Kitty6)
Tablea <- table(Kitty6$Gen)
names(Kitty6)
Kitty6[,9] <- ifelse(Kitty6[,9] == '1.0',  '1', 
                     ifelse(Kitty6[,9] == '2.0',  '2',
                            ifelse(Kitty6[,9] == '3.0',  '3',
                                   ifelse(Kitty6[,9] == '0.0',  '0', 'Other'))))

Tablea <- table(Kitty6$Mouth)
Kitty6[,10] <- ifelse(Kitty6[,10] == 'pouty',  'pouty', 
                     ifelse(Kitty6[,10] == 'happygokitty',  'happygokitty',
                            ifelse(Kitty6[,10] == 'soserious',  'soserious',
                                   ifelse(Kitty6[,10] == 'wuvme',  'wuvme',
                                          ifelse(Kitty6[,10] == 'grim',  'grim',
                                                 ifelse(Kitty6[,10] == '	confuzzled',  'confuzzled', 'Other'))))))
Tablea <-table(Kitty6$Color3)
Kitty6[,12] <- ifelse(Kitty6[,12] == 'frosting',  'frosting', 
                      ifelse(Kitty6[,12] == 'purplehaze',  'purplehaze',
                             ifelse(Kitty6[,12] == 'icy',  'icy',
                                    ifelse(Kitty6[,12] == 'cashewmilk',  'cashewmilk',
                                           ifelse(Kitty6[,12] == 'kittencream',  'kittencream',
                                                  ifelse(Kitty6[,12] == '	sandalwood',  'sandalwood', 'Other'))))))
Tablea <- table(Kitty6$Color_pattern)
Kitty6[,13] <- ifelse(Kitty6[,13] == 'totesbasic',  'totesbasic', 
                      ifelse(Kitty6[,13] == 'rascal',  'rascal',
                             ifelse(Kitty6[,13] == 'amur',  'amur',
                                    ifelse(Kitty6[,13] == 'tiger',  'tiger',
                                                  ifelse(Kitty6[,13] == 'ganado',  'ganado', 'Other'))))) 
Tablea <- table(Kitty6$Eye_Color)
Kitty6[,14] <- ifelse(Kitty6[,14] == 'cyan',  'cyan', 
                      ifelse(Kitty6[,14] == 'thundergrey',  'thundergrey',
                             ifelse(Kitty6[,14] == 'sapphire',  'sapphire',
                                    ifelse(Kitty6[,14] == 'dahlia',  'dahlia',
                                           ifelse(Kitty6[,14] == 'olive',  'olive	',
                                                  ifelse(Kitty6[,14] == '	mintgreen',  'mintgreen', 'Other'))))))
Tablea <- table(Kitty6$Body)
Kitty6[,15] <- ifelse(Kitty6[,15] == 'ragdoll',  'ragdoll', 
                      ifelse(Kitty6[,15] == 'selkirk',  'selkirk',
                             ifelse(Kitty6[,15] == 'koladiviya',  'koladiviya',
                                    ifelse(Kitty6[,15] == 'birman',  'birman',
                                           ifelse(Kitty6[,15] == 'munchkin',  'munchkin	',
                                                  ifelse(Kitty6[,15] == '	pixiebob',  'pixiebob', 'Other'))))))
Tablea <- table(Kitty6$Color2)
Kitty6[,8] <- ifelse(Kitty6[,8] == 'swampgreen',  'swampgreen', 
                      ifelse(Kitty6[,8] == 'royalpurple',  'royalpurple',
                             ifelse(Kitty6[,8] == 'coffee',  'coffee',
                                    ifelse(Kitty6[,8] == 'lemonade',  'lemonade',
                                           ifelse(Kitty6[,8] == 'egyptiankohl',  'egyptiankohl','Other')))))
Tablea <- table(Kitty6$Color_main)
Kitty6[,11] <- ifelse(Kitty6[,11] == 'greymatter',  'greymatter', 
                     ifelse(Kitty6[,11] == 'cottoncandy',  'cottoncandy',
                            ifelse(Kitty6[,11] == 'bananacream',  'bananacream',
                                   ifelse(Kitty6[,11] == 'cinderella',  'cinderella',
                                          ifelse(Kitty6[,11] == 'onyx',  'onyx','Other')))))
Tablea <- table(Kitty6$Eyes)
Kitty6[,16] <- ifelse(Kitty6[,16] == 'thicccbrowz',  'thicccbrowz', 
                      ifelse(Kitty6[,16] == 'wiley',  'wiley',
                             ifelse(Kitty6[,16] == 'slyboots',  'slyboots',
                                    ifelse(Kitty6[,16] == 'chronic',  'chronic',
                                           ifelse(Kitty6[,16] == 'serpent',  'serpent','Other')))))
############################################Exclusive
Kitty6$Founder <- Kitty6$Name 
Kitty6$Founder <- as.numeric(Kitty6$Founder)
Kitty6[,20] <- ifelse(Kitty6[,20] > 1 &  Kitty6[,20] <= 5000, '1', '0')
#1=low number, 0 - not low

############################################Зависимая переменная
#Новая переменная - средняя цена.
#Transactions включают всё 
Kitty7 <- Kitty6
a <- as.data.frame(table(Kitty7$Sold_count))
#Всего, что меньше 5- мало. Сократим.
Table <- table(Kitty7$Sold_count)
a <- names(Table)[Table>200]
Kitty7 <- filter(Kitty7, Sold_count %in% a)
#На 100 наблюдений меньше
Kitty7 <- Kitty7 %>% separate('Transactions', c('Tr1', 'Tr2', 'Tr3', 'Tr4', 'Tr5'),  sep = "\\s+")
Kitty7$Tr1 <- as.numeric(gsub(",",".", Kitty7$Tr1))
Kitty7$Tr2 <- as.numeric(gsub(",",".", Kitty7$Tr2))
Kitty7$Tr3 <- as.numeric(gsub(",",".", Kitty7$Tr3))
Kitty7$Tr4 <- as.numeric(gsub(",",".", Kitty7$Tr4))
Kitty7$Tr5 <- as.numeric(gsub(",",".", Kitty7$Tr5))
Kitty7[is.na(Kitty7)] = 0
Kitty7$Price_Av <- (Kitty7$Tr1 + Kitty7$Tr2 + Kitty7$Tr3 + Kitty7$Tr4 + Kitty7$Tr5)/Kitty7$Sold_count
Kitty8 <- subset(Kitty7, select = -c(Tr1, Tr2, Tr3, Tr4, Tr5))
summary(Kitty8)
View(Kitty8)

#Сменим первый месяц

table(Kitty7$Monthly)
Kitty8$Monthly <- factor(Kitty8$Monthly, levels = c('2021 09', '2017 11','2017 12', '2018 01', '2018 02', '2018 03',
       '2018 04', '2018 05', '2018 06', '2018 07', '2018 08', '2018 09', '2018 10', 
       '2018 11', '2018 12', '2019 01', '2019 02', '2019 03', '2019 04', '2019 05',
       '2019 06', '2019 07', '2019 08', '2019 09', '2019 10', '2019 11', '2019 12',
       '2020 01', '2020 02', '2020 03', '2020 04', '2020 05', '2020 06', '2020 07',
       '2020 08', '2020 09', '2020 10', '2020 11', '2020 12', '2021 01', '2021 02',
       '2021 03', '2021 04', '2021 05','2021 06', '2021 07', '2021 08',
       '2021 10', '2021 11', '2021 12', '2022 01', '2022 02', '2022 03'))

####NFT_Kitty_Index####

#Гипотеза: час продажи <- влияет. Инд. характеристики влияют.
Kitty_mod <- subset(Kitty8, select = -c(Project, Name))
Kitty_mod <- Kitty_mod[-c(10498, 16758), ]
mod_nft_h <- lm(Last_price ~ . ,data = Kitty_mod)

summary(mod_nft_h)
library(car)
v <- vif(mod_nft_h)
v
#Мультикол. высокая у  Accum.sales, удалим
mod_nft_h <- update(mod_nft_h, . ~ . - Accumulated_sales)

#Попробуем поудалять переменные, чтобы понять, от чего зависит такой большой R2
names(Kitty_mod)
mod_new <- update(mod_nft_h, . ~ . - First_price)
mod_new <- update(mod_nft_h, . ~ . - Monthly)
mod_new <- update(mod_nft_h, . ~ . - Gen)
mod_new <- update(mod_nft_h, . ~ . - Color2 - Gen -Mouth - Color_main - 
                    Color3 - Color_pattern - Eye_Color - Body - Eyes)
mod_new <- update(mod_nft_h, . ~ . - First_price - Price_Av)
mod_new <- update(mod_nft_h, . ~ . - First_price - Price_Av - Monthly)
mod_new <- update(mod_nft_h, . ~ . - First_price - Price_Av - Monthly - Owners 
                  - Sold_count - Hour_UK)
#Price_Av, Accumulated_sales, Owners, Sold_count, Founder, Hour_UK - нет. 
#Оч. много R2 от First price = 0.20
#Без Monthly R2 = 0.82 (-0.02)
#Без Gen вообще выше
#При удалении отдельного Attribute ничего не меняется
#При удалении всех Attributes ничего не меняется
#При удалении First_price и Price_Av, R2 снижается на 0.4
#При удалении First_price, Price_Av и Monthly, R2 снижается на 0.46
#При удалении First_price, Price_Av, Monthly и Owners, R2 снижается на 0.47
summary(mod_new)

#Остаёмся в модели с большим R2, зная из-за чего он образовался

plot(mod_nft_h, 1)
#Есть несколько выбросов, которые портят картинку. В целом, не так уж и плохо
plot(mod_nft_h, 2)
#остатки не нормальны (от -20 до 20)
plot(mod_nft_h, 3)
#Прям как будто нужны квадраты. Полная жесть.

#________________________________________
#Нормальность остатков (2)

boxCox(mod_nft_h)
#Нужен логарифм
mod_nft_h1 <- update(mod_nft_h, log(Last_price) ~ . )
summary(mod_nft_h1)
v <- vif(mod_nft_h1)
plot(mod_nft_h2, 2)
#Стало лучше. От -10 до 5
mod_nft_h1

#________________________________________
#Спецификация
plot(mod_nft_h1, which = 1) 
#Не так уж и плохо

library(lmtest)
resettest(mod_nft_h1)
#p-value оч маленький, значит чего-то не хватает
crPlots(mod_nft_h1)
#First price не оч
#Price_Av не оч

mod_nft_h21 <- update(mod_nft_h1, . ~ . + log(First_price) - First_price)
summary(mod_nft_h1)
summary(mod_nft_h21)
#R2 немного поднялся
mod_nft_h22<- update(mod_nft_h1, . ~ . + log(Price_Av) - Price_Av)
summary(mod_nft_h22)
#R2 сильно поднялся
mod_nft_h23<- update(mod_nft_h1, . ~ . + log(Price_Av) - Price_Av + log(First_price) - First_price)
summary(mod_nft_h23)
#R2 сильно поднялся (сильнее всего)
resettest(mod_nft_h1)
resettest(mod_nft_h22)
resettest(mod_nft_h22)
resettest(mod_nft_h23)
#p-value улучшается, но не принципиально

crPlots(mod_nft_h21)
#First_price стало сильно лучше
crPlots(mod_nft_h22)
#Price_Av стало сильно лучше
crPlots(mod_nft_h23)
#Price_Av и First_price стало сильно лучше
#Оставим mod_nft_h23
mod_nft_h2<- update(mod_nft_h1, . ~ . + log(Price_Av) - Price_Av + log(First_price) - First_price)
summary(mod_nft_h2)
plot(mod_nft_h1, which = 1)
#График хороший
mod_nft_h2

#________________________________________
#Гетероскедастичность - одинакова ли станд.ошибка
plot(mod_nft_h2, which = 3)
#Тест Бреуша-Пагана
bptest(mod_nft_h2)
#H0 - гетероскедастичности нет. Если мал. p-value, то гетероскедастичность есть.
#Гетеро есть (неоднородность остатков)

#Перейдём к робастным ошибкам в форме Вайта
library(sandwich)
cov_white <- vcovHC(mod_nft_h2, type = "HC0")
coeftest(mod_nft_h2, cov_white)
mod_nft_h3 <- mod_nft_h2
summary(mod_nft_h3, vcov = cov_white)
#Оставляем так.
mod_nft_h3

#________________________________________
#Ещё поудаляем переменные
library(MASS)
mod_nft_h4 <- stepAIC(mod_nft_h3)
summary(mod_nft_h4)
#Стало побольше значимого
mod_nft_h4

#________________________________________
#Итоговая модель
mod_NFT_Kitty <- mod_nft_h4
summary(mod_NFT_Kitty)

#Год для сравнения <-  2021 09 (так как это единственный общий месяц)
NFT_Kitty <- coef(mod_NFT_Kitty,  vcov = cov_white)
View(NFT_Kitty1)
NFT_Kitty1 <- NFT_Kitty[41:92]
write.xlsx(p, 'NFT_Kitty_Index1.xlsx')


####SuperRare####
#Картины в NFT

Table <- as.data.frame(table(DataNFT$PROJECT))
Table <- table(DataNFT$PROJECT)
a <- names(Table)[Table>22100]
Rare <- filter(DataNFT, PROJECT %in% a)
Table <- table(Rare$PROJECT)
a <- names(Table)[Table<22200]
Rare <- filter(Rare, PROJECT %in% a)
View(Rare)

#Artist (Transform to reputation?)
Rare$Artist <- Rare$Attributes
Rare1 <- Rare %>% separate(Artist, c('artist', 'Other'),  sep = "artist_normalized ")
Rare1 <- Rare1 %>% separate(Other, c('-', 'Artist'),  sep = '–')
Rare1$Artist<-gsub("[]]","",as.character(Rare1$Artist))
Rare1$Artist<-gsub("[[]","",as.character(Rare1$Artist))
Rare1 <- subset(Rare1, select=-c(artist, `-`))
#Reputation?
#Number of editions
Rare1$Edition <- Rare1$Attributes
Rare2 <- Rare1 %>% separate(Edition, c('Edition', 'Other'),  sep = "edition_count ")
Rare2 <- Rare2 %>% separate(Other, c('-', 'Edition'),  sep = '–')
Rare2$Edition<-gsub("unique_id ","",as.character(Rare2$Edition))
Rare2 <- subset(Rare2, select=-c(`-`))
#Year
Rare2$Dating <- Rare2$Attributes
Rare3 <- Rare2 %>% separate(Dating, c('Dating', 'Other'),  sep = "year ")
Rare3 <- Rare3 %>% separate(Other, c('-', 'Dating'),  sep = '–')
Rare3$Dating<-gsub("edition ","",as.character(Rare3$Dating))
Rare3 <- subset(Rare3, select=-c(`-`))
Rare3 <- na.omit(Rare3)
level <- table(Rare3$Dating)
a <- names(level)[level>200]
Rare4 <- filter(Rare3, Dating %in% a)
#Tags - хз как отделить

############################################Colnames
names(Rare4)
Rare5 <- subset(Rare4, select=-c(Attributes, `FIRST SALE PRICE ETH`, `LAST SALE PRICE ETH`))
colnames(Rare5) = c('Project', 'Name', 'First_price', 'Last_price', 'Owners', 'Accumulated_sales', 'Sold_count', 
                   'Transactions', 'Date', "Artist", "Edition","Dating")

############################################Quaters and Hour
Rare5 <- Rare5 %>% separate(Date, c('Date', 'Time'),  sep = ",")
Rare5 <- Rare5 %>% separate(Time, c('Hour', 'Del1', 'Del2'),  sep = ":")
Rare5 <- Rare5 %>% separate(Date, c('Del3', 'Month', 'Year'),  sep = "/")
Rare5 <- subset(Rare5, select = -c(Del1, Del2, Del3))

#Совместим период и год
Rare5$Monthly <- paste(Rare5$Year, Rare5$Month)
#Удалим вспомогательные столбцы
Rare5 <- subset(Rare5, select = c(-Year, -Month))
#Проверим, что получилось
d <-  as.data.frame(table(Rare5$Monthly))

############################################Character attributes
Table <- table(Rare5$Artist)
a <- names(Table)[Table>120]
View(a)
#Оставим крупные имена
Rare5[,10] <- ifelse(Rare5[,10] == a[1], 'missalsimpson',
                     ifelse(Rare5[,10] == a[2], 'yuramiron',
                            ifelse(Rare5[,10] == a[3], 'maxosiris',
                                   ifelse(Rare5[,10] == a[4], 'michaeljordan',
                                          ifelse(Rare5[,10] == '	 pbock', 'pbock',
                                                 ifelse(Rare5[,10] == '	 buzzlightning', 'buzzlightning', 'Other'))))))


############################################Отсчетный год
table(Rare5$Monthly)
Rare5$Monthly <- factor(Rare5$Monthly, levels = c('2021 09', '2018 04', '2018 05', '2018 06', '2018 07', '2018 08', '2018 09', '2018 10', 
                                                    '2018 11', '2018 12', '2019 01', '2019 02', '2019 03', '2019 04', '2019 05',
                                                    '2019 06', '2019 07', '2019 08', '2019 09', '2019 10', '2019 11', '2019 12',
                                                    '2020 01', '2020 02', '2020 03', '2020 04', '2020 05', '2020 06', '2020 07',
                                                    '2020 08', '2020 09', '2020 10', '2020 11', '2020 12', '2021 01', '2021 02',
                                                    '2021 03', '2021 04', '2021 05','2021 06', '2021 07', '2021 08',
                                                    '2021 10', '2021 11', '2021 12', '2022 01', '2022 02', '2022 03'))

############################################Уберём r/n
print(Rare5)
str(Rare5)
Rare5$Edition <- gsub('\\r\\n', '', Rare5$Edition)
Rare5$Dating <- gsub('\\r\\n', '', Rare5$Dating)
Rare5 <- na.omit(Rare5)

####Index Superare####

#Модель
#Гипотеза: час продажи <- влияет. Инд. характеристики влияют.

Rare_mod <- subset(Rare5, select = -c(Project, Name, Transactions))
Rare_mod <- subset(Rare_mod, Last_price!="0")
View(Rare_mod)
mod_nft_h <- lm(Last_price ~ . ,data = Rare_mod)
summary(mod_nft_h)

v <- vif(mod_nft_h)
v
#Мультикол. нет

plot(mod_nft_h, 1)
#выбросы
plot(mod_nft_h, 2)
#от 0 до +50
plot(mod_nft_h, 3)
#Жесть

#________________________________________
#Нормальность остатков
boxCox(mod_nft_h)
#Нужен логарифм
mod_nft_h1 <- update(mod_nft_h, log(Last_price) ~ . )
summary(mod_nft_h1)
v <- vif(mod_nft_h1)
plot(mod_nft_h1, 2)
#От -5 до 0. Идеально
mod_nft_h1
#________________________________________
mod_nft_h2 <- stepAIC(mod_nft_h1)
summary(mod_nft_h2)

#________________________________________
#Спецификация
plot(mod_nft_h2, which = 1) 
#Стало получше, но всё же
library(lmtest)
resettest(mod_nft_h2)
#p-value оч маленький, значит чего-то не хватает
crPlots(mod_nft_h2)
#First price не оч
#Accum sales не оч

mod_nft_h31 <- update(mod_nft_h2, . ~ . + log(First_price) - First_price)
#Не возводится в лог
summary(mod_nft_h2)
summary(mod_nft_h31)
#R2 немного поднялся
mod_nft_h32<- update(mod_nft_h2, . ~ . + log(Accumulated_sales) - Accumulated_sales)
summary(mod_nft_h32)
#R2 сильно поднялся, но пропала значимость

resettest(mod_nft_h2)
resettest(mod_nft_h32)
#p-value не улучшается, так что не надо
mod_nft_h2
#________________________________________
mod_nft_h3 <- stepAIC(mod_nft_h2)

#________________________________________
#Гетероскедастичность - одинакова ли станд.ошибка
plot(mod_nft_h3, which = 3)
#Тест Бреуша-Пагана
bptest(mod_nft_h3)
#H0 - гетероскедастичности нет при p-value большом выполняется
#Гетеро <- есть (неоднородность остатков)
#Перейдём к робастным ошибкам в форме Вайта
library(sandwich)
cov_white <- vcovHC(mod_nft_h3, type = "HC0")
coeftest(mod_nft_h3, cov_white)
mod_nft_h4 <- mod_nft_h3
summary(mod_nft_h4, vcov = cov_white)
#Оставляем так.
mod_nft_h4

#________________________________________

mod_NFT_Superare <- mod_nft_h4
summary(mod_NFT_Superare, vcov = cov_white)

#Год для сравнения <-  2017 12 (так как много наблюдений с этого момента)
NFT_Superare <- coef(mod_NFT_Superare, vcov = cov_white)
View(NFT_Superare)
NFT_Superare1 <- NFT_Superare[51:97]

library(stargazer)
stargazer(mod_NFT_Superare, type = 'text', df = FALSE, 
          digits =2, title = '.', column.labels = 'NFT_Superare',
          out = 'NFT_Superare.txt')

####BlockArt####
#Искусство
Table <- as.data.frame(table(DataNFT$PROJECT))
Table <- table(DataNFT$PROJECT)
a <- names(Table)[Table>9800]
Block <- filter(DataNFT, PROJECT %in% a)
Table <- table(Block$PROJECT)
a <- names(Table)[Table<9819]
Block <- filter(Block, PROJECT %in% a)
View(Block)
#style_edition, artist_normalized, style

#Artist (Transform to reputation?)
Block$Artist <- Block$Attributes
Block1 <- Block %>% separate(Artist, c('artist', 'Other'),  sep = "artist_normalized ")
Block1 <- Block1 %>% separate(Other, c('-', 'Artist'),  sep = '–')
Block1$Artist<-gsub("maurer_rose_eggs ","",as.character(Block1$Artist))
Block1$Artist<-gsub("[]]","",as.character(Block1$Artist))
Block1$Artist<-gsub("[[]","",as.character(Block1$Artist))
Block1 <- subset(Block1, select=-c(artist, `-`))
#Reputation?
#style_edition
Block1$Edition <- Block1$Attributes
Block2 <- Block1 %>% separate(Edition, c('edition', 'Other'),  sep = "style_edition ")
Block2 <- Block2 %>% separate(Other, c('-', 'Edition'),  sep = '–')
Block2$Edition<-gsub("edition ","",as.character(Block2$Edition))
Block2 <- subset(Block2, select=-c(edition, `-`))
#style
Block2$Style <- Block2$Attributes
Block3 <- Block2 %>% separate(Style, c('style', 'Other'),  sep = "style ")
Block3 <- Block3 %>% separate(Other, c('-', 'Style'),  sep = '–')
Block3$Style<-gsub("maurer_rose_n ","",as.character(Block3$Style))
Block3 <- subset(Block3, select=-c(style, `-`))
level <- table(Block3$Style)
a <- names(level)[level>100]
Block4 <- filter(Block3, Style %in% a)

############################################Colnames
names(Block4)
Block4 <- subset(Block4, select=-c(Name, Attributes, `FIRST SALE PRICE ETH`, `LAST SALE PRICE ETH`))
Block4 <- na.omit(Block4)
colnames(Block4) = c('Project', 'First_price', 'Last_price', 'Owners', 'Accumulated_sales', 'Sold_count', 
                    'Transactions', 'Date', "Artist", "Edition","Style")

############################################Quaters and Hour
Block4 <- Block4 %>% separate(Date, c('Date', 'Time'),  sep = ",")
Block4 <- Block4 %>% separate(Time, c('Hour', 'Del1', 'Del2'),  sep = ":")
Block4 <- Block4 %>% separate(Date, c('Del3', 'Month', 'Year'),  sep = "/")
Block4 <- subset(Block4, select = -c(Del1, Del2, Del3))

#Совместим период и год
Block4$Monthly <- paste(Block4$Year, Block4$Month)
#Удалим вспомогательные столбцы
Block4 <- subset(Block4, select = c(-Year, -Month))
#Проверим, что получилось
d <-  as.data.frame(table(Block4$Monthly))
d

############################################Отсчётный год
table(Block4$Monthly)
Block4$Monthly <- factor(Block4$Monthly, levels = c('2021 09','2021 01', '2021 02',
                                                    '2021 03', '2021 04', '2021 05','2021 06', '2021 07', '2021 08',
                                                    '2021 10', '2021 11', '2021 12', '2022 01', '2022 02'))

############################################//r//n omit
str(Block4)
Block4$Edition <- gsub("\\r\\n", "", Block4$Edition)
Block4$Style <- gsub("\\r\\n", "", Block4$Style)
Block4$Artist <- gsub("\\r\\n", "", Block4$Artist)
Block4$Transactions <- gsub("\\r\\n", "", Block4$Transactions)

############################################Уменьшим размерности переменных
View(Block4)

#Edition
Table <- table(Block4$Edition)
a <- names(Table)[Table>45]
View(a)
Block4[,10] <- ifelse(Block4[,10] == a[1], '	 0.0',
                     ifelse(Block4[,10] == a[2], '	 1.0',
                            ifelse(Block4[,10] == a[3], ' 107.0',
                                   ifelse(Block4[,10] == a[6], '	 87.0', 'Other'))))

#Style
Block5 <- Block4
Table <- table(Block5$Style)
a <- names(Table)[Table>500]
View(a)
Block5[,11] <- ifelse(Block5[,11] == a[1], 'Ether Marbles',
                      ifelse(Block5[,11] == a[2], 'Morphology	',
                             ifelse(Block5[,11] == a[3], 'Pattern 03',
                                    ifelse(Block5[,11] == a[4], 'Hapori',
                                           ifelse(Block5[,11] == a[5], 'Space', 'Other')))))

####Index BlockArt####

#Модель
#Гипотеза: час продажи <- влияет. Инд. характеристики влияют.
Block_mod <- subset(Block5, select = -c(Project, Transactions))
Block_mod <- subset(Block_mod, Last_price!="0")
mod_nft_h <- lm(Last_price ~ . ,data = Block_mod)
summary(mod_nft_h)

plot(mod_nft_h, 1)
#Линии не совпадают
plot(mod_nft_h, 2)
#остатки от - 10 до 10
plot(mod_nft_h, 3)
#жесть

#________________________________________
#Нормальность остатков
boxCox(mod_nft_h)
#Нужен логарифм
mod_nft_h1 <- update(mod_nft_h, log(Last_price) ~ . )
summary(mod_nft_h1)
plot(mod_nft_h1, 2)
#стало лучше от -5 до 5
mod_nft_h1

#________________________________________
mod_nft_h2 <- stepAIC(mod_nft_h1)
summary(mod_nft_h2)

v <- vif(mod_nft_h1)
v
mod_nft_h3 <- update(mod_nft_h2, . ~ . - Style)
summary(mod_nft_h3)
#Мультикол. высокая у Style. Удалим
#Но этому явно есть лог. объяснение

#________________________________________
#Спецификация
plot(mod_nft_h3, which = 1) 
resettest(mod_nft_h3)
#p value мал <-  плохо
crPlots(mod_nft_h3)
mod_nft_h4 <- update(mod_nft_h3, . ~ . + log(First_price) - First_price)
summary(mod_nft_h4)
mod_nft_h5 <- update(mod_nft_h4, . ~ . + log(Accumulated_sales) - Accumulated_sales)
summary(mod_nft_h5)
vif(mod_nft_h5)
#Artist оч высокая мультикол. Удалим
mod_nft_h6 <- update(mod_nft_h5, . ~ . - Artist)
summary(mod_nft_h6)
mod_nft_h6

#________________________________________
#Гетероскедастичность - одинакова ли станд.ошибка
#plot(mod_nft_h2, which = 3)
#Тест Бреуша-Пагана
bptest(mod_nft_h6)
#H0 - гетероскедастичности нет при p-value большом выполняется
#Гетеро <- есть (неоднородность остатков)
#Перейдём к робастным ошибкам в форме Вайта
library(sandwich)
cov_white <- vcovHC(mod_nft_h6, type = "HC0")
coeftest(mod_nft_h6, cov_white)
summary(mod_nft_h6, vcov = cov_white)

#________________________________________

#Итоговая модель
mod_NFT_Block <- mod_nft_h6
summary(mod_NFT_Block, vcov = cov_white)
NFT_Block <- coef(mod_NFT_Block,  vcov = cov_white)
View(NFT_Block)


####Bored Ape####
#Имеем почти полный датасет. Так как всего было выпущено 10000 Bored Apes.

Table <- as.data.frame(table(DataNFT$PROJECT))
Table <- table(DataNFT$PROJECT)
a <- names(Table)[Table>29000]
Ape <- filter(DataNFT, PROJECT %in% a)
Table <- table(Ape$PROJECT)
a <- names(Table)[Table<30000]
View(Table)
Ape <- filter(Ape, PROJECT %in% a)
View(Ape)

#Оcтавим только Bored Ape 
Ape1 <- Ape %>% separate(Name, c('Album', 'ID'),  sep = "#")
Table <- as.data.frame(table(Ape1$Album))
Ape1 <- filter(Ape1, Album == 'Bored Ape ')
Ape1 <- subset(Ape1, select = -c(`FIRST SALE PRICE ETH`, `LAST SALE PRICE ETH`))

#Earring yes/no
Ape1$Ear <- Ape1$Attributes
Ape2 <- Ape1 %>% separate(Ear, c('earring', 'Other'),  sep = "earring ")
Ape2$Earring <- Ape2$earring
Ape2[,14] <- ifelse(Ape2[,14] == "",  1, 0)
Ape2 <- subset(Ape2, select=-c(earring, Other))

#Fur and Mouth
Ape2$Fur <- Ape2$Attributes
Ape3 <- Ape2 %>% separate(Fur, c('fur', 'Other'),  sep = "fur ")
Ape3 <- Ape3 %>% separate(Other, c('-', 'Fur', 'Mouth'),  sep = '–')
Ape3 <- subset(Ape3, select=-c(fur, `-`))
#Дообработаем позже

#Background
Ape3$Background <- Ape3$Attributes
Ape4 <- Ape3 %>% separate(Background, c('background', 'Other'),  sep = "background ")
Ape4 <- Ape4 %>% separate(Other, c('-', 'Background', 'Hat'),  sep = '–')

#Hat yes/no
Ape5 <- Ape4 %>% separate(Hat, c('hat', 'Other'),  sep = "eyes")
Ape5$Hat <- Ape5$Other
Ape5[,20] <- ifelse(Ape5[,20] == ' ', 1, 0)
Ape5$Hat[is.na(Ape5$Hat)] = 0
Ape5 <- subset(Ape5, select=-c(background, `-`, hat, Other))

#Eyes
Ape5$Eyes <- Ape5$Attributes
Ape6 <- Ape5 %>% separate(Eyes, c('eyes', 'Other'),  sep = "eyes ")
Ape6 <- Ape6 %>% separate(Other, c('-', 'Eyes', 'Clothes'),  sep = '–')
Ape6$Clothes[is.na(Ape6$Clothes)] = 'No'
Ape6 <- subset(Ape6, select=-c(eyes, `-`))

#Чистка
Ape6$Fur<-gsub("mouth","",as.character(Ape6$Fur))
Ape6$Mouth <- gsub("background","",as.character(Ape6$Mouth))
Ape6$Background <- gsub("hat","",as.character(Ape6$Background))
Ape6$Background <- gsub("eyes","",as.character(Ape6$Background))
Ape6$Eyes <- gsub("clothes","",as.character(Ape6$Eyes))

Ape6 <- data.frame(Ape6)
#Проверка
level <- table(Ape6$Mouth)
a <- names(level)
Ape6[,14] <- ifelse(Ape6[,14] == a[26], 'Phoneme', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[27], 'Phoneme', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[28], 'Phoneme', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[29], 'Phoneme', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[30], 'Phoneme', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[21], 'Grin', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[22], 'Grin', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[23], 'Grin', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[24], 'Grin', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[32], 'Grin', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[1], 'Bored', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[2], 'Bored', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[3], 'Bored', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[4], 'Bored', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[5], 'Bored', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[6], 'Bored', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[7], 'Bored', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[8], 'Bored', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[9], 'Bored', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[10], 'Bored Unshaven', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[11], 'Bored Unshaven', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[12], 'Bored Unshaven', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[13], 'Bored Unshaven', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[14], 'Bored Unshaven', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[15], 'Bored Unshaven', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[16], 'Bored Unshaven', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[17], 'Bored Unshaven', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[18], 'Bored Unshaven', Ape6[,14])
level <- table(Ape6$Mouth)
a <- names(level)
Ape6[,14] <- ifelse(Ape6[,14] == a[1], 'Other', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[2], 'Other', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[3], 'Other', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[4], 'Other', Ape6[,14])
Ape6[,14] <- ifelse(Ape6[,14] == a[5], 'Other', Ape6[,14])
level <- table(Ape6$Eyes)
a <- names(level)
Ape6[,17] <- ifelse(Ape6[,17] == a[1], '3d', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[2], '3d', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[3], 'Angry', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[4], 'Angry', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[5], 'Blindfold', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[6], 'Blindfold', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[7], 'Bloodshot', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[8], 'Bloodshot', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[9], 'Blue Beams', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[10], 'Blue Beams', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[11], 'Bored', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[12], 'Bored', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[13], 'Closed', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[14], 'Closed', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[15], 'Coins', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[16], 'Coins', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[17], 'Crazy', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[18], 'Crazy', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[19], 'Cyborg', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[20], 'Cyborg', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[21], 'Eyepatch', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[22], 'Eyepatch', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[23], 'Heart', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[24], 'Heart', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[25], 'Holographic', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[26], 'Holographic', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[27], 'Hypnotized', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[28], 'Hypnotized', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[29], 'Laser Eyes', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[30], 'Laser Eyes', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[31], 'Robot', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[32], 'Robot', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[33], 'Sad', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[34], 'Sad', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[35], 'Scumbag', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[36], 'Scumbag', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[37], 'Sleepy', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[38], 'Sleepy', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[39], 'Sunglasses', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[40], 'Sunglasses', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[41], 'Wide Eyed', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[42], 'Wide Eyed', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[43], 'X Eyes', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[44], 'X Eyes', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[45], 'Zombie', Ape6[,17])
Ape6[,17] <- ifelse(Ape6[,17] == a[46], 'Zombie', Ape6[,17])

############################################Colnames
names(Ape6)
Ape6 <- subset(Ape6, select=-c(Attributes))
colnames(Ape6) = c('Project', 'Album', 'Id', 'First_price', 'Last_price', 'Owners', 'Accumulated_sales', 'Sold_count', 
                   'Transactions', 'Date', "Earring", "Fur","Mouth","Background", "Hat","Eyes" ,"Clothes" )

############################################Quaters and Hour
Ape7 <- Ape6
Ape7 <- Ape7 %>% separate(Date, c('Date', 'Time'),  sep = ",")
Ape7 <- Ape7 %>% separate(Time, c('Hour', 'Del1', 'Del2'),  sep = ":")
Ape7 <- Ape7 %>% separate(Date, c('Del3', 'Month', 'Year'),  sep = "/")
Ape7 <- subset(Ape7, select = -c(Del1, Del2, Del3))

#Совместим период и год
Ape7$Monthly <- paste(Ape7$Year, Ape7$Month)
#Удалим вспомогательные столбцы
Ape7 <- subset(Ape7, select = c(-Year, -Month))
#Проверим, что получилось
d <-  as.data.frame(table(Ape7$Monthly))
d

View(Ape7)
############################################Отсчётный год

table(Ape7$Monthly)
Ape7$Monthly <- factor(Ape7$Monthly, levels = c('2021 09', '2021 04', '2021 05','2021 06',
                                                    '2022 02', '2022 03'))

############################################Delete \\r\\n
Ape7$Fur <- gsub("\\r\\n", "", Ape7$Fur)
Ape7$Clothes <- gsub("\\r\\n", "", Ape7$Clothes)

View(Ape7)
####Bored Index####

#Модель
#Гипотеза: час продажи <- влияет. Инд. характеристики влияют.
Bored_mod <- subset(Ape7, select = -c(Project, Album, Id, Transactions))
Bored_mod <- subset(Bored_mod, Last_price!="0")
Bored_mod <- subset(Bored_mod, First_price!="0")

mod_nft_h <- lm(Last_price ~ . ,data = Bored_mod)
summary(mod_nft_h)

#Мультикол. высокая у Price_Av и Accum.sales. 
#Но этому явно есть лог. объяснение
vif(mod_nft_h)
plot(mod_nft_h, 1)
#Линии не совпадают
plot(mod_nft_h, 2)
#от -4 до +10
plot(mod_nft_h, 3)

#________________________________________
#Нормальность остатков
boxCox(mod_nft_h)
#Нужен логарифм
mod_nft_h1 <- update(mod_nft_h, log(Last_price) ~ . )
summary(mod_nft_h1)
plot(mod_nft_h1, 2)
#В целом, от -5 до 0
mod_nft_h1

#________________________________________
#Спецификация
plot(mod_nft_h1, which = 1) 
resettest(mod_nft_h1)
crPlots(mod_nft_h1)
#First_price
mod_nft_h2 <- update(mod_nft_h1, . ~ . + log(First_price) - First_price)
summary(mod_nft_h2)
mod_nft_h2

#________________________________________
mod_nft_h3 <- stepAIC(mod_nft_h2)
summary(mod_nft_h3)

#________________________________________
#Гетероскедастичность - одинакова ли станд.ошибка
#plot(mod_nft_h2, which = 3)
#Тест Бреуша-Пагана
bptest(mod_nft_h3)
#H0 - гетероскедастичности нет при p-value большом выполняется
#Гетеро <- есть (неоднородность остатков)
#Перейдём к робастным ошибкам в форме Вайта
library(sandwich)
cov_white <- vcovHC(mod_nft_h3, type = "HC0")
coeftest(mod_nft_h3, cov_white)
#Стало много незначимого

#________________________________________

mod_NFT_Bored <- mod_nft_h3
summary(mod_NFT_Bored,  vcov = cov_white)

NFT_Bored <- coef(mod_NFT_Bored,  vcov = cov_white)
View(NFT_Bored)

library(stargazer)
stargazer(mod_NFT_Bored, type = 'text', df = FALSE, 
          digits =2, title = '.', column.labels = 'NFT_Superare',
          out = 'NFT_Bored.txt')

####CryptoPunks####
#Total - 10 000
Table <- as.data.frame(table(DataNFT$PROJECT))
Table <- table(DataNFT$PROJECT)
a <- names(Table)[Table>7400]
Punk <- filter(DataNFT, PROJECT %in% a)
Table <- table(Punk$PROJECT)
a <- names(Table)[Table<7500]
Punk <- filter(Punk, PROJECT %in% a)
View(Punk)

#Можно через 1/0
#Через if (в одной из колонок есть, то 1), а потом сложить!
#8 столбцов надо
#Male
Punk$Characteristics <- Punk$Attributes 
Punk1 <- Punk %>% separate(Characteristics, c('Del1', 'Else'),  sep = "–")
Punk1$Else<-gsub("[]]","",as.character(Punk1$Else))
Punk1$Else<-gsub("[[]","",as.character(Punk1$Else))
Punk1 <- na.omit(Punk1)
Punk2 <- Punk1 %>% separate(Else, c('At1', 'At2', 'At3', 'At4', 'At5', 'At6', 'At7', 'At8', 'At9', 'At10'),  sep = ", ")
names(Punk2)
Z <- table(Punk2$At7)
z <- names(Z)
Punk2[,14] <- ifelse(Punk2[,14] == z[9], 1, 0)
Punk2[,15] <- ifelse(Punk2[,15] == z[9], 1, 0)
Punk2[,16] <- ifelse(Punk2[,16] == z[9], 1, 0)
Punk2[,17] <- ifelse(Punk2[,17] == z[9], 1, 0)
Punk2[,18] <- ifelse(Punk2[,18] == z[9], 1, 0)
Punk2[,19] <- ifelse(Punk2[,19] == z[9], 1, 0)
Punk2[,20] <- ifelse(Punk2[,20] == z[9], 1, 0)
Punk2[,21] <- ifelse(Punk2[,21] == z[9], 1, 0)
Punk2[,22] <- ifelse(Punk2[,22] == z[9], 1, 0)
Punk2[,23] <- ifelse(Punk2[,23] == z[9], 1, 0)
Punk2[is.na(Punk2)] = 0
Punk2$Sex <- (Punk2$At1 + Punk2$At2 + Punk2$At3 + Punk2$At4 + Punk2$At5 + Punk2$At6 + Punk2$At7 + Punk2$At8 +
                Punk2$At9 + Punk2$At10)
Punk2 <- subset(Punk2, select = -c(At1,At2,At3,At4,At5,At6,At7,At8,At9,At10))
Punk2 <- subset(Punk2, select = -c(Del1))

#Cap
Punk2$Characteristics <- Punk2$Attributes 
Punk3 <- Punk2 %>% separate(Characteristics, c('Del1', 'Else'),  sep = "–")
Punk3$Else<-gsub("[]]","",as.character(Punk3$Else))
Punk3$Else<-gsub("[[]","",as.character(Punk3$Else))
Punk3 <- na.omit(Punk3)
Punk4 <- Punk3 %>% separate(Else, c('At1', 'At2', 'At3', 'At4', 'At5', 'At6', 'At7', 'At8', 'At9', 'At10'),  sep = ", ")
names(Punk4)
Z <- table(Punk4$At2)
z <- names(Z)
names(Punk4)
Punk4[,15] <- ifelse(Punk4[,15] == z[11] | Punk4[,15] == z[12] | Punk4[,15] == z[13] |
                       Punk4[,15] == z[48] | Punk4[,15] == z[67], 1, 0)
Punk4[,16] <- ifelse(Punk4[,16] == z[11] | Punk4[,16] == z[12] | Punk4[,16] == z[13] |
                       Punk4[,16] == z[48] | Punk4[,16] == z[67], 1, 0)
Punk4[,17] <- ifelse(Punk4[,17] == z[11] | Punk4[,17] == z[12] | Punk4[,17] == z[13] |
                       Punk4[,17] == z[48] | Punk4[,17] == z[67], 1, 0)
Punk4[,18] <- ifelse(Punk4[,18] == z[11] | Punk4[,18] == z[12] | Punk4[,18] == z[13] |
                       Punk4[,18] == z[48] | Punk4[,18] == z[67], 1, 0)
Punk4[,19] <- ifelse(Punk4[,19] == z[11] | Punk4[,19] == z[12] | Punk4[,19] == z[13] |
                       Punk4[,19] == z[48] | Punk4[,19] == z[67], 1, 0)
Punk4[,20] <- ifelse(Punk4[,20] == z[11] | Punk4[,20] == z[12] | Punk4[,20] == z[13] |
                       Punk4[,20] == z[48] | Punk4[,20] == z[67], 1, 0)
Punk4[,21] <- ifelse(Punk4[,21] == z[11] | Punk4[,21] == z[12] | Punk4[,21] == z[13] |
                       Punk4[,21] == z[48] | Punk4[,21] == z[67], 1, 0)
Punk4[,22] <- ifelse(Punk4[,22] == z[11] | Punk4[,22] == z[12] | Punk4[,22] == z[13] |
                       Punk4[,22] == z[48] | Punk4[,22] == z[67], 1, 0)
Punk4[,23] <- ifelse(Punk4[,23] == z[11] | Punk4[,23] == z[12] | Punk4[,23] == z[13] |
                       Punk4[,23] == z[48] | Punk4[,23] == z[67], 1, 0)
Punk4[,24] <- ifelse(Punk4[,24] == z[11] | Punk4[,24] == z[12] | Punk4[,24] == z[13] |
                       Punk4[,24] == z[48] | Punk4[,24] == z[67], 1, 0)
Punk4[is.na(Punk4)] = 0
Punk4$Cap <- (Punk4$At1 + Punk4$At2 + Punk4$At3 + Punk4$At4 + Punk4$At5 + Punk4$At6 + Punk4$At7 + Punk4$At8 +
                Punk4$At9 + Punk4$At10)
Punk4 <- subset(Punk4, select = -c(At1,At2,At3,At4,At5,At6,At7,At8,At9,At10))
Punk4 <- subset(Punk4, select = -c(Del1))
Punk4[,14] <- ifelse(Punk4[,14] == 2 | Punk4[,14] == 1, 1, 0)

#Earring
Punk4$Characteristics <- Punk4$Attributes 
Punk5 <- Punk4 %>% separate(Characteristics, c('Del1', 'Else'),  sep = "–")
Punk5$Else<-gsub("[]]","",as.character(Punk5$Else))
Punk5$Else<-gsub("[[]","",as.character(Punk5$Else))
Punk5 <- na.omit(Punk5)
Punk6 <- Punk5 %>% separate(Else, c('At1', 'At2', 'At3', 'At4', 'At5', 'At6', 'At7', 'At8', 'At9', 'At10'),  sep = ", ")
names(Punk6)
Z <- table(Punk6$At3)
z <- names(Z)
names(Punk6)
Punk6[,16] <- ifelse(Punk6[,16] == z[25] | Punk6[,16] == z[26], 1, 0)
Punk6[,17] <- ifelse(Punk6[,17] == z[25] | Punk6[,17] == z[26], 1, 0)
Punk6[,18] <- ifelse(Punk6[,18] == z[25] | Punk6[,18] == z[26], 1, 0)
Punk6[,19] <- ifelse(Punk6[,19] == z[25] | Punk6[,19] == z[26], 1, 0)
Punk6[,20] <- ifelse(Punk6[,20] == z[25] | Punk6[,20] == z[26], 1, 0)
Punk6[,21] <- ifelse(Punk6[,21] == z[25] | Punk6[,21] == z[26], 1, 0)
Punk6[,22] <- ifelse(Punk6[,22] == z[25] | Punk6[,22] == z[26], 1, 0)
Punk6[,23] <- ifelse(Punk6[,23] == z[25] | Punk6[,23] == z[26], 1, 0)
Punk6[,24] <- ifelse(Punk6[,24] == z[25] | Punk6[,24] == z[26], 1, 0)
Punk6[,25] <- ifelse(Punk6[,25] == z[25] | Punk6[,25] == z[26], 1, 0)
Punk6[is.na(Punk6)] = 0
Punk6$Earring <- (Punk6$At1 + Punk6$At2 + Punk6$At3 + Punk6$At4 + Punk6$At5 + Punk6$At6 + Punk6$At7 + Punk6$At8 +
                Punk6$At9 + Punk6$At10)
Punk6 <- subset(Punk6, select = -c(At1,At2,At3,At4,At5,At6,At7,At8,At9,At10))
Punk6 <- subset(Punk6, select = -c(Del1))
Punk6[,15] <- ifelse(Punk6[,15] == 2 | Punk6[,15] == 1, 1, 0)

#Cigarette
Punk6$Characteristics <- Punk6$Attributes 
Punk7 <- Punk6 %>% separate(Characteristics, c('Del1', 'Else'),  sep = "–")
Punk7$Else<-gsub("[]]","",as.character(Punk7$Else))
Punk7$Else<-gsub("[[]","",as.character(Punk7$Else))
Punk7 <- na.omit(Punk7)
Punk8 <- Punk7 %>% separate(Else, c('At1', 'At2', 'At3', 'At4', 'At5', 'At6', 'At7', 'At8', 'At9', 'At10'),  sep = ", ")
names(Punk8)
Z <- table(Punk8$At4)
z <- names(Z)
names(Punk8)
Punk8[,17] <- ifelse(Punk8[,17] == z[10] | Punk8[,17] == z[11], 1, 0)
Punk8[,18] <- ifelse(Punk8[,18] == z[10] | Punk8[,18] == z[11], 1, 0)
Punk8[,19] <- ifelse(Punk8[,19] == z[10] | Punk8[,19] == z[11], 1, 0)
Punk8[,20] <- ifelse(Punk8[,20] == z[10] | Punk8[,20] == z[11], 1, 0)
Punk8[,21] <- ifelse(Punk8[,21] == z[10] | Punk8[,21] == z[11], 1, 0)
Punk8[,22] <- ifelse(Punk8[,22] == z[10] | Punk8[,22] == z[11], 1, 0)
Punk8[,23] <- ifelse(Punk8[,23] == z[10] | Punk8[,23] == z[11], 1, 0)
Punk8[,24] <- ifelse(Punk8[,24] == z[10] | Punk8[,24] == z[11], 1, 0)
Punk8[,25] <- ifelse(Punk8[,25] == z[10] | Punk8[,25] == z[11], 1, 0)
Punk8[,26] <- ifelse(Punk8[,26] == z[10] | Punk8[,26] == z[11], 1, 0)
Punk8[is.na(Punk8)] = 0
Punk8$Cigarette <- (Punk8$At1 + Punk8$At2 + Punk8$At3 + Punk8$At4 + Punk8$At5 + Punk8$At6 + Punk8$At7 + Punk8$At8 +
                    Punk8$At9 + Punk8$At10)
Punk8 <- subset(Punk8, select = -c(At1,At2,At3,At4,At5,At6,At7,At8,At9,At10))
Punk8 <- subset(Punk8, select = -c(Del1))
Punk8[,15] <- ifelse(Punk8[,15] == 2 | Punk8[,15] == 1, 1, 0)

#Mohawk
Punk8$Characteristics <- Punk8$Attributes 
Punk8 <- Punk8 %>% separate(Characteristics, c('Del1', 'Else'),  sep = "–")
Punk8$Else<-gsub("[]]","",as.character(Punk8$Else))
Punk8$Else<-gsub("[[]","",as.character(Punk8$Else))
Punk8 <- na.omit(Punk8)
Punk8 <- Punk8 %>% separate(Else, c('At1', 'At2', 'At3', 'At4', 'At5', 'At6', 'At7', 'At8', 'At9', 'At10'),  sep = ", ")
names(Punk8)
Z <- table(Punk8$At2)
z <- names(Z)
names(Punk8)
Punk8[,18] <- ifelse(Punk8[,18] == z[53] | Punk8[,18] == z[54]| Punk8[,18] == z[55] | Punk8[,18] == z[56] |Punk8[,18] == z[71], 1, 0)
Punk8[,19] <- ifelse(Punk8[,19] == z[53] | Punk8[,19] == z[54]| Punk8[,19] == z[55] | Punk8[,19] == z[56] |Punk8[,19] == z[71], 1, 0)
Punk8[,20] <- ifelse(Punk8[,20] == z[53] | Punk8[,20] == z[54]| Punk8[,20] == z[55] | Punk8[,20] == z[56] |Punk8[,20] == z[71], 1, 0)
Punk8[,21] <- ifelse(Punk8[,21] == z[53] | Punk8[,21] == z[54]| Punk8[,21] == z[55] | Punk8[,21] == z[56] |Punk8[,21] == z[71], 1, 0)
Punk8[,22] <- ifelse(Punk8[,22] == z[53] | Punk8[,22] == z[54]| Punk8[,22] == z[55] | Punk8[,22] == z[56] |Punk8[,22] == z[71], 1, 0)
Punk8[,23] <- ifelse(Punk8[,23] == z[53] | Punk8[,23] == z[54]| Punk8[,23] == z[55] | Punk8[,23] == z[56] |Punk8[,23] == z[71], 1, 0)
Punk8[,24] <- ifelse(Punk8[,24] == z[53] | Punk8[,24] == z[54]| Punk8[,24] == z[55] | Punk8[,24] == z[56] |Punk8[,24] == z[71], 1, 0)
Punk8[,25] <- ifelse(Punk8[,25] == z[53] | Punk8[,25] == z[54]| Punk8[,25] == z[55] | Punk8[,25] == z[56] |Punk8[,25] == z[71], 1, 0)
Punk8[,26] <- ifelse(Punk8[,26] == z[53] | Punk8[,26] == z[54]| Punk8[,26] == z[55] | Punk8[,26] == z[56] |Punk8[,26] == z[71], 1, 0)
Punk8[,27] <- ifelse(Punk8[,27] == z[53] | Punk8[,27] == z[54]| Punk8[,27] == z[55] | Punk8[,27] == z[56] |Punk8[,27] == z[71], 1, 0)
Punk8[is.na(Punk8)] = 0
Punk8$Mohawk <- (Punk8$At1 + Punk8$At2 + Punk8$At3 + Punk8$At4 + Punk8$At5 + Punk8$At6 + Punk8$At7 + Punk8$At8 +
                      Punk8$At9 + Punk8$At10)
Punk8 <- subset(Punk8, select = -c(At1,At2,At3,At4,At5,At6,At7,At8,At9,At10))
Punk8 <- subset(Punk8, select = -c(Del1))
Punk8[,17] <- ifelse(Punk8[,17] == 2 | Punk8[,17] == 3 | Punk8[,17] == 1, 1, 0)

#Lipstick
Punk8$Characteristics <- Punk8$Attributes 
Punk8 <- Punk8 %>% separate(Characteristics, c('Del1', 'Else'),  sep = "–")
Punk8$Else<-gsub("[]]","",as.character(Punk8$Else))
Punk8$Else<-gsub("[[]","",as.character(Punk8$Else))
Punk8 <- na.omit(Punk8)
Punk8 <- Punk8 %>% separate(Else, c('At1', 'At2', 'At3', 'At4', 'At5', 'At6', 'At7', 'At8', 'At9', 'At10'),  sep = ", ")
names(Punk8)
Z <- table(Punk8$At3)
z <- names(Z)
names(Punk8)
Punk8[,18] <- ifelse(Punk8[,18] == z[7] | Punk8[,18] == z[42]| Punk8[,18] == z[64], 1, 0)
Punk8[,19] <- ifelse(Punk8[,19] == z[7] | Punk8[,19] == z[42]| Punk8[,19] == z[64], 1, 0)
Punk8[,20] <- ifelse(Punk8[,20] == z[7] | Punk8[,20] == z[42]| Punk8[,20] == z[64], 1, 0)
Punk8[,21] <- ifelse(Punk8[,21] == z[7] | Punk8[,21] == z[42]| Punk8[,21] == z[64], 1, 0)
Punk8[,22] <- ifelse(Punk8[,22] == z[7] | Punk8[,22] == z[42]| Punk8[,22] == z[64], 1, 0)
Punk8[,23] <- ifelse(Punk8[,23] == z[7] | Punk8[,23] == z[42]| Punk8[,23] == z[64], 1, 0)
Punk8[,24] <- ifelse(Punk8[,24] == z[7] | Punk8[,24] == z[42]| Punk8[,24] == z[64], 1, 0)
Punk8[,25] <- ifelse(Punk8[,25] == z[7] | Punk8[,25] == z[42]| Punk8[,25] == z[64], 1, 0)
Punk8[,26] <- ifelse(Punk8[,26] == z[7] | Punk8[,26] == z[42]| Punk8[,26] == z[64], 1, 0)
Punk8[,27] <- ifelse(Punk8[,27] == z[7] | Punk8[,27] == z[42]| Punk8[,27] == z[64], 1, 0)
Punk8[,28] <- ifelse(Punk8[,28] == z[7] | Punk8[,28] == z[42]| Punk8[,28] == z[64], 1, 0)

Punk8[is.na(Punk8)] = 0
Punk8$Lipstick <- (Punk8$At1 + Punk8$At2 + Punk8$At3 + Punk8$At4 + Punk8$At5 + Punk8$At6 + Punk8$At7 + Punk8$At8 +
                   Punk8$At9 + Punk8$At10)
Punk8 <- subset(Punk8, select = -c(At1,At2,At3,At4,At5,At6,At7,At8,At9,At10))
Punk8 <- subset(Punk8, select = -c(Del1))
Punk8[,18] <- ifelse(Punk8[,18] == 2 |  Punk8[,18] == 1, 1, 0)

#Zombie
Punk8$Characteristics <- Punk8$Attributes 
Punk8 <- Punk8 %>% separate(Characteristics, c('Del1', 'Else'),  sep = "–")
Punk8$Else<-gsub("[]]","",as.character(Punk8$Else))
Punk8$Else<-gsub("[[]","",as.character(Punk8$Else))
Punk8 <- na.omit(Punk8)
Punk8 <- Punk8 %>% separate(Else, c('At1', 'At2', 'At3', 'At4', 'At5', 'At6', 'At7', 'At8', 'At9', 'At10'),  sep = ", ")
names(Punk8)
Z <- table(Punk8$At1)
z <- names(Z)
names(Punk8)
Punk8[,20] <- ifelse(Punk8[,20] == z[91], 1, 0)
Punk8$Zombie <- Punk8$At1
Punk8 <- subset(Punk8, select = -c(At1,At2,At3,At4,At5,At6,At7,At8,At9,At10))
Punk8 <- subset(Punk8, select = -c(Del1))
#Beard <-  не будем делать, так как будет корреляция с Male

############################################Colnames
Punk8 <- Punk8 %>% separate(Name, c('Album', 'ID'),  sep = "#")
names(Punk8)
Punk8 <- subset(Punk8, select=-c(Attributes, Album, `FIRST SALE PRICE ETH`, `LAST SALE PRICE ETH`))
colnames(Punk8) = c('Project', 'Id', 'First_price', 'Last_price', 'Owners', 'Accumulated_sales', 'Sold_count', 
                   'Transactions', 'Date', "Sex", "Cap","Earring","Cigarrette", "Mohawk","Lipstick" ,"Zombie" )

############################################Quaters and Hour
Punk9 <- Punk8
Punk9 <- Punk9 %>% separate(Date, c('Date', 'Time'),  sep = ",")
Punk9 <- Punk9 %>% separate(Time, c('Hour', 'Del1', 'Del2'),  sep = ":")
Punk9 <- Punk9 %>% separate(Date, c('Del3', 'Month', 'Year'),  sep = "/")
Punk9 <- subset(Punk9, select = -c(Del1, Del2, Del3))

#Совместим период и год
Punk9$Monthly <- paste(Punk9$Year, Punk9$Month)
#Удалим вспомогательные столбцы
Punk9 <- subset(Punk9, select = c(-Year, -Month))
#Проверим, что получилось
d <-  as.data.frame(table(Punk9$Monthly))
d

############################################Отсчётный год

table(Punk9$Monthly)
Punk9$Monthly <- factor(Punk9$Monthly, levels = c('2021 09', '2017 06', '2017 07', '2017 08', '2017 09', '2017 10', '2017 11','2017 12', '2018 01', '2018 02', '2018 03',
                                                    '2018 04', '2018 05', '2018 06', '2018 07', '2018 08', '2018 09', '2018 10', 
                                                    '2018 11', '2018 12', '2019 01', '2019 02', '2019 03', '2019 04', '2019 05',
                                                    '2019 06', '2019 07', '2019 08', '2019 09', '2019 10', '2019 11', '2019 12',
                                                    '2020 01', '2020 02', '2020 03', '2020 04', '2020 05', '2020 06', '2020 07',
                                                    '2020 08', '2020 09', '2020 10', '2020 11', '2020 12', '2021 01', '2021 02',
                                                    '2021 03', '2021 04', '2021 05','2021 06', '2021 07', '2021 08',
                                                    '2021 10', '2021 11', '2021 12', '2022 01', '2022 02'))



####Crypto Index####

#Модель
#Гипотеза: час продажи <- влияет. Инд. характеристики влияют.
Crypto_mod <- subset(Punk9, select = -c(Project, Id, Transactions))
Crypto_mod <- subset(Crypto_mod, Last_price!="0")
Crypto_mod <- subset(Crypto_mod, First_price!="0")

mod_nft_h <- lm(Last_price ~ . ,data = Crypto_mod)
names(Crypto_mod)
summary(mod_nft_h)

#Мультикол. высокая у Price_Av и Accum.sales. 
#Но этому явно есть лог. объяснение

plot(mod_nft_h, 1)
#Линии не совпадают
plot(mod_nft_h, 2)
#от -20 до 20
plot(mod_nft_h, 3)

#________________________________________
#Нормальность остатков
boxCox(mod_nft_h)
#Нужен логарифм
mod_nft_h1 <- update(mod_nft_h, log(Last_price) ~ . )
summary(mod_nft_h1)
plot(mod_nft_h1, 2)
#От - 5 до 5
mod_nft_h1

#________________________________________
mod_nft_h2 <- stepAIC(mod_nft_h1)
summary(mod_nft_h2)

#________________________________________
#Спецификация
plot(mod_nft_h2, which = 1) 
#2 выброса есть
crPlots(mod_nft_h2)
#First_price
mod_nft_h3 <- update(mod_nft_h2, . ~ . + log(First_price) - First_price)
summary(mod_nft_h3)
crPlots(mod_nft_h3)
#Accum_sales
mod_nft_h4 <- update(mod_nft_h3, . ~ . + log(Accumulated_sales) - Accumulated_sales)
#Не работает
#Owners (просто 1 выброс)
#Owners (просто 1 выброс)
#Sold count (просто 1 выброс). Тот же, что и Owners
vif(mod_nft_h3)
mod_nft_h3

#________________________________________
#Гетероскедастичность - одинакова ли станд.ошибка
#plot(mod_nft_h2, which = 3)
#Тест Бреуша-Пагана
bptest(mod_nft_h3)
#H0 - гетероскедастичности нет при p-value большом выполняется
#Гетеро <- есть (неоднородность остатков)
#Перейдём к робастным ошибкам в форме Вайта
library(sandwich)
cov_white <- vcovHC(mod_nft_h3, type = "HC0")
coeftest(mod_nft_h3, cov_white)
#Стало много значимого

#________________________________________

mod_NFT_Crypto <- mod_nft_h3
summary(mod_NFT_Crypto)
NFT_Crypto<- coef(mod_NFT_Crypto)
stargazer(mod_NFT_Crypto, type = 'html', df = FALSE, 
          digits =2, title = 'Crypto Punks',
          out = 'CryptoPunks3.html', vcov = 'HC0')


#Год для сравнения <-  2017 12 (так как много наблюдений с этого момента)
NFT_Crypto<- coef(mod_NFT_Crypto)
View(NFT_Crypto)
NFT_Crypto1 <- NFT_Crypto[7:62]
#write.xlsx(p, 'NFT_Kitty_Index.xlsx')

####Meebits####
#Total - 20 000
Table <- as.data.frame(table(DataNFT$PROJECT))
Table <- table(DataNFT$PROJECT)
a <- names(Table)[Table>11920]
Meeb <- filter(DataNFT, PROJECT %in% a)
Table <- table(Meeb$PROJECT)
a <- names(Table)[Table<11922]
Meeb <- filter(Meeb, PROJECT %in% a)
View(Meeb1)
Meeb1 <- subset(Meeb, select = -c(`FIRST SALE PRICE ETH`, `LAST SALE PRICE ETH`))

#glasses
Meeb1$Glasses <- Meeb1$Attributes
Meeb1 <- Meeb1 %>% separate(Glasses, c('Glasses', 'Other'),  sep = "glasses ")
Meeb1 <- Meeb1 %>% separate(Other, c('-', 'Glasses'),  sep = '–')
Meeb1$Glasses[is.na(Meeb1$Glasses)] = 'No'
Meeb1$Glasses <- gsub("shirt","",as.character(Meeb1$Glasses))
Meeb1$Glasses <- gsub("pants_color","",as.character(Meeb1$Glasses))
Meeb1$Glasses <- gsub(" ","",as.character(Meeb1$Glasses))
Meeb1 <- subset(Meeb1, select=-c(`-`))
table(Meeb1$Glasses)
Meeb1$Glasses <- gsub("\\r\\n", "", Meeb1$Glasses)
#beard
Meeb1$Beard <- Meeb1$Attributes
Meeb1 <- Meeb1 %>% separate(Beard, c('Beard', 'Other'),  sep = "beard ")
Meeb1 <- Meeb1 %>% separate(Other, c('-', 'Beard'),  sep = '–')
Meeb1$Beard[is.na(Meeb1$Beard)] = 'No'
Meeb1$Beard <- gsub("pants","",as.character(Meeb1$Beard))
Meeb1$Beard <- gsub("pants_color","",as.character(Meeb1$Beard))
Meeb1 <- subset(Meeb1, select=-c(`-`))
table(Meeb1$Beard)
Meeb1$Beard <- gsub("\\r\\n", "", Meeb1$Beard)
#pants_color
Meeb1$Pants_Color <- Meeb1$Attributes
Meeb1 <- Meeb1 %>% separate(Pants_Color, c('Pants_Color', 'Other'),  sep = "pants_color ")
Meeb1 <- Meeb1 %>% separate(Other, c('-', 'Pants_Color'),  sep = '–')
Meeb1$Pants_Color[is.na(Meeb1$Pants_Color)] = 'No'
Meeb1$Pants_Color <- gsub("pants","",as.character(Meeb1$Pants_Color))
Meeb1$Pants_Color <- gsub("shirt","",as.character(Meeb1$Pants_Color))
Meeb1$Pants_Color <- gsub("beard","",as.character(Meeb1$Pants_Color))
Meeb1 <- subset(Meeb1, select=-c(`-`))
table(Meeb1$Pants_Color)
Meeb1$Pants_Color <- gsub("\\r\\n", "", Meeb1$Pants_Color)
#shirt
Meeb1$Shirt <- Meeb1$Attributes
Meeb1 <- Meeb1 %>% separate(Shirt, c('Shirt', 'Other'),  sep = "shirt ")
Meeb1 <- Meeb1 %>% separate(Other, c('-', 'Shirt'),  sep = '–')
Meeb1$Shirt[is.na(Meeb1$Shirt)] = 'No'
Meeb1$Shirt <- gsub("hair_style","",as.character(Meeb1$Shirt))
Meeb1$Shirt <- gsub("pants","",as.character(Meeb1$Shirt))
Meeb1$Shirt <- gsub("beard_color","",as.character(Meeb1$Shirt))
Meeb1$Shirt <- gsub("over","",as.character(Meeb1$Shirt))
Meeb1$Shirt <- gsub("beard","",as.character(Meeb1$Shirt))
Meeb1 <- subset(Meeb1, select=-c(`-`))
table(Meeb1$Shirt)
Meeb1$Shirt <- gsub("\\r\\n", "", Meeb1$Shirt)
#pants
Meeb1$Pants <- Meeb1$Attributes
Meeb1 <- Meeb1 %>% separate(Pants, c('Pants', 'Other'),  sep = "pants ")
Meeb1 <- Meeb1 %>% separate(Other, c('-', 'Pants'),  sep = '–')
Meeb1$Pants[is.na(Meeb1$Pants)] = 'No'
Meeb1$Pants <- gsub("hair_style","",as.character(Meeb1$Pants))
Meeb1$Pants <- gsub("shoes_color","",as.character(Meeb1$Pants))
Meeb1$Pants <- gsub("type","",as.character(Meeb1$Pants))
Meeb1$Pants <- gsub("tattoo_motif","",as.character(Meeb1$Pants))
Meeb1$Pants <- gsub("hat_color","",as.character(Meeb1$Pants))
Meeb1$Pants <- gsub("necklace ","",as.character(Meeb1$Pants))
Meeb1$Pants <- gsub("overshirt","",as.character(Meeb1$Pants))
Meeb1$Pants <- gsub("beard_color ","",as.character(Meeb1$Pants))
Meeb1 <- subset(Meeb1, select=-c(`-`))
table(Meeb1$Pants)
Meeb1$Pants <- gsub("\\r\\n", "", Meeb1$Pants)
#hair_style
Meeb1$Hair_style <- Meeb1$Attributes
Meeb1 <- Meeb1 %>% separate(Hair_style, c('Hair_style', 'Other'),  sep = "hair_style ")
Meeb1 <- Meeb1 %>% separate(Other, c('-', 'Hair_style'),  sep = '–')
Meeb1$Hair_style[is.na(Meeb1$Hair_style)] = 'No'
Meeb1$Hair_style <- gsub("shirt_color","",as.character(Meeb1$Hair_style))
Meeb1$Hair_style <- gsub("shoes_color","",as.character(Meeb1$Hair_style))
Meeb1$Hair_style <- gsub("type","",as.character(Meeb1$Hair_style))
Meeb1$Hair_style <- gsub("tattoo_motif","",as.character(Meeb1$Hair_style))
Meeb1$Hair_style <- gsub("hat_color","",as.character(Meeb1$Hair_style))
Meeb1$Hair_style <- gsub("necklace ","",as.character(Meeb1$Hair_style))
Meeb1$Hair_style <- gsub("overshirt","",as.character(Meeb1$Hair_style))
Meeb1$Hair_style <- gsub("beard_color ","",as.character(Meeb1$Hair_style))
Meeb1$Hair_style <- gsub("hat","",as.character(Meeb1$Hair_style))
Meeb1$Hair_style <- gsub("glasses_color ","",as.character(Meeb1$Hair_style))
Meeb1 <- subset(Meeb1, select=-c(`-`))
table(Meeb1$Hair_style)
Meeb1$Hair_style <- gsub("\\r\\n", "", Meeb1$Hair_style)
#shirt_color
Meeb1$Shirt_color <- Meeb1$Attributes
Meeb1 <- Meeb1 %>% separate(Shirt_color, c('Shirt_color', 'Other'),  sep = "shirt_color ")
Meeb1 <- Meeb1 %>% separate(Other, c('-', 'Shirt_color'),  sep = '–')
Meeb1$Shirt_color[is.na(Meeb1$Shirt_color)] = 'No'
Meeb1$Shirt_color <- gsub("shoes_color","",as.character(Meeb1$Shirt_color))
Meeb1$Shirt_color <- gsub("type","",as.character(Meeb1$Shirt_color))
Meeb1$Shirt_color <- gsub("tattoo_motif","",as.character(Meeb1$Shirt_color))
Meeb1$Shirt_color <- gsub("hat_color","",as.character(Meeb1$Shirt_color))
Meeb1$Shirt_color <- gsub("necklace ","",as.character(Meeb1$Shirt_color))
Meeb1$Shirt_color <- gsub("overshirt","",as.character(Meeb1$Shirt_color))
Meeb1$Shirt_color <- gsub("beard_color ","",as.character(Meeb1$Shirt_color))
Meeb1$Shirt_color <- gsub("hat","",as.character(Meeb1$Shirt_color))
Meeb1$Shirt_color <- gsub("glasses_color ","",as.character(Meeb1$Shirt_color))
Meeb1$Shirt_color <- gsub("shoes ","",as.character(Meeb1$Shirt_color))
Meeb1 <- subset(Meeb1, select=-c(`-`))
table(Meeb1$Shirt_color)
Meeb1$Shirt_color <- gsub("\\r\\n", "", Meeb1$Shirt_color)
#type!
Meeb1$Type <- Meeb1$Attributes
Meeb1 <- Meeb1 %>% separate(Type, c('Type', 'Other'),  sep = "type ")
Meeb1 <- Meeb1 %>% separate(Other, c('-', 'Type'),  sep = '–')
Meeb1$Type[is.na(Meeb1$Type)] = 'No'
Meeb1$Type <- gsub("shoes_color","",as.character(Meeb1$Type))
Meeb1$Type <- gsub("tattoo_motif","",as.character(Meeb1$Type))
Meeb1$Type <- gsub("hat_color","",as.character(Meeb1$Type))
Meeb1$Type <- gsub("necklace ","",as.character(Meeb1$Type))
Meeb1$Type <- gsub("overshirt","",as.character(Meeb1$Type))
Meeb1$Type <- gsub("beard_color ","",as.character(Meeb1$Type))
Meeb1$Type <- gsub("hat","",as.character(Meeb1$Type))
Meeb1$Type <- gsub("glasses_color ","",as.character(Meeb1$Type))
Meeb1$Type <- gsub("shoes ","",as.character(Meeb1$Type))
Meeb1$Type <- gsub("_color ","",as.character(Meeb1$Type))
Meeb1 <- subset(Meeb1, select=-c(`-`))
table(Meeb1$Type)
Meeb1$Type <- gsub("\\r\\n", "", Meeb1$Type)
#shoes = type
#hair_color
Meeb1$Hair_color <- Meeb1$Attributes
Meeb1 <- Meeb1 %>% separate(Hair_color, c('Hair_color', 'Other'),  sep = "hair_color ")
Meeb1 <- Meeb1 %>% separate(Other, c('-', 'Hair_color'),  sep = '–')
Meeb1$Hair_color[is.na(Meeb1$Hair_color)] = 'No'
Meeb1$Hair_color <- gsub("shoes_color","",as.character(Meeb1$Hair_color))
Meeb1$Hair_color <- gsub("tattoo_motif","",as.character(Meeb1$Hair_color))
Meeb1$Hair_color <- gsub("hat_color","",as.character(Meeb1$Hair_color))
Meeb1$Hair_color <- gsub("necklace ","",as.character(Meeb1$Hair_color))
Meeb1$Hair_color <- gsub("overshirt","",as.character(Meeb1$Hair_color))
Meeb1$Hair_color <- gsub("beard_color ","",as.character(Meeb1$Hair_color))
Meeb1$Hair_color <- gsub("hat","",as.character(Meeb1$Hair_color))
Meeb1$Hair_color <- gsub("glasses_color ","",as.character(Meeb1$Hair_color))
Meeb1$Hair_color <- gsub("shoes ","",as.character(Meeb1$Hair_color))
Meeb1$Hair_color <- gsub("_color ","",as.character(Meeb1$Hair_color))
Meeb1$Hair_color <- gsub("shirt ","",as.character(Meeb1$Hair_color))
Meeb1$Hair_color <- gsub("glasses ","",as.character(Meeb1$Hair_color))
Meeb1 <- subset(Meeb1, select=-c(`-`))
table(Meeb1$Hair_color)
Meeb1$Hair_color <- gsub("\\r\\n", "", Meeb1$Hair_color)

############################################Colnames
Meeb2 <- Meeb1
Meeb2 <- Meeb2 %>% separate(Name, c('Album', 'ID'),  sep = "#")
names(Meeb2)
Meeb2 <- subset(Meeb2, select=-c(Attributes, Album))
colnames(Meeb2) = c('Project', 'Id', 'First_price', 'Last_price', 'Owners', 'Accumulated_sales', 'Sold_count', 
                    'Transactions', 'Date', "Glasses", "Beard","Pants_Color","Shirt", "Pants","Hair_style" ,"Shirt_color",
                    'Type', 'Hair_color')

############################################Quaters and Hour
Meeb3 <- Meeb2
Meeb3 <- Meeb3 %>% separate(Date, c('Date', 'Time'),  sep = ",")
Meeb3 <- Meeb3 %>% separate(Time, c('Hour', 'Del1', 'Del2'),  sep = ":")
Meeb3 <- Meeb3 %>% separate(Date, c('Del3', 'Month', 'Year'),  sep = "/")
Meeb3 <- subset(Meeb3, select = -c(Del1, Del2, Del3))

#Совместим период и год
Meeb3$Monthly <- paste(Meeb3$Year, Meeb3$Month)
#Удалим вспомогательные столбцы
Meeb3 <- subset(Meeb3, select = c(-Year, -Month))
#Проверим, что получилось
d <-  as.data.frame(table(Meeb3$Monthly))
d

############################################Отсчётный год

table(Meeb3$Monthly)
Meeb3$Monthly <- factor(Meeb3$Monthly, levels = c('2021 09', '2021 05','2021 06', '2021 07', '2021 08',
                                                    '2021 10', '2021 11', '2021 12', '2022 01', '2022 02'))

############################################Сократим Attr.
a <- names(table(Meeb3$Hair_style))
Meeb3[,15] <- ifelse(Meeb3[,15] == a[10], 'Buzzcut',
                     ifelse(Meeb3[,15] == a[36], 'Simple',
                            ifelse(Meeb3[,15] == a[2], 'Bald',
                                   ifelse(Meeb3[,15] == a[44], 'Wild',
                                          ifelse(Meeb3[,15] == a[14], 'Fade', 'Other')))))
a <- names(table(Meeb3$Shirt_color))
Meeb3[,16] <- ifelse(Meeb3[,16] == a[3] | Meeb3[,16] == a[4], 'Black',
                     ifelse(Meeb3[,16] == a[10] | Meeb3[,16] == a[9], 'Gray',
                            ifelse(Meeb3[,16] == a[30] | Meeb3[,16] == a[29], 'White',
                                   ifelse(Meeb3[,16] == a[32] | Meeb3[,16] == a[31], 'Yellow',
                                          ifelse(Meeb3[,16] == a[28] | Meeb3[,16] == a[27], 'Red Plaid',
                                                 ifelse(Meeb3[,16] == a[25] | Meeb3[,16] == a[26], 'Red', 
                                                        ifelse(Meeb3[,16] == a[23] | Meeb3[,16] == a[24], 'Purple',
                                                               ifelse(Meeb3[,16] == a[21] | Meeb3[,16] == a[22], 'Posh',
                                                                      ifelse(Meeb3[,16] == a[19] | Meeb3[,16] == a[20], 'Magenta',
                                                                             ifelse(Meeb3[,16] == a[17] | Meeb3[,16] == a[18], 'Luxe',
                                                                                    ifelse(Meeb3[,16] == a[15] | Meeb3[,16] == a[16],'Leopard Print',
                                                                                           ifelse(Meeb3[,16] == a[13] | Meeb3[,16] == a[14],'Green Plaid',
                                                                                                  ifelse(Meeb3[,16] == a[12] | Meeb3[,16] == a[11],'Green',
                                                                                                         ifelse(Meeb3[,16] == a[7] | Meeb3[,16] == a[8],'Camo',
                                                                                                                ifelse(Meeb3[,16] == a[5] | Meeb3[,16] == a[6],'Blue Camo',
                                                                                                                       ifelse(Meeb3[,16] == a[1] | Meeb3[,16] == a[2],'Argyle', 'Other'))))))))))))))))
#Удалим единственное наблюдение
Meeb3 <- Meeb3[-c(5076),]

####Index Meebits####
#Модель
#Гипотеза: час продажи <- влияет. Инд. характеристики влияют.
Meeb_mod <- subset(Meeb3, select = -c(Project, Id, Transactions))
Meeb_mod <- subset(Meeb_mod, Last_price!="0")
Meeb_mod <- subset(Meeb_mod, First_price!="0")

mod_nft_h <- lm(Last_price ~ . ,data = Meeb_mod)
summary(mod_nft_h)
#Много незначимого

#Мультикол. высокая у Price_Av и Accum.sales. 
#Но этому явно есть лог. объяснение

plot(mod_nft_h, 1)
#Всё собрано в одном месте
plot(mod_nft_h, 2)
#от - 20 до 40
plot(mod_nft_h, 3)
#Всё собрано в одном месте

#________________________________________
#Нормальность остатков
boxCox(mod_nft_h)
#Нужен логарифм
mod_nft_1 <- update(mod_nft_h, log(Last_price) ~ . )
summary(mod_nft_1)
#R2 почему-то упал до 0.16???
plot(mod_nft_1, 2)
#От -10 до 5
mod_nft_1

#________________________________________
mod_nft_2 <- stepAIC(mod_nft_1)
summary(mod_nft_2)
vif(mod_nft_2)

#________________________________________
#Спецификация
plot(mod_nft_2, which = 1) 
crPlots(mod_nft_2)
resettest(mod_nft_2)

#First_price
mod_nft_3 <- update(mod_nft_2, . ~ . + log(First_price) - First_price)
summary(mod_nft_3)
#Это явно требовалось. R2 подлетел до 0.58

#Accumulated_sales
mod_nft_4 <- update(mod_nft_3, . ~ . + log(Accumulated_sales) - Accumulated_sales)
summary(mod_nft_4)
#R2 возрос до 0.59

#Sold_count
resettest(mod_nft_4, which = 2)
crPlots(mod_nft_4)
mod_nft_5 <- update(mod_nft_4, . ~ . + log(Sold_count) - Sold_count)
summary(mod_nft_5)
#R2 возрос до 0.6063
crPlots(mod_nft_5)
#Вот теперь всё хорошо
plot(mod_nft_5, which = 1) 
mod_nft_5

#________________________________________
#Гетероскедастичность - одинакова ли станд.ошибка
plot(mod_nft_5, which = 3)
#Тест Бреуша-Пагана
bptest(mod_nft_5)
#H0 - гетероскедастичности нет при p-value большом выполняется
#Гетеро <- есть (неоднородность остатков)
#Перейдём к робастным ошибкам в форме Вайта
library(sandwich)
cov_white <- vcovHC(mod_nft_5, type = "HC0")
coeftest(mod_nft_5, cov_white)
#Стало много незначимого

#________________________________________

mod_NFT_Meebits <- mod_nft_5
summary(mod_nft_h2, vcov = cov_white)

#Год для сравнения <-  2017 12 (так как много наблюдений с этого момента)
NFT_Meebits <- coef(mod_NFT_Meebits,  vcov = cov_white)
View(NFT_Meebits)
NFT_Meebits1 <- NFT_Meebits[67:76]
#write.xlsx(p




####CyberKongz####
#Total - 20 000
Table <- as.data.frame(table(DataNFT$PROJECT))
Table <- table(DataNFT$PROJECT)
a <- names(Table)[Table>14441]
Kong <- filter(DataNFT, PROJECT %in% a)
Table <- table(Kong$PROJECT)
a <- names(Table)[Table<14443]
Kong <- filter(Kong, PROJECT %in% a)
View(Kong)

#Оcтавим только CyberKong VX 
Kong <- Kong %>% separate(Name, c('Album', 'ID'),  sep = "#")
table(Kong$Album)
Kong1 <- filter(Kong, Album == 'CyberKong VX ')
Kong1 <- subset(Kong1, select = -c(`FIRST SALE PRICE ETH`, `LAST SALE PRICE ETH`))

#body_accessory <- 5936 none
Kong1$Body_accessory <- Kong1$Attributes
Kong1 <- Kong1 %>% separate(Body_accessory, c('Body_accessory', 'Other'),  sep = "body_accessory ")
Kong1 <- Kong1 %>% separate(Other, c('-', 'Body_accessory'),  sep = '–')
Kong1$Body_accessory[is.na(Kong1$Body_accessory)] = 'No'
Kong1$Body_accessory <- gsub("fur","",as.character(Kong1$Body_accessory))
Kong1 <- subset(Kong1, select=-c(`-`))
Kong1$Body_accessory <- gsub("\\r\\n", "", Kong1$Body_accessory)
table(Kong1$Body_accessory)
#fur <- 9873 none
Kong1$Fur <- Kong1$Attributes
Kong1 <- Kong1 %>% separate(Fur, c('Fur', 'Other'),  sep = "fur ")
Kong1 <- Kong1 %>% separate(Other, c('-', 'Fur'),  sep = '–')
Kong1$Fur[is.na(Kong1$Fur)] = 'No'
Kong1$Fur <- gsub("head_accessory","",as.character(Kong1$Fur))
Kong1 <- subset(Kong1, select=-c(`-`))
Kong1$Fur <- gsub("\\r\\n", "", Kong1$Fur)
table(Kong1$Fur)
#head_accessory <- 8018 none
Kong1$Head_accessory <- Kong1$Attributes
Kong1 <- Kong1 %>% separate(Head_accessory, c('Head_accessory', 'Other'),  sep = "head_accessory ")
Kong1 <- Kong1 %>% separate(Other, c('-', 'Head_accessory'),  sep = '–')
Kong1$Head_accessory[is.na(Kong1$Head_accessory)] = 'No'
Kong1$Head_accessory <- gsub("mouth","",as.character(Kong1$Head_accessory))
Kong1 <- subset(Kong1, select=-c(`-`))
Kong1$Head_accessory <- gsub("\\r\\n", "", Kong1$Head_accessory)
table(Kong1$Head_accessory)
#mouth <- 5818 none
Kong1$Mouth <- Kong1$Attributes
Kong1 <- Kong1 %>% separate(Mouth, c('Mouth', 'Other'),  sep = "mouth ")
Kong1 <- Kong1 %>% separate(Other, c('-', 'Mouth'),  sep = '–')
Kong1$Mouth[is.na(Kong1$Mouth)] = 'No'
Kong1$Mouth <- gsub("legendary","",as.character(Kong1$Mouth))
Kong1 <- subset(Kong1, select=-c(`-`))
Kong1$Mouth <- gsub("\\r\\n", "", Kong1$Mouth)
table(Kong1$Mouth)
#hat <- 1718 none
Kong1$Hat <- Kong1$Attributes
Kong1 <- Kong1 %>% separate(Hat, c('Hat', 'Other'),  sep = "hat ")
Kong1 <- Kong1 %>% separate(Other, c('-', 'Hat'),  sep = '–')
Kong1$Hat[is.na(Kong1$Hat)] = 'No'
Kong1$Hat <- gsub("augmentation","",as.character(Kong1$Hat))
Kong1 <- subset(Kong1, select=-c(`-`))
Kong1$Hat <- gsub("\\r\\n", "", Kong1$Hat)
table(Kong1$Hat)
#augmentation <- 10429 none
Kong1$Augmentation <- Kong1$Attributes
Kong1 <- Kong1 %>% separate(Augmentation, c('Augmentation', 'Other'),  sep = "augmentation ")
Kong1 <- Kong1 %>% separate(Other, c('-', 'Augmentation'),  sep = '–')
Kong1$Augmentation[is.na(Kong1$Augmentation)] = 'No'
Kong1$Augmentation <- gsub("eyes","",as.character(Kong1$Augmentation))
Kong1 <- subset(Kong1, select=-c(`-`))
Kong1$Augmentation <- gsub("\\r\\n", "", Kong1$Augmentation)
table(Kong1$Augmentation)
#legendary <- 11576 none
Kong1$Legendary <- Kong1$Attributes
Kong1 <- Kong1 %>% separate(Legendary, c('Legendary', 'Other'),  sep = "legendary ")
Kong1 <- Kong1 %>% separate(Other, c('-', 'Legendary'),  sep = '–')
Kong1$Legendary[is.na(Kong1$Legendary)] = 'No'
Kong1$Legendary <- gsub("hat","",as.character(Kong1$Legendary))
Kong1 <- subset(Kong1, select=-c(`-`))
Kong1$Legendary <- gsub("\\r\\n", "", Kong1$Legendary)
table(Kong1$Legendary)
#eyes <- 2790 None
Kong1$Eyes <- Kong1$Attributes
Kong1 <- Kong1 %>% separate(Eyes, c('Eyes', 'Other'),  sep = "eyes ")
Kong1 <- Kong1 %>% separate(Other, c('-', 'Eyes'),  sep = '–')
Kong1$Eyes[is.na(Kong1$Eyes)] = 'No'
Kong1$Eyes <- gsub("clothes ","",as.character(Kong1$Eyes))
Kong1 <- subset(Kong1, select=-c(`-`))
Kong1$Eyes <- gsub("\\r\\n", "", Kong1$Eyes)
table(Kong1$Eyes)
#clothes <- 10205 none
Kong1$Clothes <- Kong1$Attributes
Kong1 <- Kong1 %>% separate(Clothes, c('Clothes', 'Other'),  sep = "clothes ")
Kong1 <- Kong1 %>% separate(Other, c('-', 'Clothes'),  sep = '–')
Kong1$Clothes[is.na(Kong1$Clothes)] = 'No'
Kong1 <- subset(Kong1, select=-c(`-`))
Kong1$Clothes <- gsub("\\r\\n", "", Kong1$Clothes)
table(Kong1$Clothes)

############################################Colnames
names(Kong1)
Kong1 <- subset(Kong1, select=-c(Attributes, Clothes, Legendary, Augmentation, Fur))
colnames(Kong1) = c('Project', 'Album', 'Id', 'First_price', 'Last_price', 'Owners', 'Accumulated_sales', 'Sold_count', 
                   'Transactions', 'Date', "Body_accessory", "Head_accessory","Mouth", "Hat","Eyes")

############################################Quaters and Hour
Kong2 <- Kong1
Kong2 <- Kong2 %>% separate(Date, c('Date', 'Time'),  sep = ",")
Kong2 <- Kong2 %>% separate(Time, c('Hour', 'Del1', 'Del2'),  sep = ":")
Kong2 <- Kong2 %>% separate(Date, c('Del3', 'Month', 'Year'),  sep = "/")
Kong2 <- subset(Kong2, select = -c(Del1, Del2, Del3))

#Совместим период и год
Kong2$Monthly <- paste(Kong2$Year, Kong2$Month)
#Удалим вспомогательные столбцы
Kong2 <- subset(Kong2, select = c(-Year, -Month))
#Проверим, что получилось
d <-  as.data.frame(table(Kong2$Monthly))
d

############################################Отсчётный год

table(Kong2$Monthly)
Kong2$Monthly <- factor(Kong2$Monthly, levels = c('2021 09', '2021 08', '2021 10', '2021 11', 
                                                   '2021 12', '2022 01', '2022 02'))


####Index CyberKongz####
View(Kong2)
#Модель
#Гипотеза: час продажи <- влияет. Инд. характеристики влияют.
Kong_mod <- subset(Kong2, select = -c(Project, Album, Id, Transactions))
Kong_mod <- subset(Kong_mod, Last_price!="0")
Kong_mod <- subset(Kong_mod, First_price!="0")

mod_nft_h <- lm(Last_price ~ . ,data = Kong_mod)
summary(mod_nft_h)

#Мультикол. высокая у Price_Av и Accum.sales. 
#Но этому явно есть лог. объяснение

plot(mod_nft_h, 1)
#Линии не совпадают
plot(mod_nft_h, 2)
#от - 20 до + 20
plot(mod_nft_h, 3)

#________________________________________
#Нормальность остатков
boxCox(mod_nft_h)
#Нужен логарифм
mod_nft_h1 <- update(mod_nft_h, log(Last_price) ~ . )
summary(mod_nft_h1)
plot(mod_nft_h1, 2)
#от -10 до 5
mod_nft_h1

#________________________________________
#Спецификация
plot(mod_nft_h1, which = 1) 
crPlots(mod_nft_h1)

#First_sales
mod_nft_h2 <- update(mod_nft_h1, . ~ . +log(First_price) - First_price)
summary(mod_nft_h2)
#Accumulated_sales
mod_nft_h3 <- update(mod_nft_h2, . ~ . +log(Accumulated_sales) - Accumulated_sales)
summary(mod_nft_h3)
#Стало получше
plot(mod_nft_h3, 1)
mod_nft_h3

#________________________________________
mod_nft_h4 <- stepAIC(mod_nft_h3)
summary(mod_nft_h4)

#________________________________________
#Гетероскедастичность - одинакова ли станд.ошибка
plot(mod_nft_h4, which = 3)
#Тест Бреуша-Пагана
bptest(mod_nft_h4)
#H0 - гетероскедастичности нет при p-value большом выполняется
#Гетеро <- есть (неоднородность остатков)
#Перейдём к робастным ошибкам в форме Вайта
library(sandwich)
cov_white <- vcovHC(mod_nft_h4, type = "HC0")
coeftest(mod_nft_h4, cov_white)
summary(mod_nft_h4, vcov = cov_white)

#________________________________________

mod_NFT_Kong<- mod_nft_h4
summary(mod_NFT_Kong,  vcov = cov_white)

#Год для сравнения <-  2017 12 (так как много наблюдений с этого момента)
NFT_Kong <- coef(mod_NFT_Kong,  vcov = cov_white)
View(NFT_Kong)

####Cool Cats####
#Total - 9 999
Table <- as.data.frame(table(DataNFT$PROJECT))
Table <- table(DataNFT$PROJECT)
a <- names(Table)[Table>9916]
Cats <- filter(DataNFT, PROJECT %in% a)
Table <- table(Cats$PROJECT)
a <- names(Table)[Table<9918]
Cats <- filter(Cats, PROJECT %in% a)
Table <- table(Cats$PROJECT)
Cats <- filter(Cats, PROJECT == 'Cool Cats')
View(Cats)
#Оcтавим только CyberKong VX 
Cats1 <- Cats %>% separate(Name, c('Album', 'ID'),  sep = "#")
table(Cats1$Album)
Cats1 <- subset(Cats1, select = -c(`FIRST SALE PRICE ETH`, `LAST SALE PRICE ETH`))
View(Cats1)

#total_points
Cats1$Total_points <- Cats1$Attributes
Cats1 <- Cats1 %>% separate(Total_points, c('Total_points', 'Other'),  sep = "total_points ")
Cats1 <- Cats1 %>% separate(Other, c('-', 'Total_points'),  sep = '–')
Cats1$Total_points[is.na(Cats1$Total_points)] = 'No'
Cats1$Total_points <- gsub("hats","",as.character(Cats1$Total_points))
Cats1 <- subset(Cats1, select=-c(`-`))
Cats1$Total_points <- gsub("\\r\\n", "", Cats1$Total_points)
table(Cats1$Total_points)
#hats
Cats1$Hats <- Cats1$Attributes
Cats1 <- Cats1 %>% separate(Hats, c('Hats', 'Other'),  sep = "hats ")
Cats1 <- Cats1 %>% separate(Other, c('-', 'Hats'),  sep = '–')
Cats1$Hats[is.na(Cats1$Hats)] = 'No'
Cats1$Hats <- gsub("hats_points","",as.character(Cats1$Hats))
Cats1 <- subset(Cats1, select=-c(`-`))
Cats1$Hats <- gsub("\\r\\n", "", Cats1$Hats)
table(Cats1$Hats)
#hats_points
Cats1$Hats_points <- Cats1$Attributes
Cats1 <- Cats1 %>% separate(Hats_points, c('Hats_points', 'Other'),  sep = "hats_points ")
Cats1 <- Cats1 %>% separate(Other, c('-', 'Hats_points'),  sep = '–')
Cats1$Hats_points[is.na(Cats1$Hats_points)] = 'No'
Cats1$Hats_points <- gsub("body_points","",as.character(Cats1$Hats_points))
Cats1 <- subset(Cats1, select=-c(`-`))
Cats1$Hats_points <- gsub("\\r\\n", "", Cats1$Hats_points)
table(Cats1$Hats_points)
#body_points <- nothing
Cats1$Body_points <- Cats1$Attributes
Cats1 <- Cats1 %>% separate(Body_points, c('Body_points', 'Other'),  sep = "body_points ")
Cats1 <- Cats1 %>% separate(Other, c('-', 'Body_points'),  sep = '–')
Cats1$Body_points[is.na(Cats1$Body_points)] = 'No'
Cats1$Body_points <- gsub("body","",as.character(Cats1$Body_points))
Cats1 <- subset(Cats1, select=-c(`-`))
Cats1$Body_points <- gsub("\\r\\n", "", Cats1$Body_points)
table(Cats1$Body_points)
#face_points
Cats1$Face_points <- Cats1$Attributes
Cats1 <- Cats1 %>% separate(Face_points, c('Face_points', 'Other'),  sep = "face_points ")
Cats1 <- Cats1 %>% separate(Other, c('-', 'Face_points'),  sep = '–')
Cats1$Face_points[is.na(Cats1$Face_points)] = 'No'
Cats1$Face_points <- gsub("shirt_points","",as.character(Cats1$Face_points))
Cats1 <- subset(Cats1, select=-c(`-`))
Cats1$Face_points <- gsub("\\r\\n", "", Cats1$Face_points)
table(Cats1$Face_points)
#shirt_points
Cats1$Shirt_points <- Cats1$Attributes
Cats1 <- Cats1 %>% separate(Shirt_points, c('Shirt_points', 'Other'),  sep = "shirt_points ")
Cats1 <- Cats1 %>% separate(Other, c('-', 'Shirt_points'),  sep = '–')
Cats1$Shirt_points[is.na(Cats1$Shirt_points)] = 'No'
Cats1$Shirt_points <- gsub("body","",as.character(Cats1$Shirt_points))
Cats1 <- subset(Cats1, select=-c(`-`))
Cats1$Shirt_points <- gsub("\\r\\n", "", Cats1$Shirt_points)
table(Cats1$Shirt_points)
#body <- same
Cats1$Body <- Cats1$Attributes
Cats1 <- Cats1 %>% separate(Body, c('Body', 'Other'),  sep = "body ")
Cats1 <- Cats1 %>% separate(Other, c('-', 'Body'),  sep = '–')
Cats1$Body[is.na(Cats1$Body)] = 'No'
Cats1$Body <- gsub("face","",as.character(Cats1$Body))
Cats1 <- subset(Cats1, select=-c(`-`))
Cats1$Body <- gsub("\\r\\n", "", Cats1$Body)
table(Cats1$Body)
#face
Cats1$Face <- Cats1$Attributes
Cats1 <- Cats1 %>% separate(Face, c('Face', 'Other'),  sep = "face ")
Cats1 <- Cats1 %>% separate(Other, c('-', 'Face'),  sep = '–')
Cats1$Face[is.na(Cats1$Face)] = 'No'
Cats1$Face <- gsub("shirt","",as.character(Cats1$Face))
Cats1 <- subset(Cats1, select=-c(`-`))
Cats1$Face <- gsub("\\r\\n", "", Cats1$Face)
table(Cats1$Face)
#shirt
Cats1$Shirt <- Cats1$Attributes
Cats1 <- Cats1 %>% separate(Shirt, c('Shirt', 'Other'),  sep = "shirt ")
Cats1 <- Cats1 %>% separate(Other, c('-', 'Shirt'),  sep = '–')
Cats1$Shirt[is.na(Cats1$Shirt)] = 'No'
Cats1$Shirt <- gsub("tier","",as.character(Cats1$Shirt))
Cats1 <- subset(Cats1, select=-c(`-`))
Cats1$Shirt <- gsub("\\r\\n", "", Cats1$Shirt)
table(Cats1$Shirt)
#tier
Cats1$Tier <- Cats1$Attributes
Cats1 <- Cats1 %>% separate(Tier, c('Tier', 'Other'),  sep = "tier ")
Cats1 <- Cats1 %>% separate(Other, c('-', 'Tier'),  sep = '–')
Cats1$Tier[is.na(Cats1$Tier)] = 'No'
Cats1$Tier <- gsub("face_points","",as.character(Cats1$Tier))
Cats1 <- subset(Cats1, select=-c(`-`))
Cats1$Tier <- gsub("\\r\\n", "", Cats1$Tier)
table(Cats1$Tier)

############################################Colnames
names(Cats1)
Cats1 <- subset(Cats1, select=-c(Attributes, Body, Body_points))
colnames(Cats1) = c('Project', 'Album', 'Id', 'First_price', 'Last_price', 'Owners', 'Accumulated_sales', 'Sold_count', 
                    'Transactions', 'Date', "Total_points", "Hats","Hats_points", "Shirt_points","Face_points", 'Face', 
                    'Shirt', 'Tier')

############################################Quaters and Hour
Cats2 <- Cats1
Cats2 <- Cats2 %>% separate(Date, c('Date', 'Time'),  sep = ",")
Cats2 <- Cats2 %>% separate(Time, c('Hour', 'Del1', 'Del2'),  sep = ":")
Cats2 <- Cats2 %>% separate(Date, c('Del3', 'Month', 'Year'),  sep = "/")
Cats2 <- subset(Cats2, select = -c(Del1, Del2, Del3))

#Совместим период и год
Cats2$Monthly <- paste(Cats2$Year, Cats2$Month)
#Удалим вспомогательные столбцы
Cats2 <- subset(Cats2, select = c(-Year, -Month))
#Проверим, что получилось
d <-  as.data.frame(table(Cats2$Monthly))
d

############################################Отсчётный год

table(Cats2$Monthly)
Cats2$Monthly <- factor(Cats2$Monthly, levels = c('2021 09', '2021 07', '2021 08', '2022 02'))

####Index Cool####

#Модель
#Гипотеза: час продажи <- влияет. Инд. характеристики влияют.
#Оставим как везде
View(Cats_mod)
Cats_mod <- subset(Cats2, select = -c(Project, Album, Id, Transactions, 
                                      Total_points, Hats_points, Shirt_points,
                                      Face_points))
Cats_mod <- subset(Cats_mod, Last_price!="0")
Cats_mod <- subset(Cats_mod, First_price!="0")

mod_nft_h <- lm(Last_price ~ . ,data = Cats_mod)
summary(mod_nft_h)
vif(mod_nft_h)

#Чересчур большой R2, особенно если идти дальше. Повытаскиваем переменные
#Надо поудалять
mod_new1 <- update(mod_nft_h, . ~ . - Hats - Face - Shirt - Tier)
mod_new2 <- update(mod_nft_h, . ~ . - Hats - Face - Shirt - Tier -
                       Owners - Sold_Count)
#R2 вообще не уменьшается
mod_new3 <- update(mod_nft_h, . ~ . - Monthly - Accumulated_sales - First_price)
#R2 вообще не уменьшается
#После удаления Monthly упал до 0.3
mod_new4 <- update(mod_nft_h, . ~ . - Monthly)
summary(mod_new1)
#Оч. значительно влияет Monthly. До 0.44
#Это обязательная переменная. Поэтому оставм всё.

#Удалим переменные с большшой мульткол.
mod_nft_h1 <- update(mod_nft_h, . ~ . - Tier)
vif(mod_nft_h1)
summary(mod_nft_h1)
#Теперь всё хорошо

plot(mod_nft_h1, 1)
#Линии не совпадают
plot(mod_nft_h1, 2)
#от -10 до + 10
plot(mod_nft_h1, 3)

#________________________________________
#Нормальность остатков
boxCox(mod_nft_h1)
#Нужен логарифм
mod_nft_h2 <- update(mod_nft_h1, log(Last_price) ~ . )
summary(mod_nft_h2)
#0.93 R2
plot(mod_nft_h2, 2)
#от -15 до 5, но осн. часть от -5 до 5
#не стало лучше
mod_nft_h2

#________________________________________
#Спецификация
plot(mod_nft_h2, which = 1) 
resettest(mod_nft_h2)
crPlots(mod_nft_h2)

#First_price
mod_nft_h3 <- update(mod_nft_h2, . ~ . + log(First_price) - First_price)
summary(mod_nft_h3)

#________________________________________
mod_nft_h4 <- stepAIC(mod_nft_h3)
summary(mod_nft_h4)
#Видимо, от визуальных хар-к не зависим

#________________________________________
#Гетероскедастичность - одинакова ли станд.ошибка
#plot(mod_nft_h2, which = 3)
#Тест Бреуша-Пагана
bptest(mod_nft_h4)
#H0 - гетероскедастичности нет при p-value большом выполняется
#Гетеро <- есть (неоднородность остатков)
#Перейдём к робастным ошибкам в форме Вайта
library(sandwich)
cov_white <- vcovHC(mod_nft_h4, type = "HC0")
coeftest(mod_nft_h4, cov_white)
#Стало много незначимого

#________________________________________

mod_NFT_Cats<- mod_nft_h4
summary(mod_NFT_Cats, vcov = cov_white)

NFT_Cats <- coef(mod_NFT_Cats,  vcov = cov_white)
View(NFT_Cats)
NFT_Cats1 <- NFT_Cats[86:90]

####NFT Index####

#Список
NFT_Cats1
NFT_Meebits1
NFT_Kong1
NFT_Crypto1
NFT_Bored1
NFT_Superare1
NFT_Kitty1
BlockArt

#Скачаем наш All_NFT index
#Как делали?
#Сделали общий Excel со всеми NFT-коэф. Усреднили.
#All_NFT_index 
all_NFT <- read_excel("Desktop/Diploma/Данные/Workings/All indiices.xlsx", sheet = 'Sheet2')
View(all_NFT)

#Посмотрим на график
graph <- all_NFT
str(graph)
graph$Monthly <- factor(graph$Monthly)
graph$ALL_NFT <- as.numeric(graph$ALL_NFT)

v <- ggplot(graph, aes(x=Monthly, y=ALL_NFT, group=1)) + 
  geom_line(colour="dodgerblue2") + 
  geom_point(shape=21, size=3, fill="white") + 
  theme(axis.text = element_text(angle = 90, color="black", size=10, face=3)) +
  geom_smooth(method=lm, se=FALSE, col='darkred', size=0.5)
ggplotly(v)
#Процентный рост
(-0.81+7.25)*100
#Рост рынка 644%


#Графики

dygraph(returns, main = "Market vs. Art vs. NFT") %>%
  dyAxis("y", label = "Return", valueRange = c(-1,0.5)) %>%
  dyRangeSelector(dateWindow = c("2018-01-31", "2022-02-28")) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set2")) 


#По-хорошему, надо было 1 общий месяц взять для сравнения (2019 01)
#Значит надо поменять отсчётный год

#Через change rownames
#ПоменТь местами строки внутри модели





####NFT graphs####
# libraries
library(packcircles)
library(ggplot2)

names(DataNFT)

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


