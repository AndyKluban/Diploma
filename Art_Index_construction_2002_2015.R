####Гедонистическая регрессия####

#НЕ повторные продажи
#1) Новые переменные:
#estimate price <-  среднее между нижней и верхней
#living status <-  сравнить момент смерти и год продажи (через if) <-  только если для умерших между 2002 и 2015
#death effect <- количество прожитых лет ВАЖНО!!! <-  проверяем гипотезу одного из исследований
#reputation <- Создаётся индекс репутации:
##Надо сделать столбец с годом = периодом
##group.by, summarise_all, функция среднего геометрического
##coef(best_model_1) * на среднее значение для каждого художника
#2) Столбец с кварталом продажи. Все годы надо включить в регрессию для получения коэффициентов
#3) Коэффициенты перед кварталом продажи <- наши значения для индекса. Выделяем коэффициенты в отдельный датафрейм. Имеем индекс.
#4) Создаём портфель

data <- Data16
View(data1)

############Сделаем статистику по наблюдениям
data1 <- subset(data, select=-c(Dating, Die, Estimate, Country_of_birth,
                                Stamped, Inscribed, Bill_Invoice))
sum(is.na(data$Estimate))
sum(is.na(data$Country_of_birth))
#Слишком много, уберём.
sum(is.na(data1))
data1 <- na.omit(data1)

datast <- subset(data1, select=-c(Artist, Title, Transcation_Day))
str(datast)
datast$Lot_Number <- as.numeric(datast$Lot_Number)
datast$Born <- as.numeric(datast$Born)
datast$Alive <- as.character(datast$Alive)
datast$Medium <- ifelse(datast[,1] == 'Oil',  'Oil',
                     ifelse(datast[,1] == 'Etching',  'Etching', 
                            ifelse(datast[,1] == 'Ink',  'Ink','Other')))
datast$Currency <- ifelse(datast[,5] == 'USD',  'USD',
                        ifelse(datast[,5] == 'EUR',  'EUR', 
                               ifelse(datast[,5] == 'GBP',  'GBP','Other')))

datast <- na.omit(datast)
sum(is.na(datast))
st(datast)

#332,679
#До ввода новых переменных сделаем заготовку для портфеля.
View(data1)
#Имеем 249,363

#Отделим Период продажи от даты
data1$Year <- data1$Transcation_Day
data2 <- data1 %>% separate(Year, c('Year', 'Mon', 'Day'),  sep = "-")
#Переименовать месяцы в кварталы

Table <- table(data2$Mon)
a <- names(Table)
names(data2)
View(data2)
data2[,14]
#Quartals
data2[,14] <- ifelse(data2[,14] == a[1] | data2[,14] == a[2] | data2[,14] == a[3], 'Q1',
                     ifelse(data2[,14] == a[4] | data2[,14] == a[5] | data2[,14] == a[6], 'Q2',
                            ifelse(data2[,14] == a[7] | data2[,14] == a[8] | data2[,14] == a[9], 'Q3', 'Q4')))

#Совместим период и год
data2$Quater <- paste(data2$Year, data2$Mon)
#Удалим вспомогательные столбцы
data2 <- subset(data2, select = c(-Year, -Mon, -Day))
#Проверим, что получилось
d <-  as.data.frame(table(data2$Quater))
View(d)

#Оставим только Q1 2004 до Q2 2015
data3 <- subset(data2, Quater!="2000 Q1" & Quater!="2001 Q4" & Quater!="2002 Q1" & Quater!="2002 Q2" & Quater!="2019 Q1" &
                  Quater!="2019 Q2" & Quater!="2019 Q3" & Quater!="2019 Q4" & Quater!="2019 Q4" &
                  Quater!="2020 Q1" & Quater!="2020 Q2" & Quater!="2020 Q3" &
                  Quater!="2020 Q4" & Quater!="2021 Q1" & Quater!="2021 Q2" & Quater!="2021 Q3" &
                  Quater!="2021 Q4")
View(table(data3$Quater))
data3$Quater <- factor(data3$Quater, levels = c('2004 Q2', '2002 Q4', 
                                                '2003 Q1', '2003 Q2', '2003 Q3', '2003 Q4',
                                                '2004 Q1', '2004 Q3', '2004 Q4',
                                                '2005 Q1', '2005 Q2', '2005 Q3', '2005 Q4',
                                                '2006 Q1', '2006 Q2', '2006 Q3', '2006 Q4',
                                                '2007 Q1', '2007 Q2', '2007 Q3', '2007 Q4',
                                                '2008 Q1', '2008 Q2', '2008 Q3', '2008 Q4',
                                                '2009 Q1', '2009 Q2', '2009 Q3', '2009 Q4',
                                                '2010 Q1', '2010 Q2', '2010 Q3', '2010 Q4',
                                                '2011 Q1', '2011 Q2', '2011 Q3', '2011 Q4',
                                                '2012 Q1', '2012 Q2', '2012 Q3', '2012 Q4',
                                                '2013 Q1', '2013 Q2', '2013 Q3', '2013 Q4',
                                                '2014 Q1', '2014 Q2', '2014 Q3', '2014 Q4',
                                                '2015 Q1', '2015 Q2', '2015 Q3'))

####Correlation####
names(data3)
hed_mod1 <- na.omit(data3)
summary(hed_mod1)
#Проверка
names(hed_mod1)

#Price>0
hed_mod1<-subset(hed_mod1, Price!="0")

#For correlation
View(hed_mod1)
D1_corr1 <- subset(hed_mod1, select = -c(Artist, Country_of_birth, Title, Transcation_Day,
                                         Currency, Quater))

str(D1_corr1)
D1_corr1$Auction_house <- class.ind(D1_corr1$Auction_house)
D1_corr1$Signed <- class.ind(D1_corr1$Signed)
D1_corr1$Medium <- class.ind(D1_corr1$Medium)
D1_corr1$Lot_Number <- as.numeric(D1_corr1$Lot_Number)
D1_corr1$Born <- as.numeric(D1_corr1$Born)
D1_corr1 <- na.omit(D1_corr1)
corr <- corrplot(cor(D1_corr1))

corrplot(cor(D1_corr1), type="upper", order="hclust", tl.col="black", method="pie")


####Art Index 2004-2015####
data <- subset(hed_mod1, select = -c(Artist, Title, Transcation_Day))
data$Currency <- ifelse(data[,5] == 'USD',  'USD',
                          ifelse(data[,5] == 'EUR',  'EUR', 
                                 ifelse(data[,5] == 'GBP',  'GBP','Other')))
str(data)
data$Lot_Number <- as.numeric(data$Lot_Number)
data$Born <- as.numeric(data$Born)
str(data)
data <- na.omit(data)

#Модель
mod_h <- lm(Price ~ . ,data = data)
summary(mod_h)
v <- vif(mod_h)
stargazer(v, type = 'html', df = T, 
          digits =2, title = 'vif1',
          out = 'vif1.html')

#Мультикол. нет

plot(mod_h, 1)
plot(mod_h, 2)
#остатки не нормальны
plot(mod_h, 3)
#Прям как будто нужны квадраты

#________________________________________
#Нормальность остатков
#Не нормальны
boxCox(mod_h)
#Нужен логарифм вроде
mod_h2 <- update(mod_h, log(Price) ~ . )
summary(mod_h2)
#Cразу вырос R2
boxCox(mod_h2)
#Стало значительно лучше. Даже странно. Но в некоторых работах по рынку можно найти схожий R2

#________________________________________
#Спецификация
plot(mod_h2, which = 1) 
#Какая-то жесть
resettest(mod_h2)
#p-value оч маленький, а значит не норм 
#crPlots(model2)
#Можем исправить Lot_Number
resettest(mod_h2, power = 2)
#p-value большой, а значит  норм 
resettest(mod_h2, power = 3)
#p-value мал, а значит не норм 

#________________________________________
#Гетероскедастичность - одинакова ли станд.ошибка
#plot(mod4, which = 3)
#Тест Бреуша-Пагана
library(lmtest)
bptest(mod_h2)
#H0 - гетероскедастичности нет при p-value большом выполняется
#Гетеро <- есть (неоднородность остатков)
#Перейдём к робастным ошибкам в форме Вайта
cov_white <- vcovHC(mod_h2, type = "HC0")
coeftest(mod_h2, cov_white)


#Ничего не изменилось
best_model_2 <- mod_h2
summary(best_model_2, vcov = 'HC0')

stargazer(best_model_2, type = 'html', df = T, 
          digits =2, title = 'vif1',
          out = 'Mod1.html', vcov = 'HC0')

#Год для сравнения <-  Q1 2004 (так как много наблюдений с этого момента)
p <- coef(best_model_2)
View(p)
#write.xlsx(p, 'p5.xlsx')
ArtIndex <- read_excel("Desktop/Diploma/Данные/Workings/p5.xlsx")
View(ArtIndex)

library(stargazer)
stargazer(best_model_2, type = 'text', df = FALSE, 
          digits =2, title = '.',
          out = 'Art2.txt', vcov = 'HC0')

