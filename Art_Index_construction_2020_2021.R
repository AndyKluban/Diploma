####Гедонистическая регрессия####

art <- Data16
art1 <- subset(art, select=-c(Dating, Die))
art1 <- na.omit(art1)

#До ввода новых переменных сделаем заготовку для портфеля.
#202,363

############################################Monthly
#Отделим Период продажи от даты
art1$Year <- art1$Transcation_Day
art2 <- art1 %>% separate(Year, c('Year', 'Mon', 'Day'),  sep = "-")
#Переименовать месяцы в кварталы
Table <- table(art2$Mon)
a <- names(Table)
names(art2)
#Monthly <- cовместим период и год
art2$Monthly <- paste(art2$Year, art2$Mon)
d <-  as.data.frame(table(art2$Monthly))

#Удалим вспомогательные столбцы
art2 <- subset(art2, select = c(-Year, -Mon, -Day))

##Берём с 2020 01 по 2021 09
art3 <- subset(art2, Monthly =="2020 01" | Monthly=="2020 02" | Monthly=="2020 03" | Monthly=="2020 04" |
                 Monthly=="2020 05" | Monthly=="2020 06" | Monthly=="2020 07" | Monthly=="2020 08" | 
                 Monthly=="2020 09" | Monthly=="2020 10" | Monthly=="2020 11" | Monthly=="2020 12" | 
                 Monthly=="2021 01" | Monthly=="2021 02" | Monthly=="2021 03" | Monthly=="2021 04" |
                 Monthly=="2021 05" | Monthly=="2021 06" | Monthly=="2021 07" | Monthly=="2021 08" | 
                 Monthly=="2021 09")

############################################Отсчётный год
table(art3$Monthly)
art3$Monthly <- factor(art3$Monthly, levels = c('2021 09', '2020 01', '2020 02', '2020 03', '2020 04', '2020 05', '2020 06', '2020 07',
                                                    '2020 08', '2020 09', '2020 10', '2020 11', '2020 12', '2021 01', '2021 02',
                                                    '2021 03', '2021 04', '2021 05','2021 06', '2021 07', '2021 08'))
View(art3)
####Art Index 2020-2021####

#Модель
art3 <- subset(art3, Price!="0")
art3 <- subset(art3, Estimate!="0")

names(art3)
str(art3)
art3$Lot_Number <- as.numeric(art3$Lot_Number)
art3 <- na.omit(art3)
View(art3)
mod_2020_art <- lm(Price ~ . - Artist - Title -Transcation_Day, data = art3)

summary(mod_2020_art)
vif(mod_2020_art)

plot(mod_2020_art, 1)
plot(mod_2020_art, 2)
#от 0 до +10
plot(mod_2020_art, 3)
#Прям как будто нужны квадраты

#________________________________________
#Нормальность остатков

boxCox(mod_2020_art)
#Нужен логарифм вроде
mod2_2020_art <- update(mod_2020_art, log(Price) ~ . )
boxCox(mod2_2020_art)
summary(mod2_2020_art)
plot(mod2_2020_art, 2)
#Прям на линии
mod2_2020_art

#________________________________________
mod3_2020_art <-mod2_2020_art
#Спецификация
plot(mod3_2020_art, which = 1) 
#Какая-то жесть
v <- resettest(mod3_2020_art)

resettest(mod3_2020_art, power = 2)
#p-value мал, а значит не норм 
resettest(mod3_2020_art, power = 3)
#p-value мал, а значит не норм 

#p-value оч маленький, а значит не норм 
crPlots(mod3_2020_art)

#Estimate
mod4_2020_art <- update(mod3_2020_art, . ~ . + log(Estimate) - Estimate)
summary(mod4_2020_art)
vif(mod4_2020_art)
#Мне не нравится такой R2. Уберу Estimate
mod4_2020_art <- update(mod3_2020_art, .~. -Estimate)
summary(mod4_2020_art)
plot(mod4_2020_art, 1)

#Lot number
mod4_2020_art <- update(mod4_2020_art, . ~ . + I(Lot_Number^2) - Lot_Number)
resettest(mod4_2020_art, power = 2)
summary(mod4_2020_art)
plot(mod4_2020_art, which = 1) 
#В остальном норм

#________________________________________
#Выберем по AIC
mod5_2020_art <- stepAIC(mod4_2020_art)
summary(mod5_2020_art)


#________________________________________
#Гетероскедастичность - одинакова ли станд.ошибка
plot(mod5_2020_art, which = 3)
#Тест Бреуша-Пагана
library(lmtest)
bptest(mod5_2020_art)
#H0 - гетероскедастичности нет при p-value большом выполняется
#Гетеро <- есть (неоднородность остатков)
#Перейдём к робастным ошибкам в форме Вайта
cov_white <- vcovHC(mod5_2020_art, type = "HC0")
coeftest(mod5_2020_art, cov_white)
#Ничего не изменилось. Значимость где-то даже упала.

mod_Art_2021 <- update(mod5_2020_art, . ~ . - Born - Bill_invoice - Estimate)

summary(mod_Art_2021, vcov = cov_white)
stargazer(mod_Art_2021, type = 'html', df = FALSE, 
          digits =2, title = 'Mod2',
          out = 'Mod2.html', vcov = 'HC0')

#R2 прям сильно лучше стал
Art_2021 <- coef(mod_Art_2021,  vcov = cov_white)

#Год для сравнения <-  Q1 2004 (так как много наблюдений с этого момента)
For_Art <- coef(mod_Art_2021,  vcov = cov_white)
View(For_Art)
#write.xlsx(For_Art, 'For_Art.xlsx')
ArtIndex2021 <- For_Art[62:81]
View(ArtIndex2021)

all_Art <- read_excel("Desktop/Diploma/Данные/Workings/all_Art.xlsx", sheet = "ART_Graph")

#График
graph <- all_Art
str(graph)

v <- ggplot(graph, aes(x=Monthly, y=ALL_Art, group=1)) + 
  geom_line(colour="dodgerblue2") + 
  geom_point(shape=21, size=3, fill="white") + 
  theme(axis.text = element_text(angle = 90, color="black", size=10, face=3)) +
  geom_smooth(method=lm, se=FALSE, col='darkred', size=0.5)
ggplotly(v)

v <- ggplot(graph, aes(x=Monthly, y=ALL_Art, group=1)) + 
  xlab("Месяц") + ylab("Доллар США ($)") +
  geom_text(x=2, y=120, label="Вложения = $112") +
  geom_text(x=17, y=220, label="Вложения = $212.5") +
  geom_line(colour="dodgerblue2") + 
  geom_point(shape=21, size=3, fill="white") + 
  geom_smooth(method=lm, se=FALSE, col='darkred', size=0.5) + theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplotly(v)



