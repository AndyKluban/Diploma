#Модель

library("AER")
library("sandwich")
library("lmtest")
library("car")
library("ggplot2")
#library('ExPanDaR')
library('dplyr')
library('ggplot')
library('MASS')
library('plm')
library('dplyr')
library('data.table')
library('nnet')


#Графики
table(Data16$Medium)

Data16$Born <- as.numeric(Data16$Born)
Data16$Die <- as.numeric(Data16$Die)
Data16$Lot_Number <- as.numeric(Data16$Lot_Number)
Data16$Dating <- as.numeric(Data16$Dating)
str(D_1)

#Когда были совершены основные продажи
Data_Graph <- Data16[complete.cases(Data16[ , 12]),]
Data_Graph <- filter(Data_Graph, Transcation_Day>= as.Date('2004-01-01'))
Data_Graph <- filter(Data_Graph, Transcation_Day<= as.Date('2015-01-01'))
ggplot(data = Data_Graph, aes(x = Transcation_Day)) + 
  geom_bar(fill = 'lightblue', color = "lightblue") + 
  theme_bw()+labs(x = "Дата", y = "Количество транзакций")

#Использование основных материалов, зависимость от цены
#BoxPlot / GeomViolin
Table1 <- table(Data16$Medium)
z <- names(Table1)[Table1>10000]
Data_Graph2 <- filter(Data16, Medium %in% z)

ggplot(Data_Graph2, aes(x=Price, y=Medium)) + 
  labs(x = "Price", y = 'Medium') + 
  geom_segment(aes(yend=Medium), xend=0, colour="grey50") + 
  geom_point(size=2, aes(colour = Medium)) +
  scale_colour_brewer(palette="Set1", guide=FALSE) + 
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(Medium ~ ., scales="free_y", space="free_y")+ 
  theme(text = element_text(size=8))

#Художники, и их общие сборы
Data_Graph3 <- Data16[complete.cases(Data16[ , 16]),]
dp1_new_2 <- data.frame(Artist = Data_Graph3$Artist, Sales = Data_Graph3$Price)
dp1_sum <- dp1_new_2 %>% group_by(Artist) %>% summarise_all(sum)
View(dp1_sum)
#Сделать соответствие по странам и тп



####Повторные продажи####

#Точно одинаковые картины. Сверяем размер.

#В G все должны быть нули. Если нули не все, то нахер идёт
#У нас есть все размерности. Если мы из первой вычтем все, а потом разности сложим,
#то хотим ноль. 
#Решение: можно расписать G для каждого. Но тогда надо сделать отдельно для каждого набора.
#А потом объединить. Попроьуем для пяти

#Убирать тогда надо не по названию, а по индивидуальному номеру

D <- Data16
D$row_num <- seq.int(nrow(D)) 
D$row_num <- as.character(D$row_num)

#2
t <- as.data.frame(table(D$Title))
t <- filter(t, Freq > 1)
t <- filter(t, Freq < 3)
rt <- NULL
for(i in 1:length(t$Var1)){
  cat('number #', i,'\n' )
  rt<-rbind(rt, filter(D, Title==t$Var1[i]))
}

rt2 <- rt
for(i in 1:nrow(rt)){
  cat('#', i,'\n' )
  A <- filter(rt, Title==rt[i,3])
  G2 <- A$cm_wide[1] - A$cm_wide[2]==0
  rt2 <- if(G2==FALSE) filter(rt2, row_num!=A$row_num[2]) else rt2}

g <- as.data.frame(table(rt2$Title))
g <- filter(g, Freq > 1)
g <- filter(g, Freq < 11)
gt <- NULL
for(i in 1:length(g$Var1)){
  cat('number #', i,'\n' )
  gt<-rbind(gt, filter(rt2, Title==g$Var1[i]))
}
repeated_freq2 <- gt

#3
t <- as.data.frame(table(D$Title))
t <- filter(t, Freq > 2)
t <- filter(t, Freq < 4)
rt <- NULL
for(i in 1:length(t$Var1)){
  cat('number #', i,'\n' )
  rt<-rbind(rt, filter(D, Title==t$Var1[i]))
}

rt2 <- rt
for(i in 1:nrow(rt)){
  cat('#', i,'\n' )
  A <- filter(rt, Title==rt[i,3])
  G2 <- A$cm_wide[1] - A$cm_wide[2]==0
  G3 <- A$cm_wide[1] - A$cm_wide[3]==0
  rt2 <- if(G2==FALSE) filter(rt2, row_num!=A$row_num[2]) else rt2
  rt2 <- if(G3==FALSE) filter(rt2, row_num!=A$row_num[3]) else rt2}

g <- as.data.frame(table(rt2$Title))
g <- filter(g, Freq > 1)
g <- filter(g, Freq < 11)
gt <- NULL
for(i in 1:length(g$Var1)){
  cat('number #', i,'\n' )
  gt<-rbind(gt, filter(rt2, Title==g$Var1[i]))
}
repeated_freq3 <- gt

#4
t <- as.data.frame(table(D$Title))
t <- filter(t, Freq > 3)
t <- filter(t, Freq < 5)
rt <- NULL
for(i in 1:length(t$Var1)){
  cat('number #', i,'\n' )
  rt<-rbind(rt, filter(D, Title==t$Var1[i]))
}

rt2 <- rt
for(i in 1:nrow(rt)){
  cat('#', i,'\n' )
  A <- filter(rt, Title==rt[i,3])
  G2 <- A$cm_wide[1] - A$cm_wide[2]==0
  G3 <- A$cm_wide[1] - A$cm_wide[3]==0
  G4 <- A$cm_wide[1] - A$cm_wide[4]==0
  rt2 <- if(G2==FALSE) filter(rt2, row_num!=A$row_num[2]) else rt2
  rt2 <- if(G3==FALSE) filter(rt2, row_num!=A$row_num[3]) else rt2
  rt2 <- if(G4==FALSE) filter(rt2, row_num!=A$row_num[4]) else rt2}

g <- as.data.frame(table(rt2$Title))
g <- filter(g, Freq > 1)
g <- filter(g, Freq < 11)
gt <- NULL
for(i in 1:length(g$Var1)){
  cat('number #', i,'\n' )
  gt<-rbind(gt, filter(rt2, Title==g$Var1[i]))
}
repeated_freq4 <- gt

#5
t <- as.data.frame(table(D$Title))
t <- filter(t, Freq > 4)
t <- filter(t, Freq < 6)
rt <- NULL
for(i in 1:length(t$Var1)){
  cat('number #', i,'\n' )
  rt<-rbind(rt, filter(D, Title==t$Var1[i]))
}

rt2 <- rt
for(i in 1:nrow(rt)){
  cat('#', i,'\n' )
  A <- filter(rt, Title==rt[i,3])
  G2 <- A$cm_wide[1] - A$cm_wide[2]==0
  G3 <- A$cm_wide[1] - A$cm_wide[3]==0
  G4 <- A$cm_wide[1] - A$cm_wide[4]==0
  G5 <- A$cm_wide[1] - A$cm_wide[5]==0
  rt2 <- if(G2==FALSE) filter(rt2, row_num!=A$row_num[2]) else rt2
  rt2 <- if(G3==FALSE) filter(rt2, row_num!=A$row_num[3]) else rt2
  rt2 <- if(G4==FALSE) filter(rt2, row_num!=A$row_num[4]) else rt2
  rt2 <- if(G5==FALSE) filter(rt2, row_num!=A$row_num[5]) else rt2}

g <- as.data.frame(table(rt2$Title))
g <- filter(g, Freq > 1)
g <- filter(g, Freq < 11)
gt <- NULL
for(i in 1:length(g$Var1)){
  cat('number #', i,'\n' )
  gt<-rbind(gt, filter(rt2, Title==g$Var1[i]))
}
repeated_freq5 <- gt

#6
t <- as.data.frame(table(D$Title))
t <- filter(t, Freq > 5)
t <- filter(t, Freq < 7)
rt <- NULL
for(i in 1:length(t$Var1)){
  cat('number #', i,'\n' )
  rt<-rbind(rt, filter(D, Title==t$Var1[i]))
}

rt2 <- rt
for(i in 1:nrow(rt)){
  cat('#', i,'\n' )
  A <- filter(rt, Title==rt[i,3])
  G2 <- A$cm_wide[1] - A$cm_wide[2]==0
  G3 <- A$cm_wide[1] - A$cm_wide[3]==0
  G4 <- A$cm_wide[1] - A$cm_wide[4]==0
  G5 <- A$cm_wide[1] - A$cm_wide[5]==0
  G6 <- A$cm_wide[1] - A$cm_wide[6]==0
  rt2 <- if(G2==FALSE) filter(rt2, row_num!=A$row_num[2]) else rt2
  rt2 <- if(G3==FALSE) filter(rt2, row_num!=A$row_num[3]) else rt2
  rt2 <- if(G4==FALSE) filter(rt2, row_num!=A$row_num[4]) else rt2
  rt2 <- if(G5==FALSE) filter(rt2, row_num!=A$row_num[5]) else rt2
  rt2 <- if(G6==FALSE) filter(rt2, row_num!=A$row_num[6]) else rt2}

g <- as.data.frame(table(rt2$Title))
g <- filter(g, Freq > 1)
g <- filter(g, Freq < 11)
gt <- NULL
for(i in 1:length(g$Var1)){
  cat('number #', i,'\n' )
  gt<-rbind(gt, filter(rt2, Title==g$Var1[i]))
}
repeated_freq6 <- gt

#7
t <- as.data.frame(table(D$Title))
t <- filter(t, Freq > 6)
t <- filter(t, Freq < 8)
rt <- NULL
for(i in 1:length(t$Var1)){
  cat('number #', i,'\n' )
  rt<-rbind(rt, filter(D, Title==t$Var1[i]))
}

rt2 <- rt
for(i in 1:nrow(rt)){
  cat('#', i,'\n' )
  A <- filter(rt, Title==rt[i,3])
  G2 <- A$cm_wide[1] - A$cm_wide[2]==0
  G3 <- A$cm_wide[1] - A$cm_wide[3]==0
  G4 <- A$cm_wide[1] - A$cm_wide[4]==0
  G5 <- A$cm_wide[1] - A$cm_wide[5]==0
  G6 <- A$cm_wide[1] - A$cm_wide[6]==0
  G7 <- A$cm_wide[1] - A$cm_wide[7]==0
  rt2 <- if(G2==FALSE) filter(rt2, row_num!=A$row_num[2]) else rt2
  rt2 <- if(G3==FALSE) filter(rt2, row_num!=A$row_num[3]) else rt2
  rt2 <- if(G4==FALSE) filter(rt2, row_num!=A$row_num[4]) else rt2
  rt2 <- if(G5==FALSE) filter(rt2, row_num!=A$row_num[5]) else rt2
  rt2 <- if(G6==FALSE) filter(rt2, row_num!=A$row_num[6]) else rt2
  rt2 <- if(G7==FALSE) filter(rt2, row_num!=A$row_num[7]) else rt2}

g <- as.data.frame(table(rt2$Title))
g <- filter(g, Freq > 1)
g <- filter(g, Freq < 11)
gt <- NULL
for(i in 1:length(g$Var1)){
  cat('number #', i,'\n' )
  gt<-rbind(gt, filter(rt2, Title==g$Var1[i]))
}
repeated_freq7 <- gt

#8
t <- as.data.frame(table(D$Title))
t <- filter(t, Freq > 7)
t <- filter(t, Freq < 9)
rt <- NULL
for(i in 1:length(t$Var1)){
  cat('number #', i,'\n' )
  rt<-rbind(rt, filter(D, Title==t$Var1[i]))
}

rt2 <- rt
for(i in 1:nrow(rt)){
  cat('#', i,'\n' )
  A <- filter(rt, Title==rt[i,3])
  G2 <- A$cm_wide[1] - A$cm_wide[2]==0
  G3 <- A$cm_wide[1] - A$cm_wide[3]==0
  G4 <- A$cm_wide[1] - A$cm_wide[4]==0
  G5 <- A$cm_wide[1] - A$cm_wide[5]==0
  G6 <- A$cm_wide[1] - A$cm_wide[6]==0
  G7 <- A$cm_wide[1] - A$cm_wide[7]==0
  G8 <- A$cm_wide[1] - A$cm_wide[8]==0
  rt2 <- if(G2==FALSE) filter(rt2, row_num!=A$row_num[2]) else rt2
  rt2 <- if(G3==FALSE) filter(rt2, row_num!=A$row_num[3]) else rt2
  rt2 <- if(G4==FALSE) filter(rt2, row_num!=A$row_num[4]) else rt2
  rt2 <- if(G5==FALSE) filter(rt2, row_num!=A$row_num[5]) else rt2
  rt2 <- if(G6==FALSE) filter(rt2, row_num!=A$row_num[6]) else rt2
  rt2 <- if(G7==FALSE) filter(rt2, row_num!=A$row_num[7]) else rt2
  rt2 <- if(G8==FALSE) filter(rt2, row_num!=A$row_num[8]) else rt2}

g <- as.data.frame(table(rt2$Title))
g <- filter(g, Freq > 1)
g <- filter(g, Freq < 11)
gt <- NULL
for(i in 1:length(g$Var1)){
  cat('number #', i,'\n' )
  gt<-rbind(gt, filter(rt2, Title==g$Var1[i]))
}
repeated_freq8 <- gt

#9
t <- as.data.frame(table(D$Title))
t <- filter(t, Freq > 8)
t <- filter(t, Freq < 10)
rt <- NULL
for(i in 1:length(t$Var1)){
  cat('number #', i,'\n' )
  rt<-rbind(rt, filter(D, Title==t$Var1[i]))
}

rt2 <- rt
for(i in 1:nrow(rt)){
  cat('#', i,'\n' )
  A <- filter(rt, Title==rt[i,3])
  G2 <- A$cm_wide[1] - A$cm_wide[2]==0
  G3 <- A$cm_wide[1] - A$cm_wide[3]==0
  G4 <- A$cm_wide[1] - A$cm_wide[4]==0
  G5 <- A$cm_wide[1] - A$cm_wide[5]==0
  G6 <- A$cm_wide[1] - A$cm_wide[6]==0
  G7 <- A$cm_wide[1] - A$cm_wide[7]==0
  G8 <- A$cm_wide[1] - A$cm_wide[8]==0
  G9 <- A$cm_wide[1] - A$cm_wide[9]==0
  rt2 <- if(G2==FALSE) filter(rt2, row_num!=A$row_num[2]) else rt2
  rt2 <- if(G3==FALSE) filter(rt2, row_num!=A$row_num[3]) else rt2
  rt2 <- if(G4==FALSE) filter(rt2, row_num!=A$row_num[4]) else rt2
  rt2 <- if(G5==FALSE) filter(rt2, row_num!=A$row_num[5]) else rt2
  rt2 <- if(G6==FALSE) filter(rt2, row_num!=A$row_num[6]) else rt2
  rt2 <- if(G7==FALSE) filter(rt2, row_num!=A$row_num[7]) else rt2
  rt2 <- if(G8==FALSE) filter(rt2, row_num!=A$row_num[8]) else rt2
  rt2 <- if(G9==FALSE) filter(rt2, row_num!=A$row_num[9]) else rt2}

g <- as.data.frame(table(rt2$Title))
g <- filter(g, Freq > 1)
g <- filter(g, Freq < 11)
gt <- NULL
for(i in 1:length(g$Var1)){
  cat('number #', i,'\n' )
  gt<-rbind(gt, filter(rt2, Title==g$Var1[i]))
}
repeated_freq9 <- gt

#10
t <- as.data.frame(table(D$Title))
t <- filter(t, Freq > 9)
t <- filter(t, Freq < 11)
rt <- NULL
for(i in 1:length(t$Var1)){
  cat('number #', i,'\n' )
  rt<-rbind(rt, filter(D, Title==t$Var1[i]))
}

rt2 <- rt
for(i in 1:nrow(rt)){
  cat('#', i,'\n' )
  A <- filter(rt, Title==rt[i,3])
  G2 <- A$cm_wide[1] - A$cm_wide[2]==0
  G3 <- A$cm_wide[1] - A$cm_wide[3]==0
  G4 <- A$cm_wide[1] - A$cm_wide[4]==0
  G5 <- A$cm_wide[1] - A$cm_wide[5]==0
  G6 <- A$cm_wide[1] - A$cm_wide[6]==0
  G7 <- A$cm_wide[1] - A$cm_wide[7]==0
  G8 <- A$cm_wide[1] - A$cm_wide[8]==0
  G9 <- A$cm_wide[1] - A$cm_wide[9]==0
  G10 <- A$cm_wide[1] - A$cm_wide[10]==0
  rt2 <- if(G2==FALSE) filter(rt2, row_num!=A$row_num[2]) else rt2
  rt2 <- if(G3==FALSE) filter(rt2, row_num!=A$row_num[3]) else rt2
  rt2 <- if(G4==FALSE) filter(rt2, row_num!=A$row_num[4]) else rt2
  rt2 <- if(G5==FALSE) filter(rt2, row_num!=A$row_num[5]) else rt2
  rt2 <- if(G6==FALSE) filter(rt2, row_num!=A$row_num[6]) else rt2
  rt2 <- if(G7==FALSE) filter(rt2, row_num!=A$row_num[7]) else rt2
  rt2 <- if(G8==FALSE) filter(rt2, row_num!=A$row_num[8]) else rt2
  rt2 <- if(G9==FALSE) filter(rt2, row_num!=A$row_num[9]) else rt2
  rt2 <- if(G10==FALSE) filter(rt2, row_num!=A$row_num[10]) else rt2}

g <- as.data.frame(table(rt2$Title))
g <- filter(g, Freq > 1)
g <- filter(g, Freq < 11)
gt <- NULL
for(i in 1:length(g$Var1)){
  cat('number #', i,'\n' )
  gt<-rbind(gt, filter(rt2, Title==g$Var1[i]))
}
repeated_freq10 <- gt

#11
t <- as.data.frame(table(D$Title))
t <- filter(t, Freq > 10)
t <- filter(t, Freq < 12)
rt <- NULL
for(i in 1:length(t$Var1)){
  cat('number #', i,'\n' )
  rt<-rbind(rt, filter(D, Title==t$Var1[i]))
}

rt2 <- rt
for(i in 1:nrow(rt)){
  cat('#', i,'\n' )
  A <- filter(rt, Title==rt[i,3])
  G2 <- A$cm_wide[1] - A$cm_wide[2]==0
  G3 <- A$cm_wide[1] - A$cm_wide[3]==0
  G4 <- A$cm_wide[1] - A$cm_wide[4]==0
  G5 <- A$cm_wide[1] - A$cm_wide[5]==0
  G6 <- A$cm_wide[1] - A$cm_wide[6]==0
  G7 <- A$cm_wide[1] - A$cm_wide[7]==0
  G8 <- A$cm_wide[1] - A$cm_wide[8]==0
  G9 <- A$cm_wide[1] - A$cm_wide[9]==0
  G10 <- A$cm_wide[1] - A$cm_wide[10]==0
  G11 <- A$cm_wide[1] - A$cm_wide[11]==0
  rt2 <- if(G2==FALSE) filter(rt2, row_num!=A$row_num[2]) else rt2
  rt2 <- if(G3==FALSE) filter(rt2, row_num!=A$row_num[3]) else rt2
  rt2 <- if(G4==FALSE) filter(rt2, row_num!=A$row_num[4]) else rt2
  rt2 <- if(G5==FALSE) filter(rt2, row_num!=A$row_num[5]) else rt2
  rt2 <- if(G6==FALSE) filter(rt2, row_num!=A$row_num[6]) else rt2
  rt2 <- if(G7==FALSE) filter(rt2, row_num!=A$row_num[7]) else rt2
  rt2 <- if(G8==FALSE) filter(rt2, row_num!=A$row_num[8]) else rt2
  rt2 <- if(G9==FALSE) filter(rt2, row_num!=A$row_num[9]) else rt2
  rt2 <- if(G10==FALSE) filter(rt2, row_num!=A$row_num[10]) else rt2
  rt2 <- if(G11==FALSE) filter(rt2, row_num!=A$row_num[11]) else rt2}

g <- as.data.frame(table(rt2$Title))
g <- filter(g, Freq > 1)
g <- filter(g, Freq < 16)
gt <- NULL
for(i in 1:length(g$Var1)){
  cat('number #', i,'\n' )
  gt<-rbind(gt, filter(rt2, Title==g$Var1[i]))
}
repeated_freq11 <- gt

#12
t <- as.data.frame(table(D$Title))
t <- filter(t, Freq > 11)
t <- filter(t, Freq < 13)
rt <- NULL
for(i in 1:length(t$Var1)){
  cat('number #', i,'\n' )
  rt<-rbind(rt, filter(D, Title==t$Var1[i]))
}

rt2 <- rt
for(i in 1:nrow(rt)){
  cat('#', i,'\n' )
  A <- filter(rt, Title==rt[i,3])
  G2 <- A$cm_wide[1] - A$cm_wide[2]==0
  G3 <- A$cm_wide[1] - A$cm_wide[3]==0
  G4 <- A$cm_wide[1] - A$cm_wide[4]==0
  G5 <- A$cm_wide[1] - A$cm_wide[5]==0
  G6 <- A$cm_wide[1] - A$cm_wide[6]==0
  G7 <- A$cm_wide[1] - A$cm_wide[7]==0
  G8 <- A$cm_wide[1] - A$cm_wide[8]==0
  G9 <- A$cm_wide[1] - A$cm_wide[9]==0
  G10 <- A$cm_wide[1] - A$cm_wide[10]==0
  G11 <- A$cm_wide[1] - A$cm_wide[11]==0
  G12 <- A$cm_wide[1] - A$cm_wide[12]==0
  rt2 <- if(G2==FALSE) filter(rt2, row_num!=A$row_num[2]) else rt2
  rt2 <- if(G3==FALSE) filter(rt2, row_num!=A$row_num[3]) else rt2
  rt2 <- if(G4==FALSE) filter(rt2, row_num!=A$row_num[4]) else rt2
  rt2 <- if(G5==FALSE) filter(rt2, row_num!=A$row_num[5]) else rt2
  rt2 <- if(G6==FALSE) filter(rt2, row_num!=A$row_num[6]) else rt2
  rt2 <- if(G7==FALSE) filter(rt2, row_num!=A$row_num[7]) else rt2
  rt2 <- if(G8==FALSE) filter(rt2, row_num!=A$row_num[8]) else rt2
  rt2 <- if(G9==FALSE) filter(rt2, row_num!=A$row_num[9]) else rt2
  rt2 <- if(G10==FALSE) filter(rt2, row_num!=A$row_num[10]) else rt2
  rt2 <- if(G11==FALSE) filter(rt2, row_num!=A$row_num[11]) else rt2
  rt2 <- if(G12==FALSE) filter(rt2, row_num!=A$row_num[12]) else rt2}

g <- as.data.frame(table(rt2$Title))
g <- filter(g, Freq > 1)
g <- filter(g, Freq < 16)
gt <- NULL
for(i in 1:length(g$Var1)){
  cat('number #', i,'\n' )
  gt<-rbind(gt, filter(rt2, Title==g$Var1[i]))
}
repeated_freq12 <- gt

#13
t <- as.data.frame(table(D$Title))
t <- filter(t, Freq > 12)
t <- filter(t, Freq < 14)
rt <- NULL
for(i in 1:length(t$Var1)){
  cat('number #', i,'\n' )
  rt<-rbind(rt, filter(D, Title==t$Var1[i]))
}

rt2 <- rt
for(i in 1:nrow(rt)){
  cat('#', i,'\n' )
  A <- filter(rt, Title==rt[i,3])
  G2 <- A$cm_wide[1] - A$cm_wide[2]==0
  G3 <- A$cm_wide[1] - A$cm_wide[3]==0
  G4 <- A$cm_wide[1] - A$cm_wide[4]==0
  G5 <- A$cm_wide[1] - A$cm_wide[5]==0
  G6 <- A$cm_wide[1] - A$cm_wide[6]==0
  G7 <- A$cm_wide[1] - A$cm_wide[7]==0
  G8 <- A$cm_wide[1] - A$cm_wide[8]==0
  G9 <- A$cm_wide[1] - A$cm_wide[9]==0
  G10 <- A$cm_wide[1] - A$cm_wide[10]==0
  G11 <- A$cm_wide[1] - A$cm_wide[11]==0
  G12 <- A$cm_wide[1] - A$cm_wide[12]==0
  G13 <- A$cm_wide[1] - A$cm_wide[13]==0
  rt2 <- if(G2==FALSE) filter(rt2, row_num!=A$row_num[2]) else rt2
  rt2 <- if(G3==FALSE) filter(rt2, row_num!=A$row_num[3]) else rt2
  rt2 <- if(G4==FALSE) filter(rt2, row_num!=A$row_num[4]) else rt2
  rt2 <- if(G5==FALSE) filter(rt2, row_num!=A$row_num[5]) else rt2
  rt2 <- if(G6==FALSE) filter(rt2, row_num!=A$row_num[6]) else rt2
  rt2 <- if(G7==FALSE) filter(rt2, row_num!=A$row_num[7]) else rt2
  rt2 <- if(G8==FALSE) filter(rt2, row_num!=A$row_num[8]) else rt2
  rt2 <- if(G9==FALSE) filter(rt2, row_num!=A$row_num[9]) else rt2
  rt2 <- if(G10==FALSE) filter(rt2, row_num!=A$row_num[10]) else rt2
  rt2 <- if(G11==FALSE) filter(rt2, row_num!=A$row_num[11]) else rt2
  rt2 <- if(G12==FALSE) filter(rt2, row_num!=A$row_num[12]) else rt2
  rt2 <- if(G13==FALSE) filter(rt2, row_num!=A$row_num[13]) else rt2}

g <- as.data.frame(table(rt2$Title))
g <- filter(g, Freq > 1)
g <- filter(g, Freq < 16)
gt <- NULL
for(i in 1:length(g$Var1)){
  cat('number #', i,'\n' )
  gt<-rbind(gt, filter(rt2, Title==g$Var1[i]))
}
repeated_freq13 <- gt

#14
t <- as.data.frame(table(D$Title))
t <- filter(t, Freq > 13)
t <- filter(t, Freq < 15)
rt <- NULL
for(i in 1:length(t$Var1)){
  cat('number #', i,'\n' )
  rt<-rbind(rt, filter(D, Title==t$Var1[i]))
}

rt2 <- rt
for(i in 1:nrow(rt)){
  cat('#', i,'\n' )
  A <- filter(rt, Title==rt[i,3])
  G2 <- A$cm_wide[1] - A$cm_wide[2]==0
  G3 <- A$cm_wide[1] - A$cm_wide[3]==0
  G4 <- A$cm_wide[1] - A$cm_wide[4]==0
  G5 <- A$cm_wide[1] - A$cm_wide[5]==0
  G6 <- A$cm_wide[1] - A$cm_wide[6]==0
  G7 <- A$cm_wide[1] - A$cm_wide[7]==0
  G8 <- A$cm_wide[1] - A$cm_wide[8]==0
  G9 <- A$cm_wide[1] - A$cm_wide[9]==0
  G10 <- A$cm_wide[1] - A$cm_wide[10]==0
  G11 <- A$cm_wide[1] - A$cm_wide[11]==0
  G12 <- A$cm_wide[1] - A$cm_wide[12]==0
  G13 <- A$cm_wide[1] - A$cm_wide[13]==0
  G14 <- A$cm_wide[1] - A$cm_wide[14]==0
  rt2 <- if(G2==FALSE) filter(rt2, row_num!=A$row_num[2]) else rt2
  rt2 <- if(G3==FALSE) filter(rt2, row_num!=A$row_num[3]) else rt2
  rt2 <- if(G4==FALSE) filter(rt2, row_num!=A$row_num[4]) else rt2
  rt2 <- if(G5==FALSE) filter(rt2, row_num!=A$row_num[5]) else rt2
  rt2 <- if(G6==FALSE) filter(rt2, row_num!=A$row_num[6]) else rt2
  rt2 <- if(G7==FALSE) filter(rt2, row_num!=A$row_num[7]) else rt2
  rt2 <- if(G8==FALSE) filter(rt2, row_num!=A$row_num[8]) else rt2
  rt2 <- if(G9==FALSE) filter(rt2, row_num!=A$row_num[9]) else rt2
  rt2 <- if(G10==FALSE) filter(rt2, row_num!=A$row_num[10]) else rt2
  rt2 <- if(G11==FALSE) filter(rt2, row_num!=A$row_num[11]) else rt2
  rt2 <- if(G12==FALSE) filter(rt2, row_num!=A$row_num[12]) else rt2
  rt2 <- if(G13==FALSE) filter(rt2, row_num!=A$row_num[13]) else rt2
  rt2 <- if(G14==FALSE) filter(rt2, row_num!=A$row_num[14]) else rt2}

g <- as.data.frame(table(rt2$Title))
g <- filter(g, Freq > 1)
g <- filter(g, Freq < 16)
gt <- NULL
for(i in 1:length(g$Var1)){
  cat('number #', i,'\n' )
  gt<-rbind(gt, filter(rt2, Title==g$Var1[i]))
}
repeated_freq14 <- gt

#15
t <- as.data.frame(table(D$Title))
t <- filter(t, Freq > 14)
t <- filter(t, Freq < 16)
rt <- NULL
for(i in 1:length(t$Var1)){
  cat('number #', i,'\n' )
  rt<-rbind(rt, filter(D, Title==t$Var1[i]))
}

rt2 <- rt
for(i in 1:nrow(rt)){
  cat('#', i,'\n' )
  A <- filter(rt, Title==rt[i,3])
  G2 <- A$cm_wide[1] - A$cm_wide[2]==0
  G3 <- A$cm_wide[1] - A$cm_wide[3]==0
  G4 <- A$cm_wide[1] - A$cm_wide[4]==0
  G5 <- A$cm_wide[1] - A$cm_wide[5]==0
  G6 <- A$cm_wide[1] - A$cm_wide[6]==0
  G7 <- A$cm_wide[1] - A$cm_wide[7]==0
  G8 <- A$cm_wide[1] - A$cm_wide[8]==0
  G9 <- A$cm_wide[1] - A$cm_wide[9]==0
  G10 <- A$cm_wide[1] - A$cm_wide[10]==0
  G11 <- A$cm_wide[1] - A$cm_wide[11]==0
  G12 <- A$cm_wide[1] - A$cm_wide[12]==0
  G13 <- A$cm_wide[1] - A$cm_wide[13]==0
  G14 <- A$cm_wide[1] - A$cm_wide[14]==0
  G15 <- A$cm_wide[1] - A$cm_wide[15]==0
  rt2 <- if(G2==FALSE) filter(rt2, row_num!=A$row_num[2]) else rt2
  rt2 <- if(G3==FALSE) filter(rt2, row_num!=A$row_num[3]) else rt2
  rt2 <- if(G4==FALSE) filter(rt2, row_num!=A$row_num[4]) else rt2
  rt2 <- if(G5==FALSE) filter(rt2, row_num!=A$row_num[5]) else rt2
  rt2 <- if(G6==FALSE) filter(rt2, row_num!=A$row_num[6]) else rt2
  rt2 <- if(G7==FALSE) filter(rt2, row_num!=A$row_num[7]) else rt2
  rt2 <- if(G8==FALSE) filter(rt2, row_num!=A$row_num[8]) else rt2
  rt2 <- if(G9==FALSE) filter(rt2, row_num!=A$row_num[9]) else rt2
  rt2 <- if(G10==FALSE) filter(rt2, row_num!=A$row_num[10]) else rt2
  rt2 <- if(G11==FALSE) filter(rt2, row_num!=A$row_num[11]) else rt2
  rt2 <- if(G12==FALSE) filter(rt2, row_num!=A$row_num[12]) else rt2
  rt2 <- if(G13==FALSE) filter(rt2, row_num!=A$row_num[13]) else rt2
  rt2 <- if(G14==FALSE) filter(rt2, row_num!=A$row_num[14]) else rt2
  rt2 <- if(G15==FALSE) filter(rt2, row_num!=A$row_num[15]) else rt2}

g <- as.data.frame(table(rt2$Title))
g <- filter(g, Freq > 1)
g <- filter(g, Freq < 16)
gt <- NULL
for(i in 1:length(g$Var1)){
  cat('number #', i,'\n' )
  gt<-rbind(gt, filter(rt2, Title==g$Var1[i]))
}
repeated_freq15 <- gt
View(repeated_freq15)
#Получили в 3 раза больше наблюдений

repeated2 <- rbind(repeated_freq2, repeated_freq3, repeated_freq4, 
                   repeated_freq5, repeated_freq6, repeated_freq7,
                   repeated_freq8, repeated_freq9, repeated_freq10, 
                   repeated_freq11, repeated_freq12, repeated_freq13,
                   repeated_freq14, repeated_freq15)
View(repeated2)

g <- as.data.frame(table(repeated2$Title))
g <- filter(g, Freq > 1)
g <- filter(g, Freq < 11)
gt <- NULL
for(i in 1:length(g$Var1)){
  cat('number #', i,'\n' )
  gt<-rbind(gt, filter(repeated2, Title==g$Var1[i]))
}
repeated2 <- gt
repeated2 <-  subset(repeated2, select = -c(row_num))
#write.xlsx(repeated21, file = "repeated21.xlsx")

#Сколько прошло времени между продажами?

#2
dim(repeated2)
repeated3 <- repeated2[complete.cases(repeated2[ , 3]),]
sum(is.na(repeated3$Title))
A <- NULL
repeated2d22 <- NULL
repeated2d2 <- as.data.frame(table(repeated3$Title))
repeated2d2 <- filter(repeated2d2, Freq<3)              
for(i in 1:nrow(repeated2d2)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated3, Title == repeated2d2[i,1]), Transcation_Day)
  A$days <- c(0,0)
  A[2,21] <-  difftime( A[2,12],A[1,12], units = 'days')
  repeated2d22 <- rbind(repeated2d22, A)
}                    
repeated3_freq2 <-repeated2d22

#3
dim(repeated2d2)
repeated3 <- repeated2[complete.cases(repeated2[ , 3]),]
sum(is.na(repeated3$Title))
A <- NULL
repeated2d22 <- NULL
repeated2d2 <- as.data.frame(table(repeated3$Title))
repeated2d2 <- filter(repeated2d2, Freq == 3)              
for(i in 1:nrow(repeated2d2)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated3, Title == repeated2d2[i,1]), Transcation_Day)
  A$days <- c(0,0,0)
  A[2,21] <-  difftime( A[2,12],A[1,12], units = 'days')
  A[3,21] <-  difftime( A[3,12],A[2,12], units = 'days')
  repeated2d22 <- rbind(repeated2d22, A)
}                    
repeated3_freq3 <-repeated2d22

#4
dim(repeated2d2)
repeated3 <- repeated2[complete.cases(repeated2[ , 3]),]
sum(is.na(repeated3$Title))
A <- NULL
repeated2d22 <- NULL
repeated2d2 <- as.data.frame(table(repeated3$Title))
repeated2d2 <- filter(repeated2d2, Freq==4)              
for(i in 1:nrow(repeated2d2)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated3, Title == repeated2d2[i,1]), Transcation_Day)
  A$days <- c(0,0,0,0)
  A[2,21] <-  difftime( A[2,12],A[1,12], units = 'days')
  A[3,21] <-  difftime( A[3,12],A[2,12], units = 'days')
  A[4,21] <-  difftime( A[4,12],A[3,12], units = 'days')
  repeated2d22 <- rbind(repeated2d22, A)
}                    
repeated3_freq4 <-repeated2d22

#5
dim(repeated2d2)
repeated3 <- repeated2[complete.cases(repeated2[ , 3]),]
sum(is.na(repeated3$Title))
A <- NULL
repeated2d22 <- NULL
repeated2d2 <- as.data.frame(table(repeated3$Title))
repeated2d2 <- filter(repeated2d2, Freq==5)              
for(i in 1:nrow(repeated2d2)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated3, Title == repeated2d2[i,1]), Transcation_Day)
  A$days <- c(0,0,0,0,0)
  A[2,21] <-  difftime( A[2,12],A[1,12], units = 'days')
  A[3,21] <-  difftime( A[3,12],A[2,12], units = 'days')
  A[4,21] <-  difftime( A[4,12],A[3,12], units = 'days')
  A[5,21] <-  difftime( A[5,12],A[4,12], units = 'days')
  repeated2d22 <- rbind(repeated2d22, A)
}                    
repeated3_freq5 <-repeated2d22
df <- data.frame(repeated3_freq5$Title, repeated3_freq5$Transcation_Day, repeated3_freq5$days)

#6
dim(repeated2d2)
repeated3 <- repeated2[complete.cases(repeated2[ , 3]),]
sum(is.na(repeated3$Title))
A <- NULL
repeated2d22 <- NULL
repeated2d2 <- as.data.frame(table(repeated3$Title))
repeated2d2 <- filter(repeated2d2, Freq == 6)              
for(i in 1:nrow(repeated2d2)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated3, Title == repeated2d2[i,1]), Transcation_Day)
  A$days <- c(0,0,0,0,0,0)
  A[2,21] <-  difftime( A[2,12],A[1,12], units = 'days')
  A[3,21] <-  difftime( A[3,12],A[2,12], units = 'days')
  A[4,21] <-  difftime( A[4,12],A[3,12], units = 'days')
  A[5,21] <-  difftime( A[5,12],A[4,12], units = 'days')
  A[6,21] <-  difftime( A[6,12],A[5,12], units = 'days')
  repeated2d22 <- rbind(repeated2d22, A)
}                    
repeated3_freq6 <-repeated2d22

#7
dim(repeated2d2)
repeated3 <- repeated2[complete.cases(repeated2[ , 3]),]
sum(is.na(repeated3$Title))
A <- NULL
repeated2d22 <- NULL
repeated2d2 <- as.data.frame(table(repeated3$Title))
repeated2d2 <- filter(repeated2d2, Freq == 7)              
for(i in 1:nrow(repeated2d2)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated3, Title == repeated2d2[i,1]), Transcation_Day)
  A$days <- c(0,0,0,0,0,0,0)
  A[2,21] <-  difftime( A[2,12],A[1,12], units = 'days')
  A[3,21] <-  difftime( A[3,12],A[2,12], units = 'days')
  A[4,21] <-  difftime( A[4,12],A[3,12], units = 'days')
  A[5,21] <-  difftime( A[5,12],A[4,12], units = 'days')
  A[6,21] <-  difftime( A[6,12],A[5,12], units = 'days')
  A[7,21] <-  difftime( A[7,12],A[6,12], units = 'days')
  repeated2d22 <- rbind(repeated2d22, A)
}                    
repeated3_freq7 <-repeated2d22

#8
dim(repeated2d2)
repeated3 <- repeated2[complete.cases(repeated2[ , 3]),]
sum(is.na(repeated3$Title))
A <- NULL
repeated2d22 <- NULL
repeated2d2 <- as.data.frame(table(repeated3$Title))
repeated2d2 <- filter(repeated2d2, Freq==8)              
for(i in 1:nrow(repeated2d2)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated3, Title == repeated2d2[i,1]), Transcation_Day)
  A$days <- c(0,0,0,0,0,0,0,0)
  A[2,21] <-  difftime( A[2,12],A[1,12], units = 'days')
  A[3,21] <-  difftime( A[3,12],A[2,12], units = 'days')
  A[4,21] <-  difftime( A[4,12],A[3,12], units = 'days')
  A[5,21] <-  difftime( A[5,12],A[4,12], units = 'days')
  A[6,21] <-  difftime( A[6,12],A[5,12], units = 'days')
  A[7,21] <-  difftime( A[7,12],A[6,12], units = 'days')
  A[8,21] <-  difftime( A[8,12],A[7,12], units = 'days')
  repeated2d22 <- rbind(repeated2d22, A)
}                    
repeated3_freq8 <-repeated2d22

#9
dim(repeated2d2)
repeated3 <- repeated2[complete.cases(repeated2[ , 3]),]
sum(is.na(repeated3$Title))
A <- NULL
repeated2d22 <- NULL
repeated2d2 <- as.data.frame(table(repeated3$Title))
repeated2d2 <- filter(repeated2d2, Freq==9)              
for(i in 1:nrow(repeated2d2)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated3, Title == repeated2d2[i,1]), Transcation_Day)
  A$days <- c(0,0,0,0,0,0,0,0,0)
  A[2,21] <-  difftime( A[2,12],A[1,12], units = 'days')
  A[3,21] <-  difftime( A[3,12],A[2,12], units = 'days')
  A[4,21] <-  difftime( A[4,12],A[3,12], units = 'days')
  A[5,21] <-  difftime( A[5,12],A[4,12], units = 'days')
  A[6,21] <-  difftime( A[6,12],A[5,12], units = 'days')
  A[7,21] <-  difftime( A[7,12],A[6,12], units = 'days')
  A[8,21] <-  difftime( A[8,12],A[7,12], units = 'days')
  A[9,21] <-  difftime( A[9,12],A[8,12], units = 'days')
  repeated2d22 <- rbind(repeated2d22, A)
}                    
repeated3_freq9 <-repeated2d22

#10
dim(repeated2d2)
repeated3 <- repeated2[complete.cases(repeated2[ , 3]),]
sum(is.na(repeated3$Title))
A <- NULL
repeated2d22 <- NULL
repeated2d2 <- as.data.frame(table(repeated3$Title))
repeated2d2 <- filter(repeated2d2, Freq==10)              
for(i in 1:nrow(repeated2d2)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated3, Title == repeated2d2[i,1]), Transcation_Day)
  A$days <- c(0,0,0,0,0,0,0,0,0,0)
  A[2,21] <-  difftime( A[2,12],A[1,12], units = 'days')
  A[3,21] <-  difftime( A[3,12],A[2,12], units = 'days')
  A[4,21] <-  difftime( A[4,12],A[3,12], units = 'days')
  A[5,21] <-  difftime( A[5,12],A[4,12], units = 'days')
  A[6,21] <-  difftime( A[6,12],A[5,12], units = 'days')
  A[7,21] <-  difftime( A[7,12],A[6,12], units = 'days')
  A[8,21] <-  difftime( A[8,12],A[7,12], units = 'days')
  A[9,21] <-  difftime( A[9,12],A[8,12], units = 'days')
  A[10,21] <-  difftime( A[10,12],A[9,12], units = 'days')
  repeated2d22 <- rbind(repeated2d22, A)
}                    
repeated3_freq10 <-repeated2d22


repeated_days <- rbind(repeated3_freq2, repeated3_freq3, repeated3_freq4,
                       repeated3_freq5, repeated3_freq6, repeated3_freq7,
                       repeated3_freq8, repeated3_freq9, repeated3_freq10)

#В каком аукционном доме продавали?

#2
A <- NULL
repeated_days5 <-  NULL
repeated_days1 <- as.data.frame(table(repeated_days$Title))
repeated_days1 <- filter(repeated_days1, Freq<3)
for(i in 1:nrow(repeated_days1)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_days, Title == repeated_days1[i,1]), Transcation_Day)
  A$samehouse <- c('first',NA)
  A[2,22] <- ifelse(A[1,13]==A[2,13], 'same', 'notsame')
  repeated_days5 <- rbind(repeated_days5, A)
}
repeated_days2 <- repeated_days5

#3
A <- NULL
repeated_days5 <-  NULL
repeated_days1 <- as.data.frame(table(repeated_days$Title))
repeated_days1 <- filter(repeated_days1, Freq==3)
for(i in 1:nrow(repeated_days1)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_days, Title == repeated_days1[i,1]), Transcation_Day)
  A$samehouse <- c('first',NA, NA)
  A[2,22] <- ifelse(A[1,13]==A[2,13], 'same', 'notsame')
  A[3,22] <- ifelse(A[2,13]==A[3,13], 'same', 'notsame')
  repeated_days5 <- rbind(repeated_days5, A)
}
repeated_days3 <- repeated_days5

#4
A <- NULL
repeated_days5 <-  NULL
repeated_days1 <- as.data.frame(table(repeated_days$Title))
repeated_days1 <- filter(repeated_days1, Freq==4)
for(i in 1:nrow(repeated_days1)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_days, Title == repeated_days1[i,1]), Transcation_Day)
  A$samehouse <- c('first',NA, NA, NA)
  A[2,22] <- ifelse(A[1,13]==A[2,13], 'same', 'notsame')
  A[3,22] <- ifelse(A[2,13]==A[3,13], 'same', 'notsame')
  A[4,22] <- ifelse(A[3,13]==A[4,13], 'same', 'notsame')
  repeated_days5 <- rbind(repeated_days5, A)
}
repeated_days4 <- repeated_days5

#5
A <- NULL
repeated_days5 <-  NULL
repeated_days1 <- as.data.frame(table(repeated_days$Title))
repeated_days1 <- filter(repeated_days1, Freq==5)
for(i in 1:nrow(repeated_days1)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_days, Title == repeated_days1[i,1]), Transcation_Day)
  A$samehouse <- c('first',NA, NA, NA, NA)
  A[2,22] <- ifelse(A[1,13]==A[2,13], 'same', 'notsame')
  A[3,22] <- ifelse(A[2,13]==A[3,13], 'same', 'notsame')
  A[4,22] <- ifelse(A[3,13]==A[4,13], 'same', 'notsame')
  A[5,22] <- ifelse(A[4,13]==A[5,13], 'same', 'notsame')
  repeated_days5 <- rbind(repeated_days5, A)
}
repeated_days51 <- repeated_days5
df <- data.frame(repeated_days5$Title, repeated_days5$Transcation_Day, repeated_days5$Auction_house, repeated_days5$samehouse)

#6
A <- NULL
repeated_days5 <-  NULL
repeated_days1 <- as.data.frame(table(repeated_days$Title))
repeated_days1 <- filter(repeated_days1, Freq==6)
for(i in 1:nrow(repeated_days1)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_days, Title == repeated_days1[i,1]), Transcation_Day)
  A$samehouse <- c('first',NA, NA, NA, NA,NA)
  A[2,22] <- ifelse(A[1,13]==A[2,13], 'same', 'notsame')
  A[3,22] <- ifelse(A[2,13]==A[3,13], 'same', 'notsame')
  A[4,22] <- ifelse(A[3,13]==A[4,13], 'same', 'notsame')
  A[5,22] <- ifelse(A[4,13]==A[5,13], 'same', 'notsame')
  A[6,22] <- ifelse(A[5,13]==A[6,13], 'same', 'notsame')
  repeated_days5 <- rbind(repeated_days5, A)
}
repeated_days6 <- repeated_days5

#7
A <- NULL
repeated_days5 <-  NULL
repeated_days1 <- as.data.frame(table(repeated_days$Title))
repeated_days1 <- filter(repeated_days1, Freq==7)
for(i in 1:nrow(repeated_days1)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_days, Title == repeated_days1[i,1]), Transcation_Day)
  A$samehouse <- c('first',NA, NA, NA, NA,NA, NA)
  A[2,22] <- ifelse(A[1,13]==A[2,13], 'same', 'notsame')
  A[3,22] <- ifelse(A[2,13]==A[3,13], 'same', 'notsame')
  A[4,22] <- ifelse(A[3,13]==A[4,13], 'same', 'notsame')
  A[5,22] <- ifelse(A[4,13]==A[5,13], 'same', 'notsame')
  A[6,22] <- ifelse(A[5,13]==A[6,13], 'same', 'notsame')
  A[7,22] <- ifelse(A[6,13]==A[7,13], 'same', 'notsame')
  repeated_days5 <- rbind(repeated_days5, A)
}
repeated_days7 <- repeated_days5

#8
A <- NULL
repeated_days5 <-  NULL
repeated_days1 <- as.data.frame(table(repeated_days$Title))
repeated_days1 <- filter(repeated_days1, Freq==8)
for(i in 1:nrow(repeated_days1)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_days, Title == repeated_days1[i,1]), Transcation_Day)
  A$samehouse <- c('first',NA, NA, NA, NA,NA, NA, NA)
  A[2,22] <- ifelse(A[1,13]==A[2,13], 'same', 'notsame')
  A[3,22] <- ifelse(A[2,13]==A[3,13], 'same', 'notsame')
  A[4,22] <- ifelse(A[3,13]==A[4,13], 'same', 'notsame')
  A[5,22] <- ifelse(A[4,13]==A[5,13], 'same', 'notsame')
  A[6,22] <- ifelse(A[5,13]==A[6,13], 'same', 'notsame')
  A[7,22] <- ifelse(A[6,13]==A[7,13], 'same', 'notsame')
  A[8,22] <- ifelse(A[7,13]==A[8,13], 'same', 'notsame')
  repeated_days5 <- rbind(repeated_days5, A)
}
repeated_days8 <- repeated_days5

#9
A <- NULL
repeated_days5 <-  NULL
repeated_days1 <- as.data.frame(table(repeated_days$Title))
repeated_days1 <- filter(repeated_days1, Freq==9)
for(i in 1:nrow(repeated_days1)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_days, Title == repeated_days1[i,1]), Transcation_Day)
  A$samehouse <- c('first',NA, NA, NA, NA,NA, NA, NA, NA)
  A[2,22] <- ifelse(A[1,13]==A[2,13], 'same', 'notsame')
  A[3,22] <- ifelse(A[2,13]==A[3,13], 'same', 'notsame')
  A[4,22] <- ifelse(A[3,13]==A[4,13], 'same', 'notsame')
  A[5,22] <- ifelse(A[4,13]==A[5,13], 'same', 'notsame')
  A[6,22] <- ifelse(A[5,13]==A[6,13], 'same', 'notsame')
  A[7,22] <- ifelse(A[6,13]==A[7,13], 'same', 'notsame')
  A[8,22] <- ifelse(A[7,13]==A[8,13], 'same', 'notsame')
  A[9,22] <- ifelse(A[8,13]==A[9,13], 'same', 'notsame')
  repeated_days5 <- rbind(repeated_days5, A)
}
repeated_days9 <- repeated_days5

#10
A <- NULL
repeated_days5 <-  NULL
repeated_days1 <- as.data.frame(table(repeated_days$Title))
repeated_days1 <- filter(repeated_days1, Freq==10)
for(i in 1:nrow(repeated_days1)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_days, Title == repeated_days1[i,1]), Transcation_Day)
  A$samehouse <- c('first',NA, NA, NA, NA,NA, NA, NA, NA,NA)
  A[2,22] <- ifelse(A[1,13]==A[2,13], 'same', 'notsame')
  A[3,22] <- ifelse(A[2,13]==A[3,13], 'same', 'notsame')
  A[4,22] <- ifelse(A[3,13]==A[4,13], 'same', 'notsame')
  A[5,22] <- ifelse(A[4,13]==A[5,13], 'same', 'notsame')
  A[6,22] <- ifelse(A[5,13]==A[6,13], 'same', 'notsame')
  A[7,22] <- ifelse(A[6,13]==A[7,13], 'same', 'notsame')
  A[8,22] <- ifelse(A[7,13]==A[8,13], 'same', 'notsame')
  A[9,22] <- ifelse(A[8,13]==A[9,13], 'same', 'notsame')
  A[10,22] <- ifelse(A[9,13]==A[10,13], 'same', 'notsame')
  repeated_days5 <- rbind(repeated_days5, A)
}
df <- data.frame(repeated_days5$Title, repeated_days5$Transcation_Day, repeated_days5$Auction_house, repeated_days5$samehouse)
repeated_days10 <- repeated_days5

repeated_same <- rbind(repeated_days2, repeated_days3, repeated_days4,
                       repeated_days51, repeated_days6, repeated_days7, repeated_days8,
                       repeated_days9, repeated_days10)
sum(is.na(repeated_same$days))
repeated_same <- repeated_same[complete.cases(repeated_same[ , 12]),]

#В скольких % случаев тот же аукционный дом
repeated_same$int <- NA
repeated_same_no1 <- filter(repeated_same, samehouse!= 'first')
repeated_same_no1$int <- ifelse(repeated_same_no1$days < 181 | repeated_same_no1$samehouse == 'same',
                                1, 0)

summary(repeated_same_no1)
sum(repeated_same_no1$int)/nrow(repeated_same_no1)
#В 83% случаев продавали в том же доме
repeated_same_1 <- filter(repeated_same, samehouse== 'first')
repeated_same_1$int <- NA
repeated_all <- rbind(repeated_same_1,repeated_same_no1)

#В каких домах сколько продаж? Что за 5 столбец?
repeated_same_no1 <- repeated_same_no1[complete.cases(repeated_same_no1[ ,5]),]
sum(repeated_same_no1$int)/nrow(repeated_same_no1)
house_repeated <- as.data.frame(table(repeated_same_no1$Auction_house))
house_repeated <- filter(house_repeated, Freq>200)

#Насколько часто уходят из аукционного дома
house <- data.frame(house_repeated$Var1)
L <- dim(house)[1]
house[2,1]
A <- NULL
ch <- c(NA,NA,NA)

for(i in 1:12){
  cat('#', i, '\n')
  A <- filter(repeated_same_no1, Auction_house == house_repeated[i,1])
  sh_in <- sum(A$int)/nrow(A)
  r <- c(as.character(house_repeated[i,1]), as.numeric(sh_in), 1-as.numeric(sh_in))
  ch <- rbind(ch, r)
}

ch<-  ch[-1,]
colnames(ch) <- c('house', 'internal', 'external')
View(ch)

#Изменение в цене

#2
repeated_all_t <- as.data.frame(table(repeated_all$Title))
repeated_all_t <- filter(repeated_all_t, Freq==2)
A <- NULL
repeated_all5 <- NULL
for(i in 1:nrow(repeated_all_t)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_all, Title == repeated_all_t[i,1]), Transcation_Day)
  A$scaledprice <- c(0,0)
  A[2,24] <- A[2,16]/A[1,16]
  repeated_all5 <- rbind(repeated_all5, A)
}
repeated_all2 <- repeated_all5

#3
repeated_all_t <- as.data.frame(table(repeated_all$Title))
repeated_all_t <- filter(repeated_all_t, Freq==3)
A <- NULL
repeated_all5 <- NULL
for(i in 1:nrow(repeated_all_t)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_all, Title == repeated_all_t[i,1]), Transcation_Day)
  A$scaledprice <- c(0,0,0)
  A[2,24] <- A[2,16]/A[1,16]
  A[3,24] <- A[3,16]/A[1,16]
  repeated_all5 <- rbind(repeated_all5, A)
}
repeated_all3 <- repeated_all5

#4
repeated_all_t <- as.data.frame(table(repeated_all$Title))
repeated_all_t <- filter(repeated_all_t, Freq==4)
A <- NULL
repeated_all5 <- NULL
for(i in 1:nrow(repeated_all_t)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_all, Title == repeated_all_t[i,1]), Transcation_Day)
  A$scaledprice <- c(0,0,0,0)
  A[2,24] <- A[2,16]/A[1,16]
  A[3,24] <- A[3,16]/A[1,16]
  A[4,24] <- A[4,16]/A[1,16]
  repeated_all5 <- rbind(repeated_all5, A)
}
repeated_all4 <- repeated_all5

#5
repeated_all_t <- as.data.frame(table(repeated_all$Title))
repeated_all_t <- filter(repeated_all_t, Freq==5)
A <- NULL
repeated_all5 <- NULL
for(i in 1:nrow(repeated_all_t)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_all, Title == repeated_all_t[i,1]), Transcation_Day)
  A$scaledprice <- c(0,0,0,0,0)
  A[2,24] <- A[2,16]/A[1,16]
  A[3,24] <- A[3,16]/A[1,16]
  A[4,24] <- A[4,16]/A[1,16]
  A[5,24] <- A[5,16]/A[1,16]
  repeated_all5 <- rbind(repeated_all5, A)
}
repeated_all51 <- repeated_all5

#6

repeated_all_t <- as.data.frame(table(repeated_all$Title))
repeated_all_t <- filter(repeated_all_t, Freq==6)
A <- NULL
repeated_all5 <- NULL
for(i in 1:nrow(repeated_all_t)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_all, Title == repeated_all_t[i,1]), Transcation_Day)
  A$scaledprice <- c(0,0,0,0,0,0)
  A[2,24] <- A[2,16]/A[1,16]
  A[3,24] <- A[3,16]/A[1,16]
  A[4,24] <- A[4,16]/A[1,16]
  A[5,24] <- A[5,16]/A[1,16]
  A[6,24] <- A[6,16]/A[1,16]
  repeated_all5 <- rbind(repeated_all5, A)
}

repeated_all6 <- repeated_all5

#7
repeated_all_t <- as.data.frame(table(repeated_all$Title))
repeated_all_t <- filter(repeated_all_t, Freq==7)
A <- NULL
repeated_all5 <- NULL
for(i in 1:nrow(repeated_all_t)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_all, Title == repeated_all_t[i,1]), Transcation_Day)
  A$scaledprice <- c(0,0,0,0,0,0,0)
  A[2,24] <- A[2,16]/A[1,16]
  A[3,24] <- A[3,16]/A[1,16]
  A[4,24] <- A[4,16]/A[1,16]
  A[5,24] <- A[5,16]/A[1,16]
  A[6,24] <- A[6,16]/A[1,16]
  A[7,24] <- A[7,16]/A[1,16]
  repeated_all5 <- rbind(repeated_all5, A)
}
repeated_all7 <- repeated_all5

#8
repeated_all_t <- as.data.frame(table(repeated_all$Title))
repeated_all_t <- filter(repeated_all_t, Freq==8)
A <- NULL
repeated_all5 <- NULL
for(i in 1:nrow(repeated_all_t)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_all, Title == repeated_all_t[i,1]), Transcation_Day)
  A$scaledprice <- c(0,0,0,0,0,0,0,0)
  A[2,24] <- A[2,16]/A[1,16]
  A[3,24] <- A[3,16]/A[1,16]
  A[4,24] <- A[4,16]/A[1,16]
  A[5,24] <- A[5,16]/A[1,16]
  A[6,24] <- A[6,16]/A[1,16]
  A[7,24] <- A[7,16]/A[1,16]
  A[8,24] <- A[8,16]/A[1,16]
  repeated_all5 <- rbind(repeated_all5, A)
}
repeated_all8 <- repeated_all5

#9
repeated_all_t <- as.data.frame(table(repeated_all$Title))
repeated_all_t <- filter(repeated_all_t, Freq==9)
A <- NULL
repeated_all5 <- NULL
for(i in 1:nrow(repeated_all_t)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_all, Title == repeated_all_t[i,1]), Transcation_Day)
  A$scaledprice <- c(0,0,0,0,0,0,0,0,0)
  A[2,24] <- A[2,16]/A[1,16]
  A[3,24] <- A[3,16]/A[1,16]
  A[4,24] <- A[4,16]/A[1,16]
  A[5,24] <- A[5,16]/A[1,16]
  A[6,24] <- A[6,16]/A[1,16]
  A[7,24] <- A[7,16]/A[1,16]
  A[8,24] <- A[8,16]/A[1,16]
  A[9,24] <- A[9,16]/A[1,16]
  repeated_all5 <- rbind(repeated_all5, A)
}
repeated_all9 <- repeated_all5

#10
repeated_all_t <- as.data.frame(table(repeated_all$Title))
repeated_all_t <- filter(repeated_all_t, Freq==10)
A <- NULL
repeated_all5 <- NULL
for(i in 1:nrow(repeated_all_t)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_all, Title == repeated_all_t[i,1]), Transcation_Day)
  A$scaledprice <- c(0,0,0,0,0,0,0,0,0,0)
  A[2,24] <- A[2,16]/A[1,16]
  A[3,24] <- A[3,16]/A[1,16]
  A[4,24] <- A[4,16]/A[1,16]
  A[5,24] <- A[5,16]/A[1,16]
  A[6,24] <- A[6,16]/A[1,16]
  A[7,24] <- A[7,16]/A[1,16]
  A[8,24] <- A[8,16]/A[1,16]
  A[9,24] <- A[9,16]/A[1,16]
  A[10,24] <- A[10,16]/A[1,16]
  repeated_all5 <- rbind(repeated_all5, A)
}
repeated_all10 <- repeated_all5

repeated_scaledprice <- rbind(repeated_all2, repeated_all3, repeated_all4, repeated_all51,
                              repeated_all6, repeated_all7, repeated_all8, repeated_all9,
                              repeated_all10)

#Сколько прошло периодов между ТОРГАМИ?
repeated_scaledprice$repfreq <- repeated_scaledprice$days/180


#Добавим столбец предыдущей продажи

#2
repeated_scaledprice_t <- as.data.frame(table(repeated_scaledprice$Title))
repeated_scaledprice_t <- filter(repeated_scaledprice_t, Freq<3)
A <- NULL
repeated_scaledprice5 <- NULL
for(i in 1:nrow(repeated_scaledprice_t)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_scaledprice, Title == repeated_scaledprice_t[i,1]), Transcation_Day)
  A$prevusd <- c(NA,NA)
  A[2,26] <- A[1,16]
  repeated_scaledprice5 <- rbind(repeated_scaledprice5, A)
}
repeated_scaledprice2 <- repeated_scaledprice5

#3
repeated_scaledprice_t <- as.data.frame(table(repeated_scaledprice$Title))
repeated_scaledprice_t <- filter(repeated_scaledprice_t, Freq==3)
A <- NULL
repeated_scaledprice5 <- NULL
for(i in 1:nrow(repeated_scaledprice_t)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_scaledprice, Title == repeated_scaledprice_t[i,1]), Transcation_Day)
  A$prevusd <- c(NA,NA,NA)
  A[2,26] <- A[1,16]
  A[3,26] <- A[2,16]
  repeated_scaledprice5 <- rbind(repeated_scaledprice5, A)
}
repeated_scaledprice3 <- repeated_scaledprice5

#4
repeated_scaledprice_t <- as.data.frame(table(repeated_scaledprice$Title))
repeated_scaledprice_t <- filter(repeated_scaledprice_t, Freq==4)
A <- NULL
repeated_scaledprice5 <- NULL
for(i in 1:nrow(repeated_scaledprice_t)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_scaledprice, Title == repeated_scaledprice_t[i,1]), Transcation_Day)
  A$prevusd <- c(NA,NA,NA,NA)
  A[2,26] <- A[1,16]
  A[3,26] <- A[2,16]
  A[4,26] <- A[3,16]
  repeated_scaledprice5 <- rbind(repeated_scaledprice5, A)
}
repeated_scaledprice4 <- repeated_scaledprice5

#5
repeated_scaledprice_t <- as.data.frame(table(repeated_scaledprice$Title))
repeated_scaledprice_t <- filter(repeated_scaledprice_t, Freq==5)
A <- NULL
repeated_scaledprice5 <- NULL
for(i in 1:nrow(repeated_scaledprice_t)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_scaledprice, Title == repeated_scaledprice_t[i,1]), Transcation_Day)
  A$prevusd <- c(NA,NA,NA,NA,NA)
  A[2,26] <- A[1,16]
  A[3,26] <- A[2,16]
  A[4,26] <- A[3,16]
  A[5,26] <- A[4,16]
  repeated_scaledprice5 <- rbind(repeated_scaledprice5, A)
}
repeated_scaledprice51 <- repeated_scaledprice5

#6
repeated_scaledprice_t <- as.data.frame(table(repeated_scaledprice$Title))
repeated_scaledprice_t <- filter(repeated_scaledprice_t, Freq==6)
A <- NULL
repeated_scaledprice5 <- NULL
for(i in 1:nrow(repeated_scaledprice_t)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_scaledprice, Title == repeated_scaledprice_t[i,1]), Transcation_Day)
  A$prevusd <- c(NA,NA,NA,NA,NA,NA)
  A[2,26] <- A[1,16]
  A[3,26] <- A[2,16]
  A[4,26] <- A[3,16]
  A[5,26] <- A[4,16]
  A[6,26] <- A[5,16]
  repeated_scaledprice5 <- rbind(repeated_scaledprice5, A)
}
repeated_scaledprice6 <- repeated_scaledprice5

#7
repeated_scaledprice_t <- as.data.frame(table(repeated_scaledprice$Title))
repeated_scaledprice_t <- filter(repeated_scaledprice_t, Freq==7)
A <- NULL
repeated_scaledprice5 <- NULL
for(i in 1:nrow(repeated_scaledprice_t)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_scaledprice, Title == repeated_scaledprice_t[i,1]), Transcation_Day)
  A$prevusd <- c(NA,NA,NA,NA,NA,NA,NA)
  A[2,26] <- A[1,16]
  A[3,26] <- A[2,16]
  A[4,26] <- A[3,16]
  A[5,26] <- A[4,16]
  A[6,26] <- A[5,16]
  A[7,26] <- A[6,16]
  repeated_scaledprice5 <- rbind(repeated_scaledprice5, A)
}
repeated_scaledprice7 <- repeated_scaledprice5

#8
repeated_scaledprice_t <- as.data.frame(table(repeated_scaledprice$Title))
repeated_scaledprice_t <- filter(repeated_scaledprice_t, Freq==8)
A <- NULL
repeated_scaledprice5 <- NULL
for(i in 1:nrow(repeated_scaledprice_t)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_scaledprice, Title == repeated_scaledprice_t[i,1]), Transcation_Day)
  A$prevusd <- c(NA,NA,NA,NA,NA,NA,NA,NA)
  A[2,26] <- A[1,16]
  A[3,26] <- A[2,16]
  A[4,26] <- A[3,16]
  A[5,26] <- A[4,16]
  A[6,26] <- A[5,16]
  A[7,26] <- A[6,16]
  A[8,26] <- A[7,16]
  repeated_scaledprice5 <- rbind(repeated_scaledprice5, A)
}
repeated_scaledprice8 <- repeated_scaledprice5

#9
repeated_scaledprice_t <- as.data.frame(table(repeated_scaledprice$Title))
repeated_scaledprice_t <- filter(repeated_scaledprice_t, Freq==9)
A <- NULL
repeated_scaledprice5 <- NULL
for(i in 1:nrow(repeated_scaledprice_t)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_scaledprice, Title == repeated_scaledprice_t[i,1]), Transcation_Day)
  A$prevusd <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA)
  A[2,26] <- A[1,16]
  A[3,26] <- A[2,16]
  A[4,26] <- A[3,16]
  A[5,26] <- A[4,16]
  A[6,26] <- A[5,16]
  A[7,26] <- A[6,16]
  A[8,26] <- A[7,16]
  A[9,26] <- A[8,16]
  repeated_scaledprice5 <- rbind(repeated_scaledprice5, A)
}
repeated_scaledprice9 <- repeated_scaledprice5

#10
repeated_scaledprice_t <- as.data.frame(table(repeated_scaledprice$Title))
repeated_scaledprice_t <- filter(repeated_scaledprice_t, Freq==10)
A <- NULL
repeated_scaledprice5 <- NULL
for(i in 1:nrow(repeated_scaledprice_t)){
  cat('#', i, '\n')
  A <- arrange(filter(repeated_scaledprice, Title == repeated_scaledprice_t[i,1]), Transcation_Day)
  A$prevusd <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  A[2,26] <- A[1,16]
  A[3,26] <- A[2,16]
  A[4,26] <- A[3,16]
  A[5,26] <- A[4,16]
  A[6,26] <- A[5,16]
  A[7,26] <- A[6,16]
  A[8,26] <- A[7,16]
  A[9,26] <- A[8,16]
  A[10,26] <- A[9,16]  
  repeated_scaledprice5 <- rbind(repeated_scaledprice5, A)
}
repeated_scaledprice10 <- repeated_scaledprice5


repeated_pr <- rbind(repeated_scaledprice2, repeated_scaledprice3, repeated_scaledprice4,
                     repeated_scaledprice51, repeated_scaledprice6, repeated_scaledprice7,
                     repeated_scaledprice8, repeated_scaledprice9, repeated_scaledprice10)
View(repeated_pr)

dim(repeated_pr)
art <- repeated_pr
art <- filter(art, scaledprice>0)
art <- art[complete.cases(art[ ,26]),]
dim(art)
#В итоге имеем 6876 наблюдений для повторных продаж.

#Оставим в данных только крупные аукционные дома
rep_mod <- art
hhhh <-  as.data.frame(table(rep_mod$Auction_house))
hhhh <- filter(hhhh, hhhh$Freq>2)
View(hhhh)

rep_mod_norm <- NULL
for(i in 1:length(hhhh$Var1)){
  rep_mod_norm<-rbind(rep_mod_norm, filter(rep_mod, Auction_house==hhhh$Var1[i]))
}

#Посмотрим на длину картин
dim(rep_mod_norm)
rep_mod <- rep_mod_norm
l <- na.omit(rep_mod$cm_long)
quantile(l, c(.02, .97))
rep_mod <- dplyr::filter(rep_mod, cm_long < 162)

rep_mod <- rep_mod_norm
dim(rep_mod_norm)
View(rep_mod)
rep_mod$int <- as.character(rep_mod$int)

#write.xlsx(rep_mod, 'rep_mod.xlsx')

#Итог: 6739 наблюдений повторных продаж.
#Обработка данных для регрессии повторных продаж закончена

####Повт. модель####
#________________________________________
#________________________________________
#________________________________________

#Модель у Дерьи с 615 строки
View(rep_mod_norm)

#Сделаем зависимость изменений цены от прошедшего времени с перепродажи и от размера картины
names(rep_mod)
sum(is.na(rep_mod$scaledprice))
rep_mod1 <- rep_mod[complete.cases(rep_mod[ , 6]),]
summary(rep_mod1)
rep_mod1 <- na.omit(rep_mod)

#Работаем на 1831 наблюдении
str(rep_mod1)
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
ma <- filter(la, la$Freq>25)
rep_mod3 <- NULL
for(i in 1:length(ma$Var1)){rep_mod3<-rbind(rep_mod3, filter(rep_mod2, Auction_house==ma$Var1[i]))}
View(ma)
View(rep_mod3)
rep_mod3$Auction_house <- gsub("AB Stockholms Auktionsverk", "AB", rep_mod3$Auction_house)
rep_mod3$Auction_house <- gsub("Swann Galleries", "Sw", rep_mod3$Auction_house)
rep_mod3$Auction_house <- class.ind(rep_mod3$Auction_house)

#Посмотрим корреляции
D1_corr <- subset(rep_mod3, select = -c(Artist, Country_of_birth, Title, Transcation_Day,
                                        Currency, Floor_Price, Ceiling_Price, samehouse,
                                        Die, cm_long, Stamped, Inscribed, repfreq))
corr <- corrplot(cor(D1_corr))

#Scaledprice - это рост цены. Судя по корр. матрице, зависимости не оч. сильные везде
#Но есть зависимость от int (тот же или не тот же аукц.дом); Bill_Invoice; Medium.Oil, Medium.Lithorgaph
#days(дни между продажами), AuctionHouse

#Модель 1
rep_mod1
Ta <- as.data.frame(table(rep_mod1$Medium))
ma <- filter(Ta, Ta$Freq>45)
rep_mod2 <- NULL
for(i in 1:length(ma$Var1)){
  rep_mod2<-rbind(rep_mod2, filter(rep_mod1, Medium==ma$Var1[i]))}

la <- as.data.frame(table(rep_mod1$Auction_house))
View(ma)
ma <- filter(la, la$Freq>30)
rep_mod3 <- NULL
for(i in 1:length(ma$Var1)){rep_mod3<-rbind(rep_mod3, filter(rep_mod2, Auction_house==ma$Var1[i]))}


D1_corr1 <- subset(rep_mod3, select = -c(Artist, Country_of_birth, Title, Transcation_Day,
                                         Currency, Floor_Price, Ceiling_Price, samehouse,
                                         Die, cm_long, Stamped, Inscribed, repfreq, Dating, Price))

data <- D1_corr1

mod_r <- lm(scaledprice ~ . ,data = data)

summary(mod_r)
v <- vif(mod_r)
v
#Мультикол. нет

plot(mod_r, 1)
plot(mod_r, 2)
plot(mod_r, 3)

#________________________________________
#Нормальность остатков
plot(mod_r, which = 2)
#Не нормальны

#Плотность остатков
e <- resid(mod_r)
data_e <- data.frame(e)
ggplot(data = data_e, aes(e)) + geom_density() +
  scale_colour_brewer(palette="Set1", guide=FALSE)

#Мы не симметричны поо плотности.

boxCox(mod_r)
#Нужен логарифм вроде
mod2 <- update(mod_r, log(scaledprice) ~ . )
summary(mod2)
plot(mod2, 2)
#Лучше не стало, так что не будем.

#________________________________________
#Спецификация
plot(mod_r, which = 1) 
#Вроде все норм
resettest(mod_r)
#p-value оч маленький, а значит не норм 


#p-value = 0,34%, а значит в принципе норм всё
summary(mod_r)
crPlots(mod_r)
#Можем исправить только если Price
resettest(mod_r, power = 2)


#________________________________________
#Гетероскедастичность - одинакова ли станд.ошибка
plot(mod_r, which = 3)
#Тест Бреуша-Пагана
library(lmtest)
bptest(mod_r)
#H0 - гетероскедастичности нет при p-value большом выполняется
#Гетеро <- есть

best_model_1 <- mod_r
summary(mod_r)

stargazer(mod1, type = 'text', df = FALSE, 
          digits =2, title = '.', column.labels = 'lalala')

#Модель 2
names(rep_mod)
sum(is.na(rep_mod$scaledprice))
rep_mod1 <- rep_mod[complete.cases(rep_mod[ , 6]),]
summary(rep_mod1)
rep_mod1 <- na.omit(rep_mod)

Ta <- as.data.frame(table(rep_mod1$Medium))
ma <- filter(Ta, Ta$Freq>45)
rep_mod2 <- NULL
for(i in 1:length(ma$Var1)){
  rep_mod2<-rbind(rep_mod2, filter(rep_mod1, Medium==ma$Var1[i]))}

la <- as.data.frame(table(rep_mod1$Auction_house))
ma <- filter(la, la$Freq>30)
rep_mod3 <- NULL
for(i in 1:length(ma$Var1)){rep_mod3<-rbind(rep_mod3, filter(rep_mod2, Auction_house==ma$Var1[i]))}

str(rep_mod3)

D1_corr1 <- subset(rep_mod3, select = -c(Artist, Country_of_birth, Title, Transcation_Day,
                                         Currency, Floor_Price, Ceiling_Price, samehouse,
                                         Die, cm_long, Stamped, Inscribed, prevusd, Dating))

View(D1_corr1)
str(D1_corr1)
corr <- corrplot(cor(D1_corr))
data <- D1_corr1
mod_r <- lm(Price ~ . -repfreq,data = data)

summary(mod_r)
v <- vif(mod_r)
v

#Мультикол. нет

plot(mod_r, 1)
plot(mod_r, 2)
#остатки не нормально. Возможно, стоит выбросить некоторые наблюдения
plot(mod_r, 3)
#Прям как будто нужны квадраты

#________________________________________
#Нормальность остатков
plot(mod_r, which = 2)
#Не нормальны

#Плотность остатков
e <- resid(mod_r)
data_e <- data.frame(e)
ggplot(data = data_e, aes(e)) + geom_density() +
  scale_colour_brewer(palette="Set1", guide=FALSE)

#Мы не симметричны по плотности.

boxCox(mod_r)
#Нужен логарифм вроде
mod2 <- update(mod_r, log(Price) ~ . )
v <- vif(mod2)
v

mod3 <- stepAIC(mod2)

summary(mod3)
plot(mod3, 2)
#Стало значительно лучше. Даже странно. Но в некоторых работах по рынку можно найти схожий R2

#________________________________________
#Спецификация
plot(mod3, which = 1) 
#Какая-то жесть
resettest(mod3)
#p-value оч маленький, а значит не норм 
crPlots(mod3)

#Можем исправить Price
resettest(mod3, power = 2)
#p-value большой, а значит  норм 
resettest(mod3, power = 3)
#p-value мал, а значит не норм 

mod4 <- update(mod3, . ~ . + log(scaledprice) - scaledprice)
summary(mod4)
resettest(mod4, power = 3)
#p-value уже побольше
crPlots(mod4)
#По графиккам всё норм


#________________________________________
#Гетероскедастичность - одинакова ли станд.ошибка
plot(mod4, which = 3)
#Тест Бреуша-Пагана
library(lmtest)
bptest(mod4)
#H0 - гетероскедастичности нет при p-value большом выполняется
#Гетеро <- есть (неоднородность остатков)
#Перейдём к робастным ошибкам в форме Вайта
cov_white <- vcovHC(mod4, type = "HC0")
coeftest(mod4, cov_white)
#Ничего не изменилось

best_model_1 <- mod4
summary(best_model_1)

stargazer(best_model_1, type = 'text', df = FALSE, 
          digits =2, title = '.',
          out = 'best_model_1.txt')

#Попробуем сделать индекс

#Надо сделать столбец с годом = периодом
#Нужен reference artist - Andy Warhol
Art <- as.data.frame(table(data$Artist))

#1)Надо сделать столбец с годом = периодом
#group.by, summarise_all, функция среднего геометрического
#coef(best_model_1) * на среднее значение для каждого художника
