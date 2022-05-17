library(dplyr)
library(nnet)
library(tidyr)
library(corrplot)
library(stringr)
library(stargazer)
library(tidyverse)
library(readxl)
library(priceR)
library(openxlsx)
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

#Импорт данных
Data <- dir("~/Desktop/Diploma/Данные/Artists_1", full.names = T) %>% map_df(read_excel)
DataAdd <- dir("~/Desktop/Diploma/Данные/Artists_2", full.names = T) %>% map_df(read_excel)
#Вместе - 346 000 наблюдений
DataDataNew <- rbind(Data, DataAdd)

####Первичная обработка####
#Переименуем столбцы
colnames(DataDataNew) = c('All_Info', 'Artist', 'Title', 'Medium','cm', 'Signed', 'Stamped', 
                   'Inscribed', 'Dating', 'Lot_Number', 'Transcation_Day',
                   'Auction house', 'Price_estimate', 'Price', 'Bill/Invoice', 'Edition', 
                   'Foundry', 'Impression')
#Разделим столбец 'Price' на валюту продажи и цену продажи
Data1 <- DataDataNew %>% separate(Price, c('Price', 'Currency'),  sep = "\\s+")
Data1$Price <- as.numeric(gsub(",","", Data1$Price))
#Разделим столбец 'Price etimate на валюту продажи и цену продажи
Data1$Price_estimate <- as.character(gsub("-", "", Data1$Price_estimate))
n_last <- 3 
Data1$Price_estimate_Currency <- substr(Data1$Price_estimate, nchar(Data1$Price_estimate) - n_last + 1, nchar(Data1$Price_estimate))
Data1$Price_estimate <- substr(Data1$Price_estimate,1,nchar(Data1$Price_estimate)-3)
Data11 <- Data1 %>% separate(Price_estimate, c('Floor_price', 'Ceiling_Price'),  sep = "\\s+")
Data11$Floor_price <- as.numeric(gsub(",","", Data11$Floor_price))
Data11$Ceiling_Price <- as.numeric(gsub(",","", Data11$Ceiling_Price))
#Разделим размер картин: cm
Data11$cm <- as.character(gsub(")", "", Data11$cm))
Data11$cm <- as.character(gsub("in", "", Data11$cm))
Data11$cm <- as.character(gsub("cm", "", Data11$cm))
Data11$cm <- as.character(gsub("x", "", Data11$cm))
Data12 <- Data11 %>% separate(cm, c('in_wide', 'in_long', 'cm_wide', 'cm_long'),  sep = "\\s+")
Data12$cm_wide <- sub('.', '', Data12$cm_wide)
Data12$in_wide <- as.numeric(Data12$in_wide)
Data12$in_long <- as.numeric(Data12$in_long)
Data12$cm_wide <- as.numeric(Data12$cm_wide)
Data12$cm_long <- as.numeric(Data12$cm_long)

###Medium####
#Оставлять будем только это:
#Etching
#Ink
#Lithograph
#Oil
#Print
#Woodcut

Data12 <- data.frame(Data12)
#Проверка
Table <- table(Data12$Medium)
View(Table)
a <- names(Table)[Table>1000]
#Oil
Data12[,4] <- ifelse(Data12[,4] == a[26], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == a[27], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == a[28], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == a[29], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == a[30], 'Oil', Data12[,4])
#Etching = ink screenprint
Data12[,4] <- ifelse(Data12[,4] == a[11], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == a[12], 'Etching', Data12[,4])
#Pencil
Data12[,4] <- ifelse(Data12[,4] == a[33], 'Pencil', Data12[,4])
#Acrylic
Data12[,4] <- ifelse(Data12[,4] == a[1], 'Acrylic', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == a[2], 'Acrylic', Data12[,4])
#Screenprint
Data12[,4] <- ifelse(Data12[,4] == a[38], 'Screenprint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == a[39], 'Screenprint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == a[40], 'Screenprint', Data12[,4])
#Watercolor
Data12[,4] <- ifelse(Data12[,4] == a[46], 'Watercolor', Data12[,4])
#Lithograph = oil and water screenprint
Data12[,4] <- ifelse(Data12[,4] == a[20], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == a[21], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == a[22], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == a[23], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == a[24], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == a[6], 'Lithograph', Data12[,4])
#Ink
Data12[,4] <- ifelse(Data12[,4] == a[17], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == a[18], 'Ink', Data12[,4])
#Gouache
Data12[,4] <- ifelse(Data12[,4] == a[15], 'Gouache', Data12[,4])

#Проверка
Table <- table(Data12$Medium)
b <- names(Table)[Table>500]
#Lithograph
Data12[,4] <- ifelse(Data12[,4] == b[7], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == b[21], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == b[22], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == b[23], 'Lithograph', Data12[,4])
#Serigraph = silk screenprint
Data12[,4] <- ifelse(Data12[,4] == b[42], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == b[43], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == b[44], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == b[45], 'Serigraph', Data12[,4])
#Etching
Data12[,4] <- ifelse(Data12[,4] == b[15], 'Etching', Data12[,4])
#Screenprint
Data12[,4] <- ifelse(Data12[,4] == b[8], 'Screenprint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == b[38], 'Screenprint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == b[39], 'Screenprint', Data12[,4])
#Oil
Data12[,4] <- ifelse(Data12[,4] == b[28], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == b[29], 'Oil', Data12[,4])

#Проверка
Table <- table(Data12$Medium)
c <- names(Table)[Table>500]
#Mixed Media
Data12[,4] <- ifelse(Data12[,4] == c[19], 'Mixed Media', Data12[,4])
#Crayon
Data12[,4] <- ifelse(Data12[,4] == c[8], 'Crayon', Data12[,4])
#Graphite
Data12[,4] <- ifelse(Data12[,4] == c[15], 'Graphite', Data12[,4])
#Charcoal
Data12[,4] <- ifelse(Data12[,4] == c[6], 'Charcoal', Data12[,4])
#Pastel
Data12[,4] <- ifelse(Data12[,4] == c[23], 'Pastel', Data12[,4])
#Pen and ink
Data12[,4] <- ifelse(Data12[,4] == c[24], 'Pen and ink', Data12[,4])

#Проверка
Table <- table(Data12$Medium)
d <- names(Table)[Table>250]
#Acrylic
Data12[,4] <- ifelse(Data12[,4] == d[2], 'Acrylic', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == d[3], 'Acrylic', Data12[,4])
#Aquatint
Data12[,4] <- ifelse(Data12[,4] == d[6], 'Aquatint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == d[7], 'Aquatint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == d[8], 'Aquatint', Data12[,4])
#Bronze
Data12[,4] <- ifelse(Data12[,4] == d[10], 'Bronze', Data12[,4])
#Enamel
Data12[,4] <- ifelse(Data12[,4] == d[26], 'Enamel', Data12[,4])
#Etching
Data12[,4] <- ifelse(Data12[,4] == d[29], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == d[30], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == d[31], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == d[32], 'Etching', Data12[,4])
#Ink
Data12[,4] <- ifelse(Data12[,4] == d[37], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == d[38], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == d[39], 'Ink', Data12[,4])
#Lithograph
Data12[,4] <- ifelse(Data12[,4] == d[17], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == d[18], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == d[19], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == d[20], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == d[10], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == d[42], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == d[43], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == d[44], 'Lithograph', Data12[,4])
#Mixed Technique
Data12[,4] <- ifelse(Data12[,4] == d[46], 'Mixed Technique', Data12[,4])
#Oil
Data12[,4] <- ifelse(Data12[,4] == d[49], 'Oil', Data12[,4])
#Offset
Data12[,4] <- ifelse(Data12[,4] == d[21], 'Offset', Data12[,4])
#Painting
Data12[,4] <- ifelse(Data12[,4] == d[51], 'Painting', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == d[52], 'Painting', Data12[,4])
#Pen and ink
Data12[,4] <- ifelse(Data12[,4] == d[56], 'Pen and ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == d[55], 'Pen and ink', Data12[,4])
#Pen
Data12[,4] <- ifelse(Data12[,4] == d[57], 'Pen', Data12[,4])
#Pencil
Data12[,4] <- ifelse(Data12[,4] == d[59], 'Pencil', Data12[,4])
#Print
Data12[,4] <- ifelse(Data12[,4] == d[64], 'Print', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == d[65], 'Print', Data12[,4])
#Serigraph = silk screenprint
Data12[,4] <- ifelse(Data12[,4] == d[69], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == d[70], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == d[71], 'Serigraph', Data12[,4])
#Watercolor
Data12[,4] <- ifelse(Data12[,4] == d[75], 'Watercolor', Data12[,4])
#Woodcut
Data12[,4] <- ifelse(Data12[,4] == d[22], 'Woodcut', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == d[76], 'Woodcut', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == d[78], 'Woodcut', Data12[,4])

#Проверка
Table <- table(Data12$Medium)
e <- names(Table)[Table>100]
#Acrylic
Data12[,4] <- ifelse(Data12[,4] == e[2], 'Acrylic', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[3], 'Acrylic', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[4], 'Acrylic', Data12[,4])
#Aquatint
Data12[,4] <- ifelse(Data12[,4] == e[7], 'Aquatint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[8], 'Aquatint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[9], 'Aquatint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[10], 'Aquatint', Data12[,4])
#Bronze
Data12[,4] <- ifelse(Data12[,4] == e[12], 'Bronze', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[13], 'Bronze', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[14], 'Bronze', Data12[,4])
#C-print
Data12[,4] <- ifelse(Data12[,4] == e[28], 'C-print', Data12[,4])
#Chalk
Data12[,4] <- ifelse(Data12[,4] == e[20], 'Chalk', Data12[,4])
#Cibachrome
Data12[,4] <- ifelse(Data12[,4] == e[24], 'Cibachrome', Data12[,4])
#Collage
Data12[,4] <- ifelse(Data12[,4] == e[26], 'Collage', Data12[,4])
#Drawing
Data12[,4] <- ifelse(Data12[,4] == e[35], 'Drawing', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[36], 'Drawing', Data12[,4])
#Drypoint
Data12[,4] <- ifelse(Data12[,4] == e[38], 'Drypoint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[39], 'Drypoint', Data12[,4])
#Etching
Data12[,4] <- ifelse(Data12[,4] == e[43], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[44], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[45], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[46], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[47], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[48], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[49], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[50], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[51], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[52], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[53], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[54], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[55], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[56], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[57], 'Etching', Data12[,4])
#Gouache
Data12[,4] <- ifelse(Data12[,4] == e[61], 'Gouache', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[62], 'Gouache', Data12[,4])
#Ink
Data12[,4] <- ifelse(Data12[,4] == e[67], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[68], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[69], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[70], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[71], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[72], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[73], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[74], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[75], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[76], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[77], 'Ink', Data12[,4])
#Lead
Data12[,4] <- ifelse(Data12[,4] == e[78], 'Lead', Data12[,4])
#Lithograph
Data12[,4] <- ifelse(Data12[,4] == e[22], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[29], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[30], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[81], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[82], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[83], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[84], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[85], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[86], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[87], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[88], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[89], 'Lithograph', Data12[,4])
#Mixed Media
Data12[,4] <- ifelse(Data12[,4] == e[93], 'Mixed Media', Data12[,4])
#Offset
Data12[,4] <- ifelse(Data12[,4] == e[97], 'Offset', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[98], 'Offset', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[99], 'Offset', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[100], 'Offset', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[101], 'Offset', Data12[,4])
#Oil
Data12[,4] <- ifelse(Data12[,4] == e[103], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[104], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[105], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[106], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[107], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[108], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[109], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[110], 'Oil', Data12[,4])
#Pastel
Data12[,4] <- ifelse(Data12[,4] == e[115], 'Pastel', Data12[,4])
#Pen and ink
Data12[,4] <- ifelse(Data12[,4] == e[117], 'Pen and ink', Data12[,4])
#Pen
Data12[,4] <- ifelse(Data12[,4] == e[58], 'Pen', Data12[,4])
#Pencil
Data12[,4] <- ifelse(Data12[,4] == e[120], 'Pencil', Data12[,4])
#Print
Data12[,4] <- ifelse(Data12[,4] == e[130], 'Print', Data12[,4])
#Reproduction
Data12[,4] <- ifelse(Data12[,4] == e[132], 'Reproduction', Data12[,4])
#Screenprint
Data12[,4] <- ifelse(Data12[,4] == e[31], 'Screenprint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[135], 'Screenprint', Data12[,4])
#Sculpture
Data12[,4] <- ifelse(Data12[,4] == e[137], 'Sculpture', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[138], 'Sculpture', Data12[,4])
#Serigraph = silk screenprint
Data12[,4] <- ifelse(Data12[,4] == e[140], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[141], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[142], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[143], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[144], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[145], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[146], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[147], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[148], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[149], 'Serigraph', Data12[,4])
#Tempera
Data12[,4] <- ifelse(Data12[,4] == e[154], 'Tempera', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[155], 'Tempera', Data12[,4])
#Watercolor
Data12[,4] <- ifelse(Data12[,4] == e[159], 'Watercolor', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[160], 'Watercolor', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[161], 'Watercolor', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[162], 'Watercolor', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[163], 'Watercolor', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[164], 'Watercolor', Data12[,4])
#Woodcut
Data12[,4] <- ifelse(Data12[,4] == e[166], 'Woodcut', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[167], 'Woodcut', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == e[168], 'Woodcut', Data12[,4])

#Проверка
Table <- table(Data12$Medium)
f <- names(Table)[Table>50]
#Acrylic
Data12[,4] <- ifelse(Data12[,4] == f[2], 'Acrylic', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[3], 'Acrylic', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[4], 'Acrylic', Data12[,4])
#Aquatint
Data12[,4] <- ifelse(Data12[,4] == f[7], 'Aquatint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[8], 'Aquatint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[9], 'Aquatint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[10], 'Aquatint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[11], 'Aquatint', Data12[,4])
#Bronze
Data12[,4] <- ifelse(Data12[,4] == f[15], 'Bronze', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[16], 'Bronze', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[17], 'Bronze', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[18], 'Bronze', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[19], 'Bronze', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[20], 'Bronze', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[21], 'Bronze', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[22], 'Bronze', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[23], 'Bronze', Data12[,4])
#Ceramic
Data12[,4] <- ifelse(Data12[,4] == f[29], 'Ceramic', Data12[,4])
#Chalk
Data12[,4] <- ifelse(Data12[,4] == f[31], 'Chalk', Data12[,4])
#Charcoal
Data12[,4] <- ifelse(Data12[,4] == f[33], 'Charcoal', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[34], 'Charcoal', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[35], 'Charcoal', Data12[,4])
#Drrypoint
Data12[,4] <- ifelse(Data12[,4] == f[48], 'Drrypoint', Data12[,4])
#Enamel
Data12[,4] <- ifelse(Data12[,4] == f[51], 'Enamel', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[52], 'Enamel', Data12[,4])
#Engraving
Data12[,4] <- ifelse(Data12[,4] == f[54], 'Engraving', Data12[,4])
#Etching
Data12[,4] <- ifelse(Data12[,4] == f[56], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[57], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[58], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[59], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[60], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[61], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[62], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[63], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[64], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[65], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[66], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[67], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[68], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[69], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[70], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[71], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[72], 'Etching', Data12[,4])
#Goache
Data12[,4] <- ifelse(Data12[,4] == f[76], 'Goache', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[77], 'Goache', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[78], 'Goache', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[79], 'Goache', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[80], 'Goache', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[81], 'Goache', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[82], 'Goache', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[83], 'Goache', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[84], 'Goache', Data12[,4])
#Ink
Data12[,4] <- ifelse(Data12[,4] == f[24], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[89], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[90], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[91], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[92], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[93], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[94], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[95], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[96], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[97], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[98], 'Ink', Data12[,4])
#Linocut
Data12[,4] <- ifelse(Data12[,4] == f[102], 'Linocut', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[103], 'Linocut', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[104], 'Linocut', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[105], 'Linocut', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[106], 'Linocut', Data12[,4])
#Lithograph
Data12[,4] <- ifelse(Data12[,4] == f[39], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[40], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[41], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[42], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[43], 'Lithograph', Data12[,4])

Data12[,4] <- ifelse(Data12[,4] == f[109], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[110], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[111], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[112], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[113], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[114], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[115], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[116], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[117], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[118], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[119], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[120], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[121], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[122], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[123], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[124], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[125], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[126], 'Lithograph', Data12[,4])
#Marker
Data12[,4] <- ifelse(Data12[,4] == f[128], 'Marker', Data12[,4])
#Mixed Technique
Data12[,4] <- ifelse(Data12[,4] == f[132], 'Mixed Technique', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[133], 'Mixed Technique', Data12[,4])
#Offset
Data12[,4] <- ifelse(Data12[,4] == f[136], 'Offset', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[137], 'Offset', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[138], 'Offset', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[139], 'Offset', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[140], 'Offset', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[141], 'Offset', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[142], 'Offset', Data12[,4])
#Oil
Data12[,4] <- ifelse(Data12[,4] == f[144], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[145], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[146], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[147], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[148], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[149], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[150], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[151], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[152], 'Oil', Data12[,4])
#Painting
Data12[,4] <- ifelse(Data12[,4] == f[154], 'Painting', Data12[,4])
#Pastel
Data12[,4] <- ifelse(Data12[,4] == f[158], 'Pastel', Data12[,4])
#Pen and ink
Data12[,4] <- ifelse(Data12[,4] == f[161], 'Pen and ink', Data12[,4])
#Pen
Data12[,4] <- ifelse(Data12[,4] == f[12], 'Pen', Data12[,4])
#Pencil
Data12[,4] <- ifelse(Data12[,4] == f[163], 'Pencil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[164], 'Pencil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[165], 'Pencil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[166], 'Pencil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[167], 'Pencil', Data12[,4])
#Photograph
Data12[,4] <- ifelse(Data12[,4] == f[169], 'Photograph', Data12[,4])
#Pochoir
Data12[,4] <- ifelse(Data12[,4] == f[178], 'Pochoir', Data12[,4])
#Polymer Paint
Data12[,4] <- ifelse(Data12[,4] == f[181], 'Polymer Paint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[182], 'Polymer Paint', Data12[,4])
#Print
Data12[,4] <- ifelse(Data12[,4] == f[186], 'Print', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[187], 'Print', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[188], 'Print', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[189], 'Print', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[190], 'Print', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[213], 'Print', Data12[,4])
#Reproduction
Data12[,4] <- ifelse(Data12[,4] == f[192], 'Reproduction', Data12[,4])
#Sanguine
Data12[,4] <- ifelse(Data12[,4] == f[194], 'Sanguine', Data12[,4])
#Screenprint
Data12[,4] <- ifelse(Data12[,4] == f[196], 'Screenprint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[197], 'Screenprint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[198], 'Screenprint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[199], 'Screenprint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[200], 'Screenprint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[201], 'Screenprint', Data12[,4])
#Serigraph = silk screenprint
Data12[,4] <- ifelse(Data12[,4] == f[204], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[205], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[206], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[207], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[208], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[209], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[210], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[211], 'Serigraph', Data12[,4])
#Steel
Data12[,4] <- ifelse(Data12[,4] == f[214], 'Steel', Data12[,4])
#Tempera
Data12[,4] <- ifelse(Data12[,4] == f[218], 'Tempera', Data12[,4])
#Unknown
Data12[,4] <- ifelse(Data12[,4] == f[221], 'Unknown', Data12[,4])
#Watercolor
Data12[,4] <- ifelse(Data12[,4] == f[223], 'Watercolor', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[224], 'Watercolor', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[225], 'Watercolor', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[226], 'Watercolor', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[227], 'Watercolor', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[228], 'Watercolor', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[229], 'Watercolor', Data12[,4])
#Woodcut
Data12[,4] <- ifelse(Data12[,4] == f[230], 'Woodcut', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[232], 'Woodcut', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[233], 'Woodcut', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == f[234], 'Woodcut', Data12[,4])

#Проверка
Table <- table(Data12$Medium)
g <- names(Table)[Table>25]
#Acrylic
Data12[,4] <- ifelse(Data12[,4] == g[2], 'Acrylic', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[3], 'Acrylic', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[4], 'Acrylic', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[5], 'Acrylic', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[6], 'Acrylic', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[7], 'Acrylic', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[8], 'Acrylic', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[9], 'Acrylic', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[10], 'Acrylic', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[11], 'Acrylic', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[12], 'Acrylic', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[13], 'Acrylic', Data12[,4])
#Aquatint
Data12[,4] <- ifelse(Data12[,4] == g[16], 'Aquatint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[17], 'Aquatint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[18], 'Aquatint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[19], 'Aquatint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[20], 'Aquatint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[21], 'Aquatint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[22], 'Aquatint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[23], 'Aquatint', Data12[,4])
#Bronze
Data12[,4] <- ifelse(Data12[,4] == g[28], 'Bronze', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[29], 'Bronze', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[30], 'Bronze', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[31], 'Bronze', Data12[,4])
#Ceramic
Data12[,4] <- ifelse(Data12[,4] == g[38], 'Ceramic', Data12[,4])
#Chalk
Data12[,4] <- ifelse(Data12[,4] == g[40], 'Chalk', Data12[,4])
#Crayon
Data12[,4] <- ifelse(Data12[,4] == g[347], 'Crayon', Data12[,4])
#Charcoal
Data12[,4] <- ifelse(Data12[,4] == g[42], 'Charcoal', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[43], 'Charcoal', Data12[,4])
#Collage
Data12[,4] <- ifelse(Data12[,4] == g[46], 'Charcoal', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[47], 'Charcoal', Data12[,4])
#Crayon
Data12[,4] <- ifelse(Data12[,4] == g[64], 'Crayon', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[65], 'Crayon', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[66], 'Crayon', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[67], 'Crayon', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[68], 'Crayon', Data12[,4])
#Drawing
Data12[,4] <- ifelse(Data12[,4] == g[70], 'Drawing', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[71], 'Drawing', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[72], 'Drawing', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[73], 'Drawing', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[74], 'Drawing', Data12[,4])
#Drypoint
Data12[,4] <- ifelse(Data12[,4] == g[75], 'Drypoint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[77], 'Drypoint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[78], 'Drypoint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[79], 'Drypoint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[80], 'Drypoint', Data12[,4])
#Embroidery
Data12[,4] <- ifelse(Data12[,4] == g[83], 'Embroidery', Data12[,4])
#Enamel
Data12[,4] <- ifelse(Data12[,4] == g[85], 'Enamel', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[86], 'Enamel', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[87], 'Enamel', Data12[,4])
#Engraving
Data12[,4] <- ifelse(Data12[,4] == g[89], 'Engraving', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[90], 'Engraving', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[91], 'Engraving', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[92], 'Engraving', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[93], 'Engraving', Data12[,4])
#Etching
Data12[,4] <- ifelse(Data12[,4] == g[95], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[96], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[97], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[98], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[99], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[100], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[101], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[102], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[103], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[104], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[105], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[106], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[107], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[108], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[109], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[110], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[111], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[112], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[113], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[114], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[115], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[116], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[117], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[118], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[119], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[120], 'Etching', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[121], 'Etching', Data12[,4])
#Feather
Data12[,4] <- ifelse(Data12[,4] == g[122], 'Feather', Data12[,4])
#Gouache
Data12[,4] <- ifelse(Data12[,4] == g[129], 'Gouache', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[130], 'Gouache', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[131], 'Gouache', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[132], 'Gouache', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[133], 'Gouache', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[134], 'Gouache', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[135], 'Gouache', Data12[,4])
#Ink
Data12[,4] <- ifelse(Data12[,4] == g[141], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[142], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[143], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[144], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[145], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[146], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[147], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[148], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[149], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[150], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[151], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[152], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[153], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[154], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[155], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[156], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[157], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[158], 'Ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[159], 'Ink', Data12[,4])
#Kaolin
Data12[,4] <- ifelse(Data12[,4] == g[161], 'Ink', Data12[,4])
#Linocut
Data12[,4] <- ifelse(Data12[,4] == g[164], 'Linocut', Data12[,4])
#Linoleum
Data12[,4] <- ifelse(Data12[,4] == g[166], 'Linoleum', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[167], 'Linoleum', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[168], 'Linoleum', Data12[,4])
#Lithograph
Data12[,4] <- ifelse(Data12[,4] == g[49], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[50], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[51], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[52], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[53], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[54], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[55], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[56], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[57], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[170], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[171], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[172], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[173], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[174], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[175], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[176], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[177], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[178], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[179], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[180], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[181], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[182], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[183], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[184], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[185], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[186], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[187], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[188], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[189], 'Lithograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[190], 'Lithograph', Data12[,4])
#Mixed Media
Data12[,4] <- ifelse(Data12[,4] == g[196], 'Mixed Media', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[197], 'Mixed Media', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[198], 'Mixed Media', Data12[,4])
#Mixed Technique
Data12[,4] <- ifelse(Data12[,4] == g[200], 'Mixed Technique', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[201], 'Mixed Technique', Data12[,4])
#Mixograph
Data12[,4] <- ifelse(Data12[,4] == g[203], 'Mixograph', Data12[,4])
#Monotype
Data12[,4] <- ifelse(Data12[,4] == g[203], 'Monotype', Data12[,4])
#Offset
Data12[,4] <- ifelse(Data12[,4] == g[58], 'Offset', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[59], 'Offset', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[207], 'Offset', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[208], 'Offset', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[209], 'Offset', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[210], 'Offset', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[211], 'Offset', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[212], 'Offset', Data12[,4])
#Oil
Data12[,4] <- ifelse(Data12[,4] == g[214], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[215], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[216], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[217], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[218], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[219], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[220], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[221], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[222], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[223], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[224], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[225], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[226], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[227], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[228], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[229], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[230], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[231], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[232], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[233], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[234], 'Oil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[235], 'Oil', Data12[,4])
#Painting
Data12[,4] <- ifelse(Data12[,4] == g[237], 'Painting', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[238], 'Painting', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[239], 'Painting', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[240], 'Painting', Data12[,4])
#Pastel
Data12[,4] <- ifelse(Data12[,4] == g[244], 'Pastel', Data12[,4])
#Pen and ink
Data12[,4] <- ifelse(Data12[,4] == g[246], 'Pen and ink', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[249], 'Pen and ink', Data12[,4])
#Pen
Data12[,4] <- ifelse(Data12[,4] == g[25], 'Pen', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[123], 'Pen', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[248], 'Pen', Data12[,4])
#Pencil
Data12[,4] <- ifelse(Data12[,4] == g[251], 'Pencil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[252], 'Pencil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[253], 'Pencil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[254], 'Pencil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[255], 'Pencil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[256], 'Pencil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[257], 'Pencil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[258], 'Pencil', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[259], 'Pencil', Data12[,4])
#Photograph
Data12[,4] <- ifelse(Data12[,4] == g[262], 'Photograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[263], 'Photograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[264], 'Photograph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[265], 'Photograph', Data12[,4])
#Pigment
Data12[,4] <- ifelse(Data12[,4] == g[269], 'Pigment', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[270], 'Pigment', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[271], 'Pigment', Data12[,4])
#Pochoir
Data12[,4] <- ifelse(Data12[,4] == g[277], 'Pochoir', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[278], 'Pochoir', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[279], 'Pochoir', Data12[,4])
#Print
Data12[,4] <- ifelse(Data12[,4] == g[287], 'Print', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[288], 'Print', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[289], 'Print', Data12[,4])
#Reproduction
Data12[,4] <- ifelse(Data12[,4] == g[291], 'Reproduction', Data12[,4])
#Ripolin
Data12[,4] <- ifelse(Data12[,4] == g[293], 'Ripolin', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[294], 'Ripolin', Data12[,4])
#Screenprint
Data12[,4] <- ifelse(Data12[,4] == g[60], 'Screenprint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[61], 'Screenprint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[297], 'Screenprint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[298], 'Screenprint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[299], 'Screenprint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[300], 'Screenprint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[301], 'Screenprint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[302], 'Screenprint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[303], 'Screenprint', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[304], 'Screenprint', Data12[,4])
#Serigraph = silk screenprint
Data12[,4] <- ifelse(Data12[,4] == g[307], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[308], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[309], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[310], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[311], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[312], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[313], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[314], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[315], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[316], 'Serigraph', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[317], 'Serigraph', Data12[,4])
#Sculpture
Data12[,4] <- ifelse(Data12[,4] == g[348], 'Sculpture', Data12[,4])
#Tempera
Data12[,4] <- ifelse(Data12[,4] == g[323], 'Tempera', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[324], 'Tempera', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[325], 'Tempera', Data12[,4])
#Unknown
Data12[,4] <- ifelse(Data12[,4] == g[328], 'Unknown', Data12[,4])
#Watercolor
Data12[,4] <- ifelse(Data12[,4] == g[332], 'Watercolor', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[333], 'Watercolor', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[334], 'Watercolor', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[335], 'Watercolor', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[336], 'Watercolor', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[337], 'Watercolor', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[338], 'Watercolor', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[339], 'Watercolor', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[340], 'Watercolor', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[341], 'Watercolor', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[342], 'Watercolor', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[343], 'Watercolor', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[344], 'Watercolor', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[345], 'Watercolor', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[346], 'Watercolor', Data12[,4])
#Woodcut
Data12[,4] <- ifelse(Data12[,4] == g[350], 'Woodcut', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[351], 'Woodcut', Data12[,4])
Data12[,4] <- ifelse(Data12[,4] == g[352], 'Woodcut', Data12[,4])

#Проверка
Table <- table(Data12$Medium)
h <- names(Table)[Table>10]
View(h)

####Валюта####
#Работаем с Price_estimate_Currency
Currency <- class.ind(Data12$Price_estimate_Currency)
Table <- table(Data12$Price_estimate_Currency)
a <- names(Table)[Table>100]
Data13 <- filter(Data12, Price_estimate_Currency %in% a)
View(table(Data13$Price_estimate_Currency))
#Добавляем DKK, CAD, JPY, CNY, NOK, MXN

#########################################AUD
dataAUD <- dplyr::filter(Data13, Price_estimate_Currency == 'AUD')
#dataSEK <- dataSEK[order(dataSEK$Currency),]
dataAUD <- dataAUD[-c(165, 661, 779, 2462, 2952, 2994, 3324), ]
table(dataAUD$Currency)
w <- dataAUD$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
curAUD <- historical_exchange_rates("AUD", to = "USD",
                                    start_date = "2000-01-01", end_date = "2021-09-30")
AUD_TO_USD <- merge(w3, curAUD, by = 'date')
#Расставить данные в dataAUD  по возрастанию
dataAUD$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataAUD1 <- dataAUD[order(dataAUD$Transcation_Day),]
#Уберём даты, где NA
dfAUD <- dataAUD1 %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfAUD$Price_USD <- dfAUD$Price * AUD_TO_USD$one_AUD_equivalent_to_x_USD
dfAUD$Floor_price_USD <- dfAUD$Floor_price * AUD_TO_USD$one_AUD_equivalent_to_x_USD
dfAUD$Ceiling_Price_USD <- dfAUD$Ceiling_Price * AUD_TO_USD$one_AUD_equivalent_to_x_USD
#Получили скорректированный датасет - dfAUD

#########################################CHF
?historical_exchange_rates

dataCHF <- dplyr::filter(Data13, Price_estimate_Currency == 'CHF')
dataCHF <- dataCHF[-c(9112, 6676, 588, 9553, 375, 2231, 4572, 4596, 4647, 4683, 4742, 4958, 5020, 
                      5035, 5093, 5427, 5481, 5486, 5570, 9160, 10563, 10972, 11008,
                      9030, 1091, 9020), ]
table(dataCHF$Currency)
w <- dataCHF$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("CHF", to = "USD",
                                    start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataCHF  по возрастанию
dataCHF$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataCHF1 <- dataCHF[order(dataCHF$Transcation_Day),]
#Уберём даты, где NA
dfCHF <- dataCHF1 %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfCHF$Price_USD <- dfCHF$Price * TO_USD$one_CHF_equivalent_to_x_USD
dfCHF$Floor_price_USD <- dfCHF$Floor_price * TO_USD$one_CHF_equivalent_to_x_USD
dfCHF$Ceiling_Price_USD <- dfCHF$Ceiling_Price * TO_USD$one_CHF_equivalent_to_x_USD
#Получили скорректированный датасет - dfCHF

#########################################SEK
dataSEK <- dplyr::filter(Data13, Price_estimate_Currency == 'SEK')
dataSEK <- dataSEK[-c(2591, 6609, 5670, 7274, 1747, 9020), ]
table(dataSEK$Currency)
w <- dataSEK$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("SEK", to = "USD",
                                 start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataSEK  по возрастанию
dataSEK$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataSEK1 <- dataSEK[order(dataSEK$Transcation_Day),]
#Уберём даты, где NA
dfSEK <- dataSEK1 %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfSEK$Price_USD <- dfSEK$Price * TO_USD$one_SEK_equivalent_to_x_USD
dfSEK$Floor_price_USD <- dfSEK$Floor_price * TO_USD$one_SEK_equivalent_to_x_USD
dfSEK$Ceiling_Price_USD <- dfSEK$Ceiling_Price * TO_USD$one_SEK_equivalent_to_x_USD
#Получили скорректированный датасет - dfSEK

#########################################HKD 
dataHKD  <- dplyr::filter(Data13, Price_estimate_Currency == 'HKD')
dataHKD <- dataHKD[-c(290, 294, 503, 521, 1336, 2431, 2449, 2597, 3330, 5894, 5895, 5896, 5897, 5898, 5899, 
                      5900, 5901, 5905, 5919, 5921, 5922, 5923, 5924, 5925, 5926, 5927, 5928, 5929, 5930,
                      6117, 7307, 7964, 8503, 8506, 9157, 10609, 10886, 13875, 4925, 12084, 4814), ]
table(dataHKD$Currency)
w <- dataHKD$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("HKD", to = "USD",
                                 start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataHKD  по возрастанию
dataHKD$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataHKD1 <- dataHKD[order(dataHKD$Transcation_Day),]
#Уберём даты, где NA
dfHKD <- dataHKD1 %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfHKD$Price_USD <- dfHKD$Price * TO_USD$one_HKD_equivalent_to_x_USD
dfHKD$Floor_price_USD <- dfHKD$Floor_price * TO_USD$one_HKD_equivalent_to_x_USD
dfHKD$Ceiling_Price_USD <- dfHKD$Ceiling_Price * TO_USD$one_HKD_equivalent_to_x_USD
#Получили скорректированный датасет - dfHKD

#########################################GBP 
dataGBP  <- dplyr::filter(Data13, Price_estimate_Currency == 'GBP')
dataGBP <- dataGBP[-c(16861, 22586, 51270, 51279, 51282, 51284, 53162, 21095, 38361,
                      38399, 39491, 52414, 53446, 54184, 58754, 34362, 46637, 48137, 49906, 54838, 
                      60266, 60380, 2958, 55025, 25601, 50910, 9575, 31997, 41474, 41495,
                      43143, 49528, 52517, 54777, 57226, 2966, 7222, 15037, 37253, 47878,
                      49433, 51347, 6262, 536, 569, 587, 638, 769, 772, 2502, 6928, 6933, 7512,
                      37319, 43430, 43975, 45331, 46178, 46180, 48681, 49799, 49870, 58077, 59328), ]
table(dataGBP$Currency)
w <- dataGBP$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("GBP", to = "USD",
                                 start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataGBP  по возрастанию
dataGBP$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataGBP1 <- dataGBP[order(dataGBP$Transcation_Day),]
#Уберём даты, где NA
dfGBP <- dataGBP1 %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfGBP$Price_USD <- dfGBP$Price * TO_USD$one_GBP_equivalent_to_x_USD
dfGBP$Floor_price_USD <- dfGBP$Floor_price * TO_USD$one_GBP_equivalent_to_x_USD
dfGBP$Ceiling_Price_USD <- dfGBP$Ceiling_Price * TO_USD$one_GBP_equivalent_to_x_USD
#Получили скорректированный датасет - dfGBP

#########################################EUR 
dataEUR  <- dplyr::filter(Data13, Price_estimate_Currency == 'EUR')
dataEUR <- dataEUR[-c(53066, 76499, 90169, 22935, 49870, 75558, 93497, 17949, 18904,
                      22963, 28149, 28849, 36267, 36292, 40742, 47610, 49076, 50160,
                      55775, 67445, 75240, 92619, 96124, 3604, 3738, 4072, 14519, 14974,
                      75994, 95404, 16355, 21801, 1729, 27720, 36717, 40848, 41154,
                      52881, 71591, 73629, 85454, 96733, 97917, 98836, 98842, 100034,
                      18745, 64149, 94857, 32488, 606, 7108, 7679, 10588, 11328,
                      24285, 24374, 24463, 24549, 24895, 24962, 25066, 25289, 25439, 25489,
                      68635, 75023, 228, 242, 252, 621, 683, 4625, 6463, 16095, 19127,
                      22254, 37544, 42790, 44213, 44250, 44254, 50039, 51563, 51897,
                      52213, 60835, 61797, 63277, 64211, 64213, 68687, 68712, 70070,
                      70083, 70099, 85765, 86972, 88332, 88354, 88460, 88491, 88916,
                      92313, 92331, 95761, 98729), ]

table(dataEUR$Currency)
w <- dataEUR$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("EUR", to = "USD",
                                 start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataEUR  по возрастанию
dataEUR$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataEUR1 <- dataEUR[order(dataEUR$Transcation_Day),]
#Уберём даты, где NA
dfEUR <- dataEUR1 %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfEUR$Price_USD <- dfEUR$Price * TO_USD$one_EUR_equivalent_to_x_USD
dfEUR$Floor_price_USD <- dfEUR$Floor_price * TO_USD$one_EUR_equivalent_to_x_USD
dfEUR$Ceiling_Price_USD <- dfEUR$Ceiling_Price * TO_USD$one_EUR_equivalent_to_x_USD
#Получили скорректированный датасет - dfEUR

#########################################DKK
dataDKK  <- dplyr::filter(Data13, Price_estimate_Currency == 'DKK')
dataDKK <- dataDKK[-c(19,	149,	165,	345,	354,	413,	417,	420,	426,	573,
                      589,	592,	653,	822,	823,	824,	825,	841,	1029,	1119,
                      1127,	1128,	1150,	1152,	1159,	1170,	1179,	1185,	1204,	1218,
                      1366,	1370,	1376,	1400,	1405,	1437,	1508,	1544, 158, 185, 255, 844), ]

table(dataDKK$Currency)
w <- dataDKK$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("DKK", to = "USD",
                                 start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataDKK  по возрастанию
dataDKK$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataDKK1 <- dataDKK[order(dataDKK$Transcation_Day),]
#Уберём даты, где NA
dfDKK <- dataDKK1 %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfDKK$Price_USD <- dfDKK$Price * TO_USD$one_DKK_equivalent_to_x_USD
dfDKK$Floor_price_USD <- dfDKK$Floor_price * TO_USD$one_DKK_equivalent_to_x_USD
dfDKK$Ceiling_Price_USD <- dfDKK$Ceiling_Price * TO_USD$one_DKK_equivalent_to_x_USD
#Получили скорректированный датасет - dfDKK

#########################################CAD
dataCAD  <- dplyr::filter(Data13, Price_estimate_Currency == 'CAD')
dataCAD <- dataCAD[-c(1216, 1217, 1290, 684, 301), ]

table(dataCAD$Currency)
w <- dataCAD$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("CAD", to = "USD",
                                 start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataCAD  по возрастанию
dataCAD$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataCAD <- dataCAD[order(dataCAD$Transcation_Day),]
#Уберём даты, где NA
dfCAD <- dataCAD %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfCAD$Price_USD <- dfCAD$Price * TO_USD$one_CAD_equivalent_to_x_USD
dfCAD$Floor_price_USD <- dfCAD$Floor_price * TO_USD$one_CAD_equivalent_to_x_USD
dfCAD$Ceiling_Price_USD <- dfCAD$Ceiling_Price * TO_USD$one_CAD_equivalent_to_x_USD
#Получили скорректированный датасет - dfCAD

#########################################JPY
dataJPY  <- dplyr::filter(Data13, Price_estimate_Currency == 'JPY')
table(dataJPY$Currency)
w <- dataJPY$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("JPY", to = "USD",
                                 start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataJPY  по возрастанию
dataJPY$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataJPY <- dataJPY[order(dataJPY$Transcation_Day),]
#Уберём даты, где NA
dfJPY <- dataJPY %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfJPY$Price_USD <- dfJPY$Price * TO_USD$one_JPY_equivalent_to_x_USD
dfJPY$Floor_price_USD <- dfJPY$Floor_price * TO_USD$one_JPY_equivalent_to_x_USD
dfJPY$Ceiling_Price_USD <- dfJPY$Ceiling_Price * TO_USD$one_JPY_equivalent_to_x_USD
#Получили скорректированный датасет - dfJPY

#########################################CNY
dataCNY  <- dplyr::filter(Data13, Price_estimate_Currency == 'CNY')
dataCNY <- dataCNY[-c(341, 469, 826, 259), ]

table(dataCNY$Currency)
w <- dataCNY$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("CNY", to = "USD",
                                 start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataCNY  по возрастанию
dataCNY$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataCNY <- dataCNY[order(dataCNY$Transcation_Day),]
#Уберём даты, где NA
dfCNY <- dataCNY %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfCNY$Price_USD <- dfCNY$Price * TO_USD$one_CNY_equivalent_to_x_USD
dfCNY$Floor_price_USD <- dfCNY$Floor_price * TO_USD$one_CNY_equivalent_to_x_USD
dfCNY$Ceiling_Price_USD <- dfCNY$Ceiling_Price * TO_USD$one_CNY_equivalent_to_x_USD
#Получили скорректированный датасет - dfCNY

#########################################NOK
dataNOK  <- dplyr::filter(Data13, Price_estimate_Currency == 'NOK')
dataNOK <- dataNOK[-c(179, 219, 238, 257, 300, 412), ]

table(dataNOK$Currency)
w <- dataNOK$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("NOK", to = "USD",
                                 start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataNOK  по возрастанию
dataNOK$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataNOK <- dataNOK[order(dataNOK$Transcation_Day),]
#Уберём даты, где NA
dfNOK <- dataNOK %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfNOK$Price_USD <- dfNOK$Price * TO_USD$one_NOK_equivalent_to_x_USD
dfNOK$Floor_price_USD <- dfNOK$Floor_price * TO_USD$one_NOK_equivalent_to_x_USD
dfNOK$Ceiling_Price_USD <- dfNOK$Ceiling_Price * TO_USD$one_NOK_equivalent_to_x_USD
#Получили скорректированный датасет - dfNOK

#########################################MXN
dataMXN  <- dplyr::filter(Data13, Price_estimate_Currency == 'MXN')
dataMXN <- dataMXN[-c(607, 740), ]

table(dataMXN$Currency)
w <- dataMXN$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("MXN", to = "USD",
                                 start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataMXN  по возрастанию
dataMXN$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataMXN <- dataMXN[order(dataMXN$Transcation_Day),]
#Уберём даты, где NA
dfMXN <- dataMXN %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfMXN$Price_USD <- dfMXN$Price * TO_USD$one_MXN_equivalent_to_x_USD
dfMXN$Floor_price_USD <- dfMXN$Floor_price * TO_USD$one_MXN_equivalent_to_x_USD
dfMXN$Ceiling_Price_USD <- dfMXN$Ceiling_Price * TO_USD$one_MXN_equivalent_to_x_USD
#Получили скорректированный датасет - dfMXN

#########################################INR
dataINR  <- dplyr::filter(Data13, Price_estimate_Currency == 'INR')
table(dataINR$Currency)
w <- dataINR$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("INR", to = "USD",
                                 start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataINR  по возрастанию
dataINR$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataINR <- dataINR[order(dataINR$Transcation_Day),]
#Уберём даты, где NA
dfINR <- dataINR %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfINR$Price_USD <- dfINR$Price * TO_USD$one_INR_equivalent_to_x_USD
dfINR$Floor_price_USD <- dfINR$Floor_price * TO_USD$one_INR_equivalent_to_x_USD
dfINR$Ceiling_Price_USD <- dfINR$Ceiling_Price * TO_USD$one_INR_equivalent_to_x_USD
#Получили скорректированный датасет - dfINR

#########################################SGD
dataSGD  <- dplyr::filter(Data13, Price_estimate_Currency == 'SGD')
dataSGD <- dataSGD[-c(153, 400, 117), ]

table(dataSGD$Currency)
w <- dataSGD$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("SGD", to = "USD",
                                 start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataSGD  по возрастанию
dataSGD$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataSGD <- dataSGD[order(dataSGD$Transcation_Day),]
#Уберём даты, где NA
dfSGD <- dataSGD %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfSGD$Price_USD <- dfSGD$Price * TO_USD$one_SGD_equivalent_to_x_USD
dfSGD$Floor_price_USD <- dfSGD$Floor_price * TO_USD$one_SGD_equivalent_to_x_USD
dfSGD$Ceiling_Price_USD <- dfSGD$Ceiling_Price * TO_USD$one_SGD_equivalent_to_x_USD
#Получили скорректированный датасет - dfSGD

#########################################PLN
dataPLN  <- dplyr::filter(Data13, Price_estimate_Currency == 'PLN')
dataPLN <- dataPLN[-c(83), ]

table(dataPLN$Currency)
w <- dataPLN$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("PLN", to = "USD",
                                 start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataPLN  по возрастанию
dataPLN$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataPLN <- dataPLN[order(dataPLN$Transcation_Day),]
#Уберём даты, где NA
dfPLN <- dataPLN %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfPLN$Price_USD <- dfPLN$Price * TO_USD$one_PLN_equivalent_to_x_USD
dfPLN$Floor_price_USD <- dfPLN$Floor_price * TO_USD$one_PLN_equivalent_to_x_USD
dfPLN$Ceiling_Price_USD <- dfPLN$Ceiling_Price * TO_USD$one_PLN_equivalent_to_x_USD
#Получили скорректированный датасет - dfPLN

#########################################HUF
dataHUF  <- dplyr::filter(Data13, Price_estimate_Currency == 'HUF')
table(dataHUF$Currency)
w <- dataHUF$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("HUF", to = "USD",
                                 start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataHUF  по возрастанию
dataHUF$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataHUF <- dataHUF[order(dataHUF$Transcation_Day),]
#Уберём даты, где NA
dfHUF <- dataHUF %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfHUF$Price_USD <- dfHUF$Price * TO_USD$one_HUF_equivalent_to_x_USD
dfHUF$Floor_price_USD <- dfHUF$Floor_price * TO_USD$one_HUF_equivalent_to_x_USD
dfHUF$Ceiling_Price_USD <- dfHUF$Ceiling_Price * TO_USD$one_HUF_equivalent_to_x_USD
#Получили скорректированный датасет - dfHUF

#########################################TWD
dataTWD  <- dplyr::filter(Data13, Price_estimate_Currency == 'TWD')
table(dataTWD$Currency)
w <- dataTWD$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("TWD", to = "USD",
                                 start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataTWD  по возрастанию
dataTWD$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataTWD <- dataTWD[order(dataTWD$Transcation_Day),]
#Уберём даты, где NA
dfTWD <- dataTWD %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfTWD$Price_USD <- dfTWD$Price * TO_USD$one_TWD_equivalent_to_x_USD
dfTWD$Floor_price_USD <- dfTWD$Floor_price * TO_USD$one_TWD_equivalent_to_x_USD
dfTWD$Ceiling_Price_USD <- dfTWD$Ceiling_Price * TO_USD$one_TWD_equivalent_to_x_USD
#Получили скорректированный датасет - dfTWD

#########################################ZAR
dataZAR  <- dplyr::filter(Data13, Price_estimate_Currency == 'ZAR')
dataZAR <- dataZAR[-c(117), ]
table(dataZAR$Currency)
w <- dataZAR$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("ZAR", to = "USD",
                                 start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataZAR  по возрастанию
dataZAR$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataZAR <- dataZAR[order(dataZAR$Transcation_Day),]
#Уберём даты, где NA
dfZAR <- dataZAR %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfZAR$Price_USD <- dfZAR$Price * TO_USD$one_ZAR_equivalent_to_x_USD
dfZAR$Floor_price_USD <- dfZAR$Floor_price * TO_USD$one_ZAR_equivalent_to_x_USD
dfZAR$Ceiling_Price_USD <- dfZAR$Ceiling_Price * TO_USD$one_ZAR_equivalent_to_x_USD
#Получили скорректированный датасет - dfZAR


#########################################N/A
dataNA  <- dplyr::filter(Data12, Price_estimate_Currency == 'N/A')
Table <- table(dataNA$Currency)
r <- names(Table)[Table>70]
dataNA1 <- filter(dataNA, Currency %in% r)
table(dataNA1$Currency)
#N/A <- EUR
dataNAEUR  <- dplyr::filter(dataNA1, Currency == 'EUR')
table(dataNAEUR$Currency)
w <- dataNAEUR$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("EUR", to = "USD",
                                 start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataNAEUR  по возрастанию
dataNAEUR$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataNAEUR1 <- dataNAEUR[order(dataNAEUR$Transcation_Day),]
#Уберём даты, где NA
dfNAEUR <- dataNAEUR1 %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfNAEUR$Price_USD <- dfNAEUR$Price * TO_USD$one_EUR_equivalent_to_x_USD
dfNAEUR$Floor_price_USD <- dfNAEUR$Floor_price * TO_USD$one_EUR_equivalent_to_x_USD
dfNAEUR$Ceiling_Price_USD <- dfNAEUR$Ceiling_Price * TO_USD$one_EUR_equivalent_to_x_USD
#Получили скорректированный датасет - dfNAEUR
#N/A <- GBP
dataNAGBP  <- dplyr::filter(dataNA1, Currency == 'GBP')
table(dataNAGBP$Currency)
w <- dataNAGBP$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("GBP", to = "USD",
                                 start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataNAGBP  по возрастанию
dataNAGBP$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataNAGBP1 <- dataNAGBP[order(dataNAGBP$Transcation_Day),]
#Уберём даты, где NA
dfNAGBP <- dataNAGBP1 %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfNAGBP$Price_USD <- dfNAGBP$Price * TO_USD$one_GBP_equivalent_to_x_USD
dfNAGBP$Floor_price_USD <- dfNAGBP$Floor_price * TO_USD$one_GBP_equivalent_to_x_USD
dfNAGBP$Ceiling_Price_USD <- dfNAGBP$Ceiling_Price * TO_USD$one_GBP_equivalent_to_x_USD
#Получили скорректированный датасет - dfNAGBP
#N/A <- HKD
dataNAHKD  <- dplyr::filter(dataNA1, Currency == 'HKD')
table(dataNAHKD$Currency)
w <- dataNAHKD$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("HKD", to = "USD",
                                 start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataNAHKD  по возрастанию
dataNAHKD$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataNAHKD <- dataNAHKD[order(dataNAHKD$Transcation_Day),]
#Уберём даты, где NA
dfNAHKD <- dataNAHKD %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfNAHKD$Price_USD <- dfNAHKD$Price * TO_USD$one_HKD_equivalent_to_x_USD
dfNAHKD$Floor_price_USD <- dfNAHKD$Floor_price * TO_USD$one_HKD_equivalent_to_x_USD
dfNAHKD$Ceiling_Price_USD <- dfNAHKD$Ceiling_Price * TO_USD$one_HKD_equivalent_to_x_USD
#Получили скорректированный датасет - dfNAHKD
#N/A <- SEK
dataNASEK  <- dplyr::filter(dataNA1, Currency == 'SEK')
table(dataNASEK$Currency)
w <- dataNASEK$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("SEK", to = "USD",
                                 start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataNASEK  по возрастанию
dataNASEK$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataNASEK <- dataNASEK[order(dataNASEK$Transcation_Day),]
#Уберём даты, где NA
dfNASEK <- dataNASEK %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfNASEK$Price_USD <- dfNASEK$Price * TO_USD$one_SEK_equivalent_to_x_USD
dfNASEK$Floor_price_USD <- dfNASEK$Floor_price * TO_USD$one_SEK_equivalent_to_x_USD
dfNASEK$Ceiling_Price_USD <- dfNASEK$Ceiling_Price * TO_USD$one_SEK_equivalent_to_x_USD
#Получили скорректированный датасет - dfNASEK
#N/A <- CHF
dataNACHF  <- dplyr::filter(dataNA1, Currency == 'CHF')
table(dataNACHF$Currency)
w <- dataNACHF$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("CHF", to = "USD",
                                 start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataNACHF  по возрастанию
dataNACHF$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataNACHF <- dataNACHF[order(dataNACHF$Transcation_Day),]
#Уберём даты, где NA
dfNACHF <- dataNACHF %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfNACHF$Price_USD <- dfNACHF$Price * TO_USD$one_CHF_equivalent_to_x_USD
dfNACHF$Floor_price_USD <- dfNACHF$Floor_price * TO_USD$one_CHF_equivalent_to_x_USD
dfNACHF$Ceiling_Price_USD <- dfNACHF$Ceiling_Price * TO_USD$one_CHF_equivalent_to_x_USD
#Получили скорректированный датасет - dfNACHF
#N/A <- AUD
dataNAAUD  <- dplyr::filter(dataNA1, Currency == 'AUD')
table(dataNAAUD$Currency)
w <- dataNAAUD$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("AUD", to = "USD",
                                 start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataNAAUD  по возрастанию
dataNAAUD$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataNAAUD <- dataNAAUD[order(dataNAAUD$Transcation_Day),]
#Уберём даты, где NA
dfNAAUD <- dataNAAUD %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfNAAUD$Price_USD <- dfNAAUD$Price * TO_USD$one_AUD_equivalent_to_x_USD
dfNAAUD$Floor_price_USD <- dfNAAUD$Floor_price * TO_USD$one_AUD_equivalent_to_x_USD
dfNAAUD$Ceiling_Price_USD <- dfNAAUD$Ceiling_Price * TO_USD$one_AUD_equivalent_to_x_USD
#Получили скорректированный датасет - dfNAAUD
#N/A <- JPY
dataNAJPY  <- dplyr::filter(dataNA1, Currency == 'JPY')
table(dataNAJPY$Currency)
w <- dataNAJPY$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("JPY", to = "USD",
                                 start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataNAJPY  по возрастанию
dataNAJPY$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataNAJPY <- dataNAJPY[order(dataNAJPY$Transcation_Day),]
#Уберём даты, где NA
dfNAJPY <- dataNAJPY %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfNAJPY$Price_USD <- dfNAJPY$Price * TO_USD$one_JPY_equivalent_to_x_USD
dfNAJPY$Floor_price_USD <- dfNAJPY$Floor_price * TO_USD$one_JPY_equivalent_to_x_USD
dfNAJPY$Ceiling_Price_USD <- dfNAJPY$Ceiling_Price * TO_USD$one_JPY_equivalent_to_x_USD
#Получили скорректированный датасет - dfNAJPY
#N/A <- PLN
dataNAPLN  <- dplyr::filter(dataNA1, Currency == 'PLN')
table(dataNAPLN$Currency)
w <- dataNAPLN$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("PLN", to = "USD",
                                 start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataNAPLN  по возрастанию
dataNAPLN$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataNAPLN <- dataNAPLN[order(dataNAPLN$Transcation_Day),]
#Уберём даты, где NA
dfNAPLN <- dataNAPLN %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfNAPLN$Price_USD <- dfNAPLN$Price * TO_USD$one_PLN_equivalent_to_x_USD
dfNAPLN$Floor_price_USD <- dfNAPLN$Floor_price * TO_USD$one_PLN_equivalent_to_x_USD
dfNAPLN$Ceiling_Price_USD <- dfNAPLN$Ceiling_Price * TO_USD$one_PLN_equivalent_to_x_USD
#Получили скорректированный датасет - dfNAPLN
#N/A <- CZK
dataNACZK  <- dplyr::filter(dataNA1, Currency == 'CZK')
table(dataNACZK$Currency)
w <- dataNACZK$Transcation_Day
w2 <- as.Date(w, format = c("%m-%d-%Y"))
w3 <- data.frame(date = w2)
cur <- historical_exchange_rates("CZK", to = "USD",
                                 start_date = "2000-01-01", end_date = "2021-09-30")
TO_USD <- merge(w3, cur, by = 'date')
#Расставить данные в dataNACZK  по возрастанию
dataNACZK$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dataNACZK <- dataNACZK[order(dataNACZK$Transcation_Day),]
#Уберём даты, где NA
dfNACZK <- dataNACZK %>% drop_na(Transcation_Day)
#Перемножаем столбцы цены и оценок на ставки валюты
dfNACZK$Price_USD <- dfNACZK$Price * TO_USD$one_CZK_equivalent_to_x_USD
dfNACZK$Floor_price_USD <- dfNACZK$Floor_price * TO_USD$one_CZK_equivalent_to_x_USD
dfNACZK$Ceiling_Price_USD <- dfNACZK$Ceiling_Price * TO_USD$one_CZK_equivalent_to_x_USD
#Получили скорректированный датасет - dfNACZK

#USD
dataUSD  <- dplyr::filter(Data13, Price_estimate_Currency == 'USD')
dataUSD <- dataUSD[-c(99101, 36772, 747, 828, 854, 1146, 1152, 9467, 14039, 21972, 25965,
                      35009, 62587, 65089, 69550, 84228, 101916, 106598, 18164, 19054, 21201,
                      21924, 22188, 30972, 32094, 34020, 36917, 37617, 40098, 56099, 56113,
                      56121, 60959, 76498, 78449, 80656, 81838, 82900, 85266, 85313, 85361,
                      85916, 85918, 87055, 93809, 97779, 99675, 104342, 106564, 106578,
                      820, 1096, 1484, 1532, 7991, 18773, 18812, 18851, 19650, 19692, 19735,
                      19778, 19820, 19859, 22091, 31119, 55890, 64410, 77602, 92357, 99309,
                      100173, 23235, 48382, 4853, 66074, 74139, 44130, 39901, 59815, 73631,
                      76440, 81221, 90599, 98211, 10174, 27455, 35480, 36213, 100508, 59345), ]
table(dataUSD$Currency)
w <- dataUSD$Transcation_Day
#Расставить данные в dataNAGBP  по возрастанию
dataUSD$Transcation_Day <- as.Date(w, format = c("%m-%d-%Y")) 
dfUSD <- dataUSD[order(dataUSD$Transcation_Day),]
#Перемножаем столбцы цены и оценок на ставки валюты
dfUSD$Price_USD <- dfUSD$Price
dfUSD$Floor_price_USD <- dfUSD$Floor_price
dfUSD$Ceiling_Price_USD <- dfUSD$Ceiling_Price

#Теперь все датасеты надо объединить
Data_no_inflation <- rbind(dfAUD, dfCHF, dfEUR, dfGBP, dfHKD, dfSEK, dfDKK, dfCAD, dfJPY, dfCNY, dfNOK,
                           dfMXN, dfINR,dfSGD, dfPLN, dfHUF, dfTWD, dfZAR, 
                           dfNAEUR, dfNAGBP, dfNAHKD, dfNASEK,dfNACHF, dfNAAUD, dfNAJPY, dfNAPLN, dfNACZK,
                           dfUSD)

#Добавляем DKK, CAD, JPY, CNY, NOK, MXN, INR, SGD, PLN, HUF,TWD, ZAR
#Добавляем HKD, SEK, CHF, AUD, JPY, PLN, CZK

####Поправка на инфляцию####
?adjust_for_inflation
#Отделить год от даты
Data_no_inflation$Year <- as.Date(Data_no_inflation$Transcation_Day, format = c("%Y")) 
Data_no_inflation$Year = substr(Data_no_inflation$Year,1,nchar(Data_no_inflation$Year)-1)
#Скорректировать на годовую инфляцию
Data_no_inflation$Price_USD_Adj <- adjust_for_inflation(Data_no_inflation$Price_USD, 
                                                        Data_no_inflation$Year, "US", to_date = 2008)
Data_no_inflation$Floor_price_USD_Adj <- adjust_for_inflation(Data_no_inflation$Floor_price_USD, 
                                                        Data_no_inflation$Year, "US", to_date = 2008)
Data_no_inflation$Ceiling_Price_USD_Adj <- adjust_for_inflation(Data_no_inflation$Ceiling_Price_USD, 
                                                        Data_no_inflation$Year, "US", to_date = 2008)
#Пока скорректированные данные стоят в столбцах Ceiling_Price_USD и Floor_price_USD

####Переименуем всё лишнее Medium в Other####
#По предварительной оценке модели, важным останутся только: 
#Etching, Ink, Lithograph, Oil, Print, Woodcut
#Всё остальное в Other!
Data15 <- Data_no_inflation
names(Data15)
Table <- table(Data_no_inflation$Medium)
View(Table)
#Etchin = гравирование, Lithograph = Печатание с плоской поверхности камня
#Ink = рисование чернилами
Data15[,4] <- ifelse(Data15[,4] == 'Oil',  'Oil',
                     ifelse(Data15[,4] == 'Etching',  'Etching', 
                     ifelse(Data15[,4] == 'Ink',  'Ink',
                            ifelse(Data15[,4] == 'Lithograph',  'Lithograph',
                                   ifelse(Data15[,4] == 'Print',  'Print',
                                          ifelse(Data15[,4] == 'Screenprint',  'Screenprint	',
                                                 ifelse(Data15[,4] == 'Serigraph',  'Serigraph	',
                                                        ifelse(Data15[,4] == 'Watercolor',  'Watercolor	',
                                                               ifelse(Data15[,4] == 'Bronze',  'Bronze	',
                                                                      ifelse(Data15[,4] == 'Acrylic',  'Acrylic	',
                                                                             ifelse(Data15[,4] == 'Woodcut',  'Woodcut	',
                                                                                    ifelse(Data15[,4] == 'Pencil',  'Pencil	',
                                                 'Other'))))))))))))

####Уберём ненужные столбцы####
DataIdeas <-  subset(Data15, select = -c(All_Info, in_wide, in_long, Floor_price, Ceiling_Price, Price, 
                                         Edition, Foundry, Impression, Currency, Price_USD, Floor_price_USD,
                                         Ceiling_Price_USD, Year))

####Artist#####
DataIdeas$Years <- as.character(gsub('([a-zA-Z])', "", DataIdeas$Artist))
DataIdeas$Artist <- as.character(gsub('([0-9])', "", DataIdeas$Artist))
DataIdeas$Years <- as.character(gsub("[()]", "", DataIdeas$Years))

#write.xlsx(DataIdeas$Years, file = "Years4.xlsx")
DataIdeas$Years <- read_excel("Desktop/Diploma/Данные/Workings/Years4.xlsx")
DataIdeas <- DataIdeas %>%
  separate(Artist, c("Artist", "Country_of_Birth"), "(-)")
#write.xlsx(DataIdeas$Country_of_Birth, file = "Country_of_Birth2.xlsx")
DataIdeas$Country_of_Birth <- read_excel("Desktop/Diploma/Данные/Workings/Country_of_Birth2.xlsx")
#write.xlsx(DataIdeas$Artist, file = "Artist2.xlsx")
DataIdeas$Artist <- read_excel("Desktop/Diploma/Данные/Workings/Artist2.xlsx")

#Почистим страны от остатков имён
u <- names(table(DataIdeas$Country_of_Birth))
Country <- DataIdeas$Country_of_Birth
Country <- as.data.frame(Country)
str(Country)
Country[,1] <- ifelse(Country[,1] == u[9], 'United States of America', Country[,1])
Country[,1] <- ifelse(Country[,1] == u[15], 'Hungary', Country[,1])
Country[,1] <- ifelse(Country[,1] == u[17], 'Philippines', Country[,1])
Country[,1] <- ifelse(Country[,1] == u[24], 'China', Country[,1])
Country[,1] <- ifelse(Country[,1] == u[31], 'France', Country[,1])
Country[,1] <- ifelse(Country[,1] == u[46], 'China', Country[,1])
u <- names(table(Country$Country_of_Birth))
DataIdeas$Country_of_Birth <- Country

#Дополним пропавшие имена
y <- names(table(DataIdeas$Artist))
Artist <- DataIdeas$Artist
Artist <- as.data.frame(Artist)
Artist[,1] <- ifelse(Artist[,1] == y[205], 'Jean Pierre Cassigneul', Artist[,1])
Artist[,1] <- ifelse(Artist[,1] == y[450], 'Zao Wou-ki ', Artist[,1])
Artist[,1] <- ifelse(Artist[,1] == y[36], 'Anita Magsaysay-ho', Artist[,1])
Artist[,1] <- ifelse(Artist[,1] == y[27], 'Amrita Sher-gil', Artist[,1])
Artist[,1] <- ifelse(Artist[,1] == y[353], 'Richard Pousette-dart', Artist[,1])
Artist[,1] <- ifelse(Artist[,1] == y[93], 'Ding Yanyong', Artist[,1])
y <- names(table(DataIdeas$Artist))
DataIdeas$Artist <- Artist

#Поработаем с годами (уберём лишние -)
i <- names(table(DataIdeas$Years))
Years <- DataIdeas$Years
Years <- as.data.frame(Years)
Years[,1] <- ifelse(Years[,1] == i[1], '1952-2013', Years[,1])
Years[,1] <- ifelse(Years[,1] == i[2], '1943-', Years[,1])
Years[,1] <- ifelse(Years[,1] == i[3], '1902-1978', Years[,1])
Years[,1] <- ifelse(Years[,1] == i[4], '1887-1986', Years[,1])
Years[,1] <- ifelse(Years[,1] == i[260], '1914-2012', Years[,1])
Years[,1] <- ifelse(Years[,1] == i[271], '1917-2009', Years[,1])
Years[,1] <- ifelse(Years[,1] == i[280], '1921-2021', Years[,1])
DataIdeas$Years <- Years
i <- names(table(DataIdeas$Years))

DataIdeas <- as.data.frame(DataIdeas)

#Автосохранение
#write.xlsx(Data15, file = 'Data15.xlsx')

str(DataIdeas1)

####Auction_house####
#Auction_house <-  поделить, как medium на общие категории: Sotheby's и тп. 
DataIdeas1 <- DataIdeas %>% separate(Auction.house, c('Auction.house', 'Auction.country'),  sep = "-")
DataIdeas1 <- subset(DataIdeas1, select = -c(Auction.country))
#Пока страну удалим

#DataIdeas2 <- DataIdeas1 %>% separate(Auction.country, c('Auction.country', 'Auction.city'),  sep = ":")
Years <- DataIdeas1$Years
Years <- Years %>% separate(Years, c('Born', 'Die'),  sep = "-")
DataIdeas1$Born <- Years$Born
DataIdeas1$Die <- Years$Die
DataIdeas1 <- subset(DataIdeas1, select = -c(Years))

str(DataIdeas1)
Artist <- DataIdeas1$Artist
Artist$Artist <- as.character(Artist$Artist)
DataIdeas1$Artist <- Artist$Artist
Country_of_Birth <- DataIdeas1$Country_of_Birth
Country_of_Birth$Country_of_Birth <- as.character(Country_of_Birth$Country_of_Birth)
DataIdeas1$Country_of_Birth <- Country_of_Birth$Country_of_Birth

####Переименовать все столбцы####
Data16 <- DataIdeas1
colnames(Data16) = c('Artist', 'Country_of_birth', 'Title', 'Medium','cm_wide', 'cm_long', 'Signed', 'Stamped', 
                          'Inscribed', 'Dating', 'Lot_Number', 'Transcation_Day',
                          'Auction_house', 'Bill_Invoice', 'Currency','Price', 'Floor_Price', 
                     'Ceiling_Price', 'Born', 'Die')
#332 679 наблюдений (потеряно 10к из-за Transaction day) 

#write.xlsx(Data16, file = 'Data_16.xlsx')
#Добавим новые переменные
#Переменная <- годы между рождением и смертью (death effect)

####Size####
Data16 <- Data_16
Data16$Size <- Data16$cm_wide * Data16$cm_long
Data16 <- subset(Data16, select = -c(cm_wide, cm_long))

####Estimate####
Data16$Estimate <- (Data16$Ceiling_Price - Data16$Floor_Price)/2
Data16 <- subset(Data16, select = -c(Ceiling_Price, Floor_Price))


####Alive####
View(Data16)
str(Data16)
Data16$Year <- Data16$Transcation_Day
Data16 <- Data16 %>% separate(Year, c('Year', 'Mon', 'Day'),  sep = "-")
names(Data16)
Data16$Die2 <- as.numeric(Data16$Die)
Data16$Year <- as.numeric(Data16$Year)
#Все данные по Die были поправлены, если человек не умер = NA
Data16$Die2[is.na(Data16$Die2)] <- 10000
Data16$Alive <- ifelse(Data16$Die2 <= Data16$Year, 0, 1)
Data16 <- subset(Data16, select = c(-Year, -Mon, -Day, - Die2))


####Auction####

names(Data16)
Table <- table(Data16$Auction_house)
View(Data16)
Data16[,11] <- ifelse(Data16[,11] == 'Christies',  'Christies', 
                     ifelse(Data16[,11] == 'Sothebys',  'Sothebys',
                            ifelse(Data16[,11] == 'Swann Galleries',  'Swann',
                                   'Other')))
#write.xlsx(Data16, 'Data16_Final.xlsx')



