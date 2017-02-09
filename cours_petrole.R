rm(list=objects())
library(zoo)
library(timeDate)
library(forecast)
library(xts)
library(dygraphs)

######Data import
setwd(dir = "/Users/Kilian/Programmation/ENSTA_2A/R_project/MAP_STA2/Projet/oil-price-evolution/Valeurs")
oilPriceValue<-read.table("Valeurs.csv", sep = ';', dec = ",", skip = 1)
head(oilPriceValue)
plot(oilPriceValue)

######Data Frame
dateEnd <- strptime(c("01/01/1990"), "%m/%d/%Y")
dateStart <- strptime(c("12/01/2016"), "%m/%d/%Y")
Date <- seq(dateStart, dateEnd, by = "-1 month")
oilPriceDate<-data.frame(Date, oilPriceValue$V3)
names(oilPriceDate)<-c("Date", "price")
plot(oilPriceDate$Date, oilPriceDate$price, type = 'l')

######Classe ts
oilPriceDate.ts <- ts(oilPriceDate$price, start=1, frequency=12) #frequency -> saisonnality time is 1 year
plot(oilPriceDate.ts)


#####Classe zoo
oilPriceDate.zoo <- zoo(oilPriceDate$price, order.by = oilPriceDate$Date)
plot(oilPriceDate.zoo)

######Essentials stats
mean(oilPriceDate$price)
sd(oilPriceDate$price)
summary(oilPriceDate$price)
boxplot(oilPriceDate$price)
hist(oilPriceDate$price)
