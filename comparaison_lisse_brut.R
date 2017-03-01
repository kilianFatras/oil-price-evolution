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

######Data Frame
dateEnd <- strptime(c("01/01/1990"), "%m/%d/%Y")
dateStart <- strptime(c("12/01/2016"), "%m/%d/%Y")
Date <- seq(dateStart, dateEnd, by = "-1 month")
oilPriceDate<-data.frame(Date, oilPriceValue$V3)
names(oilPriceDate)<-c("Date", "price")
plot(oilPriceDate$Date, oilPriceDate$price, type = 'l')

oilPriceDate_lisse<-oilPriceDate


######Smooth data and avoid choc

j = 0
for (i in 118:92) {
  oilPriceDate_lisse$price[i] = (oilPriceDate$price[91] - oilPriceDate$price[117])/27 * j + oilPriceDate$price[117]
  j = j + 1
}

plot(oilPriceDate_lisse$Date, oilPriceDate_lisse$price, type = 'l', main = "évolution du prix du baril de pétrole lissé linéairement", xlab = "Date", ylab = "Prix du baril")


#Lissage de Holt-Winters avec les donn?es brutes et sur 48 mois
oilTs <- oilPriceDate$price[324:36]
oilPriceDate.ts2<- ts(oilTs, start = 1, frequency = 12) 
plot(oilPriceDate.ts2,type='l')
hw <-  HoltWinters(oilPriceDate.ts2, alpha = NULL, beta = NULL, gamma = NULL,
                   seasonal = c("additive"),
                   start.periods = 2, l.start = NULL, b.start = NULL,
                   s.start = NULL,
                   optim.start = c(alpha = 0.3, beta = 0.1, gamma = 0.1),
                   optim.control = list())
plot(hw)
prediction<-predict(hw, n.ahead =48 , prediction.interval = TRUE, level = 0.95)
plot(hw, prediction, main = "pr?diction sur les donn?es futures (donn?es brutes) horizon : 48 mois", xlab = "Temps (en ann?es)", ylab = "Prix du baril")


#Lissage de Holt-Winters avec les donn?es liss?es et sur 48 mois
oilTs2 <- oilPriceDate_lisse$price[324:36]
oilPriceDate.ts3<- ts(oilTs2, start = 1, frequency = 12) 
plot(oilPriceDate.ts3,type='l')
hw2 <-  HoltWinters(oilPriceDate.ts3, alpha = NULL, beta = NULL, gamma = NULL,
                   seasonal = c("additive"),
                   start.periods = 2, l.start = NULL, b.start = NULL,
                   s.start = NULL,
                   optim.start = c(alpha = 0.3, beta = 0.1, gamma = 0.1),
                   optim.control = list())
plot(hw2)
prediction2<-predict(hw2, n.ahead =48 , prediction.interval = TRUE, level = 0.95)
plot(hw2, prediction2, main = "pr?diction sur les donn?es futures (donn?es liss?es) horizon : 48 mois", xlab = "Temps (en ann?es)", ylab = "Prix du baril")

