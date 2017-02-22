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
oilPriceDate.ts <- ts(oilPriceDate$price, start = 1, frequency = 12) #frequency -> saisonnality time is 1 year
plot(oilPriceDate.ts)


######Classe zoo
oilPriceDate.zoo <- zoo(oilPriceDate$price, order.by = oilPriceDate$Date)
plot(oilPriceDate.zoo)

######Essentials stats
mean(oilPriceDate$price)
sd(oilPriceDate$price)
summary(oilPriceDate$price)
boxplot(oilPriceDate$price)
hist(oilPriceDate$price)

######Trend
#mobile mean

mb<-filter(oilPriceDate$price, filter = array(1/50, dim = 50), method = c("convolution"),
           sides = 2, circular = FALSE)
mb<-xts(mb, order.by = oilPriceDate$Date)
plot(oilPriceDate$Date, oilPriceDate$price, type = 'l')
lines(mb, col = 'red')

#Differenciation by autocorrelation

oilPriceDate.ts <- ts(oilPriceDate$price, start = 1, frequency = 27) #frequency -> saisonnality time is 1 year
plot(oilPriceDate.ts)
Acf(oilPriceDate.ts, na.action = na.omit)
diff.oilPriceDate.ts <- diff(oilPriceDate.ts, lag = 1, differences = 1) 
Acf(diff.oilPriceDate.ts, na.action = na.omit)
plot(oilPriceDate.ts)
plot(diff.oilPriceDate.ts)

#parametrique trend

time <- c(1:nrow(oilPriceDate))
oilPriceDate$time <- time
reg <- lm(price ~ time + I(time^2) + I(time^3) + I(time^4), data = oilPriceDate) 
summary(reg)
#par(mfrow = c(1, 2))
plot(oilPriceDate$Date, oilPriceDate$price, type = "l", xlab = "months",
     ylab = "Ind. Price .Oil .In .London (INSEE)", col = "blue")
lines(oilPriceDate$Date, reg$fitted, col = "red", lwd = 2)
plot(oilPriceDate$Date, oilPriceDate$price - reg$fitted, type = "l",
     xlab = "months", ylab = "Ind. Prix. - detrend", col = "orangered2")
####Courbe rouge - courbe bleu

#Kernel estimation

noyau <- ksmooth(oilPriceDate$time, oilPriceDate$price, kernel = c("normal"), bandwidth = 10)
#par(mfrow = c(1, 2))
plot(oilPriceDate$Date, oilPriceDate$price, type = "l", xlab = "",
     ylab = "Ind. Price. Brent. London (INSEE)", col = "blue")
lines(oilPriceDate$Date, noyau$y, col = "red", lwd = 2)
plot(oilPriceDate$Date, oilPriceDate$price - noyau$y, type = "l",
     xlab = "", ylab = "Ind. Prix. - detrend", col = "orangered2")

#local Polynomial estimation

graphics.off()
loc<-loess (price ~ time, dat=oilPriceDate, degree=2, span=0.5) #span sur le pourcentages de valeurs prises dans la fenêtre
loc2<-loess (price ~ time, dat=oilPriceDate, degree=2, span=0.2)
loc3<-loess (price ~ time, dat=oilPriceDate, degree=1, span=0.2)
plot(oilPriceDate$Date,oilPriceDate$price, type='l',xlab="months", ylab="Oil barril price(INSEE)", col= "blue")
lines(oilPriceDate$Date, loc$fitted, col='red', lwd=2)
lines(oilPriceDate$Date, loc2$fitted, col='orange', lwd=2)
lines(oilPriceDate$Date, loc3$fitted, col='green', lwd=2)

#################Seasonality per year

## Enlever la tendance

#mobile mean 
##saionnalité sur 1 an
MA<-filter(oilPriceDate$price, filter = array(1/(27), dim = 27), method = c("convolution"),
           sides = 2, circular = FALSE)
MA2<-filter(oilPriceDate$price, filter = array(1/(27*6), dim = 27*6), method = c("convolution"),
           sides = 2, circular = FALSE)
##suite aux schémas, on observe une saisonnalité sur 6 ans 

plot(oilPriceDate$Date, oilPriceDate$price - loc2$fitted, type = 'l')
lines(oilPriceDate$Date,MA - loc2$fitted, col = 'blue')
lines(oilPriceDate$Date,MA2 - loc2$fitted, col = 'orange')


######differenciation
par(mfrow = c(1, 2))
acf(oilPriceDate.ts)
acf(diff(oilPriceDate.ts, lag = 5, differences = 1))



