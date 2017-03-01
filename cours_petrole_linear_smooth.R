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


######Smooth data and avoid choc

j = 0
for (i in 118:92) {
  oilPriceDate$price[i] = (oilPriceDate$price[91] - oilPriceDate$price[117])/27 * j + oilPriceDate$price[117]
  j = j + 1
}

plot(oilPriceDate$Date, oilPriceDate$price, type = 'l', main = "?volution du prix du baril de p?trole liss? lin?airement", xlab = "Date", ylab = "Prix du baril")

######Classe ts
oilTs = oilPriceDate$price[324:1]
oilPriceDate.ts <- ts(oilTs, start = 1, frequency = 12) #frequency -> saisonnality time is 1 year
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
plot(oilPriceDate$Date, oilPriceDate$price, type = 'l', main = "Moyenne mobile données lissées", xlab = "Date", ylab = "Prix du baril")
lines(mb, col = 'red')

#Differenciation by autocorrelation

plot(oilPriceDate.ts)
Acf(oilPriceDate.ts, na.action = na.omit)
diff.oilPriceDate.ts <- diff(oilPriceDate.ts, lag = 1, differences = 1) 
Acf(diff.oilPriceDate.ts, na.action = na.omit)
plot(oilPriceDate.ts)
plot(diff.oilPriceDate.ts, main = "évolution du prix du baril de pétrole lissé linéairement", xlab = "Date", ylab = "Prix du baril")

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
plot(oilPriceDate$Date,oilPriceDate$price, type='l', main = "tendance par polynômes locaux", xlab="months", ylab="Oil barril price(INSEE)", col= "blue")
lines(oilPriceDate$Date, loc$fitted, col='red', lwd=2)
lines(oilPriceDate$Date, loc2$fitted, col='orange', lwd=2)
lines(oilPriceDate$Date, loc3$fitted, col='green', lwd=2)

#mobile mean 
##saionality 1 year

seasonPart = oilPriceDate$price - loc2$fitted 
#la partie saisonnière est la série à laquelle on enlève sa tendance
MA<-filter(seasonPart, filter = array(1/(12), dim = 12), method = c("convolution"),
           sides = 2, circular = FALSE)
MA2<-filter(seasonPart, filter = array(1/(12*2), dim = 12*2), method = c("convolution"),
            sides = 2, circular = FALSE)
MA3<-filter(seasonPart, filter = array(1/(12*2*5), dim = 12*2*5), method = c("convolution"),
            sides = 2, circular = FALSE)



##second saisonality 6 year 

plot(oilPriceDate$Date, oilPriceDate$price - loc2$fitted, type = 'l',main = "Saisonnalité des données lissées linéairement", xlab = "Date", ylab = "Prix du baril")
lines(oilPriceDate$Date,MA - loc2$fitted, col = 'blue')
lines(oilPriceDate$Date,MA2 - loc2$fitted, col = 'orange')


######differenciation
par(mfrow = c(1, 2))
acf(oilPriceDate.ts)
acf(diff(oilPriceDate.ts, lag = 5, differences = 1))



##Lissage exponentiel de Holt-Winters:
oilTs <- oilPriceDate$price[324:36]
oilPriceDate.ts2<- ts(oilTs, start = 1, frequency = 12) 
plot(oilPriceDate.ts2,type='l')
hw <-  HoltWinters(oilPriceDate.ts2, alpha = NULL, beta = NULL, gamma = NULL,
                   seasonal = c("additive"),
                   start.periods = 2, l.start = NULL, b.start = NULL,
                   s.start = NULL,
                   optim.start = c(alpha = 0.3, beta = 0.1, gamma = 0.1),
                   optim.control = list())
plot(fitted(hw))
plot(hw)
prediction<-predict(hw, n.ahead =48 , prediction.interval = TRUE, level = 0.95)
plot(hw, prediction, main = "prediction on futur data : 48 months", xlab = "Time (year)", ylab = "Prix du baril")

