rm(list=objects())
graphics.off()
library(zoo)
library(timeDate)
library(forecast)
library(xts)
library(dygraphs)
library(magrittr)
library(utils)
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
oilPriceDate.ts<-oilPriceDate.ts[324:1]
plot(oilPriceDate.ts, type='l')


######Classe zoo
oilPriceDate.zoo <- zoo(oilPriceDate$price, order.by = oilPriceDate$Date)
plot(oilPriceDate.zoo)

######Essentials stats
mean(oilPriceDate$price)
sd(oilPriceDate$price)
summary(oilPriceDate$price)
par(mfrow = c(1, 2))
boxplot(oilPriceDate$price, main = "boite ? moustache")
hist(oilPriceDate$price, main = "histogramme des prix", xlab = "Prix")

######Trend
#mobile mean
par(mfrow = c(1, 1))
mb<-filter(oilPriceDate$price, filter = array(1/50, dim = 50), method = c("convolution"),
           sides = 2, circular = FALSE)
mb<-xts(mb, order.by = oilPriceDate$Date)
plot(oilPriceDate$Date, oilPriceDate$price, type = 'l', main = "Moyenne mobile données brutes", xlab = "Date", ylab = "Prix du baril")
lines(mb, col = 'red')

#Differenciation by autocorrelation
plot(oilPriceDate.ts, type='l')
Acf(oilPriceDate.ts, na.action = na.omit)
diff.oilPriceDate.ts <- diff(oilPriceDate.ts, lag = 1, differences = 1) 
Acf(diff.oilPriceDate.ts, na.action = na.omit)
par(mfrow = c(1, 2))
plot(oilPriceDate.ts, type='l')
plot(diff.oilPriceDate.ts, main = "Evolution du prix du baril de p?trole brutes", xlab = "Date", ylab = "Prix du baril", type='l')


#parametrique trend

time <- c(1:nrow(oilPriceDate))
oilPriceDate$time <- time
reg <- lm(price ~ time + I(time^2) + I(time^3) + I(time^4), data = oilPriceDate) 
summary(reg)
par(mfrow = c(1, 2))
plot(oilPriceDate$Date, oilPriceDate$price, type = "l", xlab = "months",
     ylab = "Ind. Price .Oil .In .London (INSEE)", col = "blue")
lines(oilPriceDate$Date, reg$fitted, col = "red", lwd = 2)
plot(oilPriceDate$Date, oilPriceDate$price - reg$fitted, type = "l",
     xlab = "months", ylab = "Ind. Prix. - detrend", col = "orangered2")
####Courbe rouge - courbe bleu

reg <- lm(price ~ time + I(time^2) + I(time^3) + I(time^4), data = oilPriceDate) 
summary(reg)
par(mfrow = c(1, 2))
plot(oilPriceDate$Date, oilPriceDate$price, type = "l", xlab = "months",
     ylab = "Ind. Price .Oil .In .London (INSEE)", col = "blue")
lines(oilPriceDate$Date, reg$fitted, col = "red", lwd = 2)
plot(oilPriceDate$Date, oilPriceDate$price - reg$fitted, type = "l",
     xlab = "months", ylab = "Ind. Prix. - detrend", col = "orangered2")

reg2 <- lm(price ~ time + I(time^2) + I(time^3), data = oilPriceDate) 
summary(reg2)
plot(oilPriceDate$Date, oilPriceDate$price, type = "l", main="Comparaison regressions polynomiales ordres 3 et 4", xlab = "months",
     ylab = "Ind. Price .Oil .In .London (INSEE)", col = "blue")
lines(oilPriceDate$Date, reg$fitted, col = "red", lwd = 2)
lines(oilPriceDate$Date, reg2$fitted, col = "green", lwd = 2)
plot(oilPriceDate$Date, oilPriceDate$price - reg$fitted, type = "l",
     xlab = "months", ylab = "Ind. Prix. - detrend", col = "orangered2")
#Kernel estimation

noyau <- ksmooth(oilPriceDate$time, oilPriceDate$price, kernel = c("normal"), bandwidth = 10)

plot(oilPriceDate$Date, oilPriceDate$price, type = "l", xlab = "",
     ylab = "Ind. Price. Brent. London (INSEE)", col = "blue")
lines(oilPriceDate$Date, noyau$y, col = "red", lwd = 2)
plot(oilPriceDate$Date, oilPriceDate$price - noyau$y, type = "l",
     xlab = "", ylab = "Ind. Prix. - detrend", col = "orangered2")

#local Polynomial estimation
par(mfrow = c(1, 1))
loc<-loess (price ~ time, dat=oilPriceDate, degree=2, span=0.5) #span sur le pourcentages de valeurs prises dans la fenêtre
loc2<-loess (price ~ time, dat=oilPriceDate, degree=2, span=0.2)
loc3<-loess (price ~ time, dat=oilPriceDate, degree=1, span=0.2)
plot(oilPriceDate$Date,oilPriceDate$price, type='l', main = "tendance par polynomes locaux des donn?es brutes", xlab="months", ylab="Oil barril price(INSEE)", col= "blue")
lines(oilPriceDate$Date, loc$fitted, col='red', lwd=2)
lines(oilPriceDate$Date, loc2$fitted, col='orange', lwd=2)
lines(oilPriceDate$Date, loc3$fitted, col='green', lwd=2)

#################Seasonality per year

#mobile mean 
##saionality 1 year

seasonPart = oilPriceDate$price - loc2$fitted 
#la partie saisonni?re est la s?rie ? laquelle on enl?ve sa tendance
MA<-filter(seasonPart, filter = array(1/(12), dim = 12), method = c("convolution"),
           sides = 2, circular = FALSE)
MA2<-filter(seasonPart, filter = array(1/(12*2), dim = 12*2), method = c("convolution"),
            sides = 2, circular = FALSE)
MA3<-filter(seasonPart, filter = array(1/(12*2*5), dim = 12*2*5), method = c("convolution"),
            sides = 2, circular = FALSE)


##Lissage exponentiel:
graphics.off()
#lissage simple:
expSmooth=function(x,alpha)
{
  xsmooth<-x
  n<-length(x)
  for (i in c(2:n))
  {
    xsmooth[i]<-(1-alpha)*xsmooth[i-1]+alpha*x[i]# xsmooth[i] correspond ? y^t+1  /t
  }
  xsmooth<-c(x[1],head(xsmooth,n-1))#d?calage de 1 de la s?rie, parce que xsmooth[i] correspond ? y^t+1  /t
  return (xsmooth)
}

alpha<-0.9
X1<-oilPriceDate$price
X1<-X1[324:1] #pour remettre les valeurs dans le bon sens
X1.smooth<-expSmooth(X1,alpha)

plot(X1,type='l',main="comparaison des lissages pour alpha=0.9(rouge) et alpha=0.1 (bleu)")
lines(X1.smooth, col='red')
alpha2<-0.1
X1.smooth2<-expSmooth(X1,alpha2)
lines(X1.smooth2, col='blue')

#Trouver le alpha optimal
mean((X1-X1.smooth)^2)
alpha<-seq(0.00,0.95,length=100)
forecast<-lapply(alpha, expSmooth, x=X1)
erreur<-unlist(lapply(forecast,function(x){mean((X1-x)^2)}))
plot(alpha, erreur, type='l')
alpha_opt<-which.min(erreur)
points(alpha[alpha_opt],erreur[alpha_opt],col='red', pch=20,cex=2)

text(alpha[alpha_opt],erreur[alpha_opt]+0.5, labels=alpha[alpha_opt]%>%round(digits=3))
alphaX1=alpha[alpha_opt]
#on trouve alphaX2 (donc l'optimal)=0.95; on peut ensuite effectuer le lissage avec alpha=0.95. Ici on ne le fait pas car la courbe obtenue sera ? la fois tr?s proche de la courbe des prix et tr?s proche de la courbe rouge ?tudi?e (calcul?e avec alpha=0.9).


#lissage double
#h est l'horizon
#horizon 1 mois

DoubleExpSmooth=function(x,alpha)
{
  xsmooth=x
  n<-length(x)
  l<-array(x[1],dim=length(x))
  b<-array(x[2]-x[1], dim=length(x)) #initialisation
  n1<-h+1
  for (i in c(n1:n))
  {
    l[i]<-l[i-1]+b[i-1]+(1-(1-alpha)^2)*(x[i]-xsmooth[i-1])
    b[i]=b[i-1]+(x[i]-xsmooth[i-1])*alpha^2
    xsmooth[i]=l[i]+b[i]*h
  }
  res<-list()
  res$smooth<-xsmooth
  res$l=l
  res$b<-b
  return(res) 
}

h=1
alpha<-seq(0.05, 0.95, length=100)
forecast2<-lapply(alpha, DoubleExpSmooth,x=X1)
erreur2<-unlist(lapply(forecast2, function(x){mean((X1-x$smooth)^2)}))  

plot(alpha,erreur2,type='l',col='blue')
opt<-which.min(erreur2)

#calcul du alpha optimal :
points(alpha[opt],erreur2[opt],col='red', pch=20, cex=2)
text(alpha[opt],erreur2[opt]+4, labels=alpha[opt]%>%round(digits=3))
text(alpha[opt]+0.08,erreur2[opt]+4, labels=min(erreur2)%>%round(digits=3))
alphaX1_D=alpha[opt]


X1.smooth_D<-DoubleExpSmooth(X1,alphaX1_D)
plot(X1,type='l', main="comparaison lissage double et simple ? l'horizon h=1", xlab="Temps en mois")
lines(X1.smooth_D$smooth,col='red' )
lines(X1.smooth,col='green')



#lissage exponentiel de Holt Winters (saisonalit?)
#On va comparer les deux mod?les de saisonalit?

#mod?le multiplicatif:
#T=saisonalit?: 12 mois 
SeasonalMultiplicatifDoubleExpSmooth=function(x,alpha,beta,delta,T)
{
  xsmooth=x
  n<-length(x)
  l<-array(x[1],dim=length(x))
  b<-array(x[2]-x[1], dim=length(x)) #initialisation
  s<-array(x[1],dim=length(x))
  n1<-T+1
  for (i in c(n1:n))
  {
    l[i]<-alpha*(x[i]/s[i-T])+(1-alpha)*(l[i-1]+b[i-1]) #T est la période
    b[i]=beta*(l[i]-l[i-1])+(1-beta)*b[i-1]
    s[i]=delta*(xsmooth[i]/l[i])+(1-delta)*s[i-T]
    xsmooth[i]=(l[i]+b[i]*h)*s[i]
  }
  res<-list()
  res$smooth<-xsmooth
  res$l=l
  res$b<-b
  return(res) 
}

h<-1
T<-12
par(mfrow=c(1,2))
X1.SeasonalMultiplicatifDoubleExpSmooth=SeasonalMultiplicatifDoubleExpSmooth(X1, 0.2, 0.2, 0.2, 12)
plot(X1,type='l',ylim=range(X1,X1.SeasonalMultiplicatifDoubleExpSmooth$smooth),main = "param?tres alpha=beta=gamma=0.2", xlab = "Temps (en mois)", ylab = "Prix du baril")
lines(X1.SeasonalMultiplicatifDoubleExpSmooth$smooth, col='orange2')
#mod?le beaucoup trop sensibe aux variations

X2.SeasonalMultiplicatifDoubleExpSmooth=SeasonalMultiplicatifDoubleExpSmooth(X1, 0.9, 0.1, 0.1, 12)
plot(X1,type='l',ylim=range(X1,X2.SeasonalMultiplicatifDoubleExpSmooth$smooth),main = "param?tres alpha=0.9 beta=gamma=0.1", xlab = "Temps (en mois)", ylab = "Prix du baril")
lines(X2.SeasonalMultiplicatifDoubleExpSmooth$smooth, col='orange2')

#Lissage saisonalit? additive:
SeasonalAdditifDoubleExpSmooth=function(x,alpha,beta,delta,T)
{
  xsmooth=x
  n<-length(x)
  l<-array(x[1],dim=length(x))
  b<-array(x[2]-x[1], dim=length(x)) #initialisation
  s<-array(x[1],dim=length(x))
  n1<-T+1
  for (i in c(n1:n))
  {
    l[i]<-alpha*(x[i]-s[i-T])+(1-alpha)*(l[i-1]+b[i-1]) #T est la période
    b[i]=beta*(l[i]-l[i-1])+(1-beta)*b[i-1]
    s[i]=delta*(xsmooth[i]-l[i])+(1-delta)*s[i-T]
    xsmooth[i]=l[i]+b[i]*h+s[i]
  }
  res<-list()
  res$smooth<-xsmooth
  res$l=l
  res$b<-b
  return(res) 
}
par(mfrow=c(1,1))
h<-1
T<-12
X1.SeasonalAdditifDoubleExpSmooth=SeasonalAdditifDoubleExpSmooth(X1, 0.2, 0.2, 0.2, 12)
plot(X1,type='l',ylim=range(X1,X1.SeasonalAdditifDoubleExpSmooth$smooth),main = "param?tres alpha=beta=gamma=0.2", xlab = "Temps (en mois)", ylab = "Prix du baril")
lines(X1.SeasonalAdditifDoubleExpSmooth$smooth, col='orange2')


# Avec la fonction Holt-Winter: 
#test ? l'horizon 1 an 
oilTs <- oilPriceDate$price[324:1]
oilPriceDate.ts2 <- ts(oilTs, start = 1, frequency = 12) 
plot(oilPriceDate.ts2,type='l')
hw <-  HoltWinters(oilPriceDate.ts2, alpha = NULL, beta = NULL, gamma = NULL,
                   seasonal = c("additive"),
                   start.periods = 2, l.start = NULL, b.start = NULL,
                   s.start = NULL,
                   optim.start = c(alpha = 0.3, beta = 0.1, gamma = 0.1),
                   optim.control = list())
plot(fitted(hw))
plot(hw)
prediction<-predict(hw, n.ahead =12 , prediction.interval = TRUE, level = 0.95)
plot(hw, prediction, main = "pr?diction sur les donn?es futures horizon : 12 mois", xlab = "Temps (en ann?es)", ylab = "Prix du baril")

## second test Holt-Winter
#Id?e : montrer les limites sur un test de 24 mois 
oilTs2 <-oilPriceDate$price[324:108] #On consid?re les donn?es entre les 1er janviers 1990 et 2007 (avant le premier choc)
oilPriceDate.ts3 <- ts(oilTs2, start = 1, frequency = 12) 
plot(oilPriceDate.ts3, type='l')
hw2 <-  HoltWinters(oilPriceDate.ts3, alpha = NULL, beta = NULL, gamma = NULL,
                   seasonal = c("additive"),
                   start.periods = 2, l.start = NULL, b.start = NULL,
                   s.start = NULL,
                   optim.start = c(alpha = 0.3, beta = 0.1, gamma = 0.1),
                   optim.control = list())
plot(fitted(hw2))
plot(hw2)
prediction<-predict(hw2, n.ahead =24 , prediction.interval = TRUE, level = 0.95)
plot(hw2, prediction, ylim=range(oilPriceDate.ts3,prediction), main = "pr?diction sur les donn?es futures (janvier 2007) horizon:24 mois", xlab = "Temps (en ann?es)",ylab = "Prix du baril")
lines(oilPriceDate.ts2)
#Ici on superpose les pr?visions avec la v?ritable courbe des prix


