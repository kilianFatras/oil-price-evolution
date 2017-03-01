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

#mobile mean and smooth

mb<-filter(oilPriceDate$price, filter = array(1/50, dim = 50), method = c("convolution"),
           sides = 2, circular = FALSE)
mb<-xts(mb, order.by = oilPriceDate$Date)
plot(oilPriceDate$Date, oilPriceDate$price, type = 'l')
lines(mb, col = 'red')

######Smooth data with mobile mean and avoid choc

for (i in 118:92) {
  oilPriceDate$price[i] = mb[324 + 1 - i]
}

plot(oilPriceDate$Date, oilPriceDate$price, type = 'l', main = "évolution du prix du baril de pétrole lissé linéairement", xlab = "Date", ylab = "Prix du baril")


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



#Differenciation by autocorrelation
par(mfrow = c(1, 2))
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

plot(oilPriceDate$Date, seasonPart, type = 'l',main = "Saisonnalité des données lissées linéairement", xlab = "Date", ylab = "Prix du baril")
lines(oilPriceDate$Date, MA, col = 'blue')
lines(oilPriceDate$Date, MA2, col = 'orange')
lines(oilPriceDate$Date, MA3, col = 'red')




##Lissage exponentiel:
graphics.off()
#lissage simple:
expSmooth=function(x,alpha)
{
  xsmooth<-x
  n<-length(x)
  for (i in c(2:n))
  {
    xsmooth[i]<-(1-alpha)*xsmooth[i-1]+alpha*x[i]# xsmooth[i] correspond à y^t+1  /t
  }
  xsmooth<-c(x[1],head(xsmooth,n-1))#décalage de 1 de la série, parce que xsmooth[i] correspond à y^t+1  /t
  return (xsmooth)
}

alpha<-0.9
X1<-oilPriceDate$price
X1<-X1[324:1] #pour remettre les valeurs dans le bon sens (critère estétique plus qu'autre chose)
X1.smooth<-expSmooth(X1,alpha)

plot(X1,type='l')
lines(X1.smooth, col='red')
alpha2<-0.1
X1.smooth2<-expSmooth(X1,alpha2)
lines(X1.smooth2, col='blue')

#Trouver le alpha optimal
mean((X1-X1.smooth)^2)

alpha<-seq(0.00,0.95,length=100)
forecast<-lapply(alpha, expSmooth, x=X1)#calcule la moyenne etc d'une matrice, revoie une liste mais on veut un vecteur
erreur<-unlist(lapply(forecast,function(x){mean((X1-x)^2)}))
plot(alpha, erreur, type='l')
alpha_opt<-which.min(erreur)#trouve le minimum de la fonction
points(alpha[alpha_opt],erreur[alpha_opt],col='red', pch=20,cex=2)#que fait cex

text(alpha[alpha_opt],erreur[alpha_opt]+0.5, labels=alpha[alpha_opt]%>%round(digits=3))
alphaX1=alpha[alpha_opt]
#on trouve alphaX2 (donc l'optimal)=0.95 je ne sais pas si c'est vraiment bien: cela signifierait qu'on ne peut pas prendre en compte la stabilitité mais plutot les variations


#lissage double

#h est l'horizon
h<-1
DoubleExpSmooth=function(x,alpha)
{
  xsmooth=x
  n<-length(x)
  l<-array(x[1],dim=length(x))
  b<-array(x[2]-x[1], dim=length(x)) #initialisation
  
  for (i in c(2:n))
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


alpha<-seq(0.05, 0.95, length=100)
forecast1<-lapply(alpha,expSmooth,x=X1)
forecast2<-lapply(alpha, DoubleExpSmooth,x=X1)
erreur1<-unlist(lapply(forecast1, function(x){mean((X1-x)^2)}))
erreur2<-unlist(lapply(forecast2, function(x){mean((X1-x$smooth)^2)}))  # pourquoi $smooth??


plot(alpha,erreur2,type='l',ylim=range(erreur2,erreur1),col='blue')

lines(alpha,erreur1, type='l')
opt<-which.min(erreur2)


points(alpha[opt],erreur2[opt],col='red', pch=20, cex=2)
text(alpha[opt],erreur2[opt]+4, labels=alpha[opt]%>%round(digits=3))
text(alpha[opt]+0.05,erreur2[opt]+4, labels=min(erreur2)%>%round(digits=3))
alphaX1_D=alpha[opt]


X1.smooth<-expSmooth(X1,alphaX1)
X1.smooth_D<-DoubleExpSmooth(X1,alphaX1_D)
plot(X1,type='l')
lines(X1.smooth_D$smooth,col='red')
lines (X1.smooth,col='blue')

plot(X1.smooth_D$l,type='l',ylim=range(X1.smooth_D$l,X1.smooth_D$b), col='blue')
lines(X1.smooth_D$b,type='l', col='green')
#Ici, on a les tracés des valeurs de b[i] et l[i], J'avoue que je ne comprends pas bien l'interet de cela, je le laisse mais sauf si tu comprends le mieux serait de l'enlever pour rendre notre travail
##Cela doit servir à determiner la période de convergence: mais je ne comprends pas bien comment; surtout qu'elle semble quasi inexistante au vue du graphe et même des b[i]
#En réalité on va voir juste après que lorsque j'enlève une période de cv, l'erreur quadratique augmente! Ce qui est toujours un tantinet étrange, il est normal qu'elle change de valeur mais pas forcement pour une augmentation..
#en essayant avec 0, je pense qu'il ne s'agit que d'un hasard, ou alors c'est que le début est particulièrement bien suivit car plus  stable (ce qui me semble le plus logique!)
erreur1<-unlist(lapply(forecast1, function(x){mean((X1[0:324]-x[0:324])^2)}))
erreur2<-unlist(lapply(forecast2, function(x){mean((X1[0:324]-x$smooth[0:324])^2)}))  # pourquoi $smooth??


plot(alpha,erreur2,type='l',ylim=range(erreur1,erreur2),col='blue')
lines(alpha,erreur1, type='l')
opt<-which.min(erreur2)


points(alpha[opt],erreur2[opt],col='red', pch=20, cex=2)
text(alpha[opt],erreur2[opt]+4, labels=alpha[opt]%>%round(digits=3))
text(alpha[opt]+0.05,erreur2[opt]+4, labels=min(erreur2)%>%round(digits=3))
alphaX1_D=alpha[opt]

#lissage exponentiel de Holt Winters (saisonalité)
#La chute brutale après la bulle de 2008 peut faire penser à un modèle multiplicatif
## + vision globale du graphe: volatilité semble proportionelle au prix
## On essayera tout de même les deux modèles!

#modèle multiplicatif:
#T=saisonalité: 12 mois (la plus petite non?)
SeasonalMultiplicatifDoubleExpSmooth=function(x,alpha,beta,delta,T)
{
  xsmooth=x
  n<-length(x)
  l<-array(x[1],dim=length(x))
  b<-array(x[2]-x[1], dim=length(x)) #initialisation
  s<-array(x[1],dim=length(x))
  
  for (i in c(13:n))
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
X1.SeasonalMultiplicatifDoubleExpSmooth=SeasonalMultiplicatifDoubleExpSmooth(X1, 0.2, 0.2, 0.2, 72)
plot(X1,type='l',ylim=range(X1,X1.SeasonalMultiplicatifDoubleExpSmooth$smooth))
lines(X1.SeasonalMultiplicatifDoubleExpSmooth$smooth, col='red')
#Terrrrible!

X2.SeasonalMultiplicatifDoubleExpSmooth=SeasonalMultiplicatifDoubleExpSmooth(X1, 0.2, 0.2, 0.2, 12)
plot(X1,type='l',ylim=range(X1,X2.SeasonalMultiplicatifDoubleExpSmooth$smooth))
lines(X2.SeasonalMultiplicatifDoubleExpSmooth$smooth, col='red')
#Et visiblement pas mieux en prenant la période plus importante!

X2.SeasonalMultiplicatifDoubleExpSmooth=SeasonalMultiplicatifDoubleExpSmooth(X1, 0.9, 0.1, 0.1, 12)
plot(X1,type='l',ylim=range(X1,X2.SeasonalMultiplicatifDoubleExpSmooth$smooth))
lines(X2.SeasonalMultiplicatifDoubleExpSmooth$smooth, col='red')
#ok on peut jouer avec les paramètres..J' ai un peu la flemme de le faire maintenant mais on regardera cela de plus près: ici le lissage est tout de même fort proche de l'allure de la courbe!

# Bon tentons le lissage saisonalité additive:
SeasonalAdditifDoubleExpSmooth=function(x,alpha,beta,delta,T)
{
  xsmooth=x
  n<-length(x)
  l<-array(x[1],dim=length(x))
  b<-array(x[2]-x[1], dim=length(x)) #initialisation
  s<-array(x[1],dim=length(x))
  
  for (i in c(13:n))
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

h<-1
T<-12
X1.SeasonalAdditifDoubleExpSmooth=SeasonalAdditifDoubleExpSmooth(X1, 0.2, 0.2, 0.2, 12)
plot(X1,type='l',ylim=range(X1,X1.SeasonalAdditifDoubleExpSmooth$smooth))
lines(X1.SeasonalAdditifDoubleExpSmooth$smooth, col='red')
#Visiblement légèrement mieux.. 


####prevision on the sample

oilPriceDate.ts.HoltWintersFunction = HoltWinters(oilPriceDate.ts, alpha = NULL, beta = NULL, gamma = NULL,
                                                  seasonal = c("additive"),
                                                  start.periods = 2, l.start = NULL, b.start = NULL,
                                                  s.start = NULL,
                                                  optim.start = c(alpha = 0.3, beta = 0.1, gamma = 0.1),
                                                  optim.control = list())
plot(fitted(oilPriceDate.ts.HoltWintersFunction))
plot(oilPriceDate.ts.HoltWintersFunction)

p <- predict(oilPriceDate.ts.HoltWintersFunction, n.ahead=35)
lines(p, col="blue")
