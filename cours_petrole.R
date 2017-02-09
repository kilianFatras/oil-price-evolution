rm(list=objects())
library(zoo)
library(timeDate)
library(forecast)
library(xts)
library(dygraphs)

######Importation des données
setwd(dir = "/Users/Kilian/Programmation/ENSTA_2A/R_project/MAP_STA2/Projet/Valeurs")
coursPetrole<-read.table("Valeurs.csv", sep = ';', dec = ",", skip = 1)
head(coursPetrole)
plot(coursPetrole)

######Création Data Frame
dateFin <- strptime(c("01/01/1990"), "%m/%d/%Y")
dateIni <- strptime(c("12/01/2016"), "%m/%d/%Y")
Date <- seq(dateIni, dateFin, by= "-1 month")
cours<-data.frame(Date,coursPetrole$V3)
names(cours)<-c("Date","couts")
plot(cours$Date,cours$couts,type='l')

######Stats de bases
mean(cours$couts)
sd(cours$couts)
summary(cours$couts)
