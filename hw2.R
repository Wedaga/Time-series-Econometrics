library(tidyverse)
library(zoo)
library(readr)
install.packages('tstools')
install.packages("rio")
library(openxlsx)
library(dplyr)
library(rio)
install.packages('extrafont')
install.packages("ggplot2")
library(extrafont)
library("ggplot2")
library(dplyr)
font_import()
install.packages('forecast', dependencies = TRUE)
install.packages("tidyverse")
library(rugarch)
library(fGarch)
library(forecast)
library(tidyverse)
library(tstools)
library(timeSeries)
library(broom)
library(extrafont)
loadfonts()
font_install("fontcm")
library(fpp2)
library(stargazer)
#Local Projection
install.packages("lpirfs")
library(lpirfs)
#install.packages("factoextra")
library(factoextra)  
install.packages("urca")
install.packages("vars")
install.packages("ggplot2")
install.packages('forecast', dependencies = TRUE)
install.packages("tidyverse")
install.packages("mFilter")
library(tseries)
library(quantmod)

library("ggplot2")
library("urca")
library("vars")
library('forecast')
library("tidyverse")
library("mFilter")
library(vars)


#getting my data togerther
cpi <- read_csv("cpi.csv")
View(cpi)
monthly <- ts(cpi, start = c(1960, 1), frequency = 12)
monthly
quarterly <- aggregate(monthly, nfrequency=4,mean)
quarterly
write.csv(quarterly,file ="cpi2.csv",col.names= True, fileEncoding = "UTF-8")


data2 <- read_csv("hw2 data.csv")
data2
import<- ts(data2[,4], start = c(1995, 1), end=c(2019,4), frequency = 4)
import
data3 <- read_csv("manufacturing2.csv")
data3
mlfp<-ts(data3[,2], start = c(1987, 1), frequency = 4)
mlfp
manprod<-window(mlfp, start = c(1995, 1), end=c(2019,4))
manprod
cpi <- read_csv("cpi3.csv")
cpi
cpi3<-ts(cpi, start=c(1960,1), frequency = 4)
cpi3
cpirate<-window(cpi3[,2], start = c(1995, 1), end=c(2019,4))
cpirate
# variables are: manprod cpirate import
#q1
#ploting series

#q1
#ploting series
#q1
#ploting series
plot(manprod, col="red",
     main="US Quarterly Manufacturing Sector Labor Productivity (Base year=2012)",
     sub="Fig 1",
     xlab="Quarterly",
     ylab="Labor Productivity",
     col.axis="black",
     family="Times New Roman",
     cex.main=0.90,
     cex.axis=0.7,
     cex.lab=0.7)

plot(import, col="red",
     main="US Quarterly Imports as a Percentage of GDP",
     sub="Fig 2",
     xlab="Quarterly",
     ylab="Import of Goods and Services",
     col.axis="black",
     family="Times New Roman",
     cex.main=0.90,
     cex.axis=0.7,
     cex.lab=0.7)


plot(cpirate, col="red",
     main="US Quarterly Consumer Price Index (Base year=2015)",
     sub="Fig 3",
     xlab="Quarterly",
     ylab="Consumer Price Index",
     col.axis="black",
     family="Times New Roman",
     cex.main=0.90,
     cex.axis=0.7,
     cex.lab=0.7)

#Q2:ADF tests
#manprod
summarize(manprod)
manprod.df <- ur.df(y=manprod, type="none", 
                    selectlags="AIC")
summary(manprod.df)
manprod.df <- ur.df(y=manprod, type="drift", 
                    selectlags="AIC")
summary(manprod.df)

manprod.df <- ur.df(y=manprod, type="trend", 
                    selectlags="AIC")
summary(manprod.df)


dmanprod<- diff(manprod)
dmanprod.df <- ur.df(y=dmanprod, type="none", 
                     selectlags="AIC")
summary(dmanprod.df)

dmanprod.df <- ur.df(y=dmanprod, type="drift", 
                     selectlags="AIC")
summary(dmanprod.df)
dmanprod.df <- ur.df(y=dmanprod, type="trend", 
                     selectlags="AIC")
summary(dmanprod.df)


import.df <- ur.df(y=import, type="none", 
                   selectlags="AIC")
summary(import.df)

import.df <- ur.df(y=import, type="drift", 
                   selectlags="AIC")
summary(import.df)

import.df <- ur.df(y=import, type="trend", 
                   selectlags="AIC")
summary(import.df)

dimport<-diff(import)

dimport.df <- ur.df(y=dimport, type="none", 
                    selectlags="AIC")
summary(dimport.df)
dimport.df <- ur.df(y=dimport, type="drift", 
                    selectlags="AIC")
summary(dimport.df)
dimport.df <- ur.df(y=dimport, type="trend", 
                    selectlags="AIC")
summary(dimport.df)

cpirate.df<-ur.df(y=cpirate, type="none", 
                  selectlags="AIC")
summary(cpirate.df)

cpirate.df<-ur.df(y=cpirate, type="drift", 
                  selectlags="AIC")
summary(cpirate.df)
cpirate.df<-ur.df(y=cpirate, type="trend", 
                  selectlags="AIC")
summary(cpirate.df)
dcpirate<-diff(cpirate)

dcpirate.df<-ur.df(y=dcpirate, type="none", 
                   selectlags="AIC")
summary(dcpirate.df)

dcpirate.df<-ur.df(y=dcpirate, type="drift", 
                   selectlags="AIC")
summary(dcpirate.df)
dcpirate.df<-ur.df(y=dcpirate, type="trend", 
                   selectlags="AIC")
summary(dcpirate.df)

#plot of differenced series.

plot(dmanprod, col="red",
     main="Diffrenced Manufacturing Sector Labor Productivity (Base year=2012)",
     sub="Fig 4",
     xlab="Quarterly",
     ylab="Manufacturing Labor Productivity",
     col.axis="black",
     family="Times New Roman",
     cex.main=0.90,
     cex.axis=0.7,
     cex.lab=0.7)

plot(dimport, col="red",
     main="Differenced Imports as a Percentage of GDP",
     sub="Fig 5",
     xlab="Quarterly",
     ylab="Import of Goods and Services",
     col.axis="black",
     family="Times New Roman",
     cex.main=0.90,
     cex.axis=0.7,
     cex.lab=0.7)

plot(dcpirate, col="red",
     main="Differenced Consumer Price Index (Base year=2015)",
     sub="Fig 6",
     xlab="Quarterly",
     ylab="Consumer Price Index",
     col.axis="black",
     family="Times New Roman",
     cex.main=0.90,
     cex.axis=0.7,
     cex.lab=0.7)

datacombine<-ts.combine(dmanprod, dimport, dcpirate)
datacombine

lagselect<-VARselect(datacombine, lag.max = 12, type="const")
lagselect$selection

varmodel<-VAR(datacombine, lag.max=12, ic="AIC")
summary(varmodel)
dmanprodplot<-(getVarForecasts(varmodel, "dmanprod", n=1:12,
                               start=c(2020,1)))
dimportplot<-(getVarForecasts(varmodel, "dimport", n=1:12,
                              start=c(2020,1)))
dcpirateplot<-(getVarForecasts(varmodel, "dcpirate", n=1:12,
                               start=c(2020,1)))   

plot(dmanprodplot, col="red",
     main="Manufacturing Labor Productivity \n (Twelve Quarters Forecast(2020:1 -2022:4))",
     sub="Fig 7",
     xlab="Quarterly",
     ylab="Manufacturing Labor Productivity",
     col.axis="black",
     family="Times New Roman",
     cex.main=0.90,
     cex.axis=0.7,
     cex.lab=0.7)

plot(dimportplot, col="red",
     main="Import of Goods and Services \n (Twelve Quarters Forecast(2020:1 -2022:4))",
     sub="Fig 8",
     xlab="Quarterly",
     ylab="Import of Goods and Services",
     col.axis="black",
     family="Times New Roman",
     cex.main=0.90,
     cex.axis=0.7,
     cex.lab=0.7)
plot(dcpirateplot, col="red",
     main="Consumer Price Index \n (Twelve Quarters Forecast(2020:1 -2022:4))",
     sub="Fig 9",
     xlab="Quarterly",
     ylab="Consumer Price Index",
     col.axis="black",
     family="Times New Roman",
     cex.main=0.90,
     cex.axis=0.7,
     cex.lab=0.7)
#in sample granger causality
grangercause<- causality(varmodel, cause="dcpirate")
grangercause$Granger


#bivariate
datacombine[,3]
grangertest(datacombine[,3]~datacombine[,2], order=2)
grangertest(datacombine[,3]~datacombine[,1], order=2)

#out-of-sample Granger causality
newcombine<-ts.combine(dcpirate,dmanprod)
##VAR model
newmodel1<-function(t){
  newdata1<-window(newcombine,end=t)
  fitvar<-VAR(newdata1, p=2)
  return(getVarForecast(fitvar, "dmanprod",1))
}
newmodel1(c(2000,3))

##set dates
dates<-make.dates(c(2009,4), c(2019,4),4)
dates

##AR Model
newmodel2<-function(t){
  newdata2<-window(newcombine,end=t)
  fitar<-arima(newdata2[,"dmanprod"],
               order=c(2,0,0))
  pred<-predict(fitar,1)
  return(pred$pred)
}
newmodel2(c(2000,3))


##forecasting
make.fcst.new<-function(f,endDates,firstForecast){
  return(ts(unlist(lapply(endDates,f)),
            start=firstForecast,
            frequency=frequency(newcombine)))
}

f.var<-make.fcst.new(newmodel1, dates, c(2010,1))
f.var
f.ar<-make.fcst.new(newmodel2, dates, c(2010,1))
f.ar
actual<-window(dmanprod, start=c(2010,1),
               end=c(2019,4))
##Forecast errors

E.var<- actual-f.var
E.ar<- actual-f.ar
num<-sum(E.ar^2 - E.ar*E.var)
num
den<- sum(E.var^2)
Enc.New<-50*num/den
Enc.New

#Cointegration test

leveldata<- ts.combine(manprod, cpirate, import)
leveldata
levelmodel<- lm(manprod~cpirate + import, data=leveldata)
levelmodel
resid<-levelmodel$residuals
resid
##ADF test
adf<-ur.df(y=resid, type="none", selectlags="AIC")
summary(adf)

#plot of deviations of LHS-variable

deviat<- manprod-levelmodel$fitted.values
deviat
plot(deviat)

plot(deviat, col="red",
     main="Deviations of Manufacturing Sector Labor Productivity",
     sub="Fig 10",
     xlab="Quarterly",
     ylab="Manufacturing Labor Productivity",
     col.axis="black",
     family="Times New Roman",
     cex.main=0.90,
     cex.axis=0.7,
     cex.lab=0.7)
