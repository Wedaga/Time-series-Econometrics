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
dmanprod<- diff(manprod)
dimport<- diff(import)
dcpirate<- diff(cpirate)
datacombine<-ts.combine(dmanprod, dimport, dcpirate)
datacombine
lagselect<-VARselect(datacombine, lag.max = 12, type="const")
lagselect$selection
varmodel<-VAR(datacombine, lag.max=12, ic="AIC")
summary(varmodel)

#Structural VAR estimation
cpieq<-tsreg(dcpirate, ts.combine(lags(dcpirate,1), lags(dimport,1), lags(dmanprod,1)))
cpieq
importeq<-tsreg(dimport, ts.combine(lags(dcpirate,0:1), lags(dimport,1), lags(dmanprod,1)))
importeq
dmanprodeq<-tsreg(dmanprod, ts.combine(lags(dcpirate,0:1), lags(dimport,0:1), lags(dmanprod,1)))
dmanprodeq

#Getting the VAR residual matrix 
ResM<-matrix(c(cpieq$reiduals, importeq$residuals, dmanprodeq$residuals),ncol=3)
ResM
#Covariance matrix
cov<-cov(ResM)
cov

#Getting the A matrix
#I first get a matrix D as in class
D<-matrix(c(0,importeq$coefficients[2],dmanprodeq$coefficients[2], 0,0,dmanprodeq$coefficients[4], 
            0,0,0),ncol=3)
D
#Now I find Matrix A
A<- diag(3)-D
A
#inverse of A
library(tstools)
invA<- solve(A)
invA
#now the impact of shocks using the cholesky decomposition
shockeffect<-t(chol(invA%*%cov%*%t(invA)))
shockeffect



#checking if correct
data2<-ts.combine(dcpirate, dimport, dmanprod)
varmodel1<-VAR(data2, p=1)
summary(varmodel1)
resid<-residuals(varmodel1)
cova<-cov(resid)

pcol<-t(chol(cova))
pcol

[,1]      [,2]      [,3]
[1,] 0.27710609 0.0000000 0.0000000
[2,] 0.19242590 0.4966754 0.0000000
[3,] 0.08662691 0.4220570 0.6999114


resid<-(residuals(varmodel))
covr<-cov(resid)
shock=t(chol(covr))
shock
