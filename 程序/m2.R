rm(list = ls())
setwd("D:\\All\\浙江工商大学\\2-大三（上）\\时间序列")
data <- read.table("./数据/m2.txt", header = TRUE,fileEncoding = 'utf-8')
m = data[,2]
M2 <- ts(m, start = c(1999, 12), frequency = 12)
plot(M2, type = "l")

library(urca)
urdf <- ur.df(m, type = "drift", lags = 13, selectlags = "BIC")
summary(urdf)
urdf1 <- ur.df(m, type = "drift", lags = 3, selectlags = "Fixed")
summary(urdf1)

acf(m)
pacf(m)

mean<-arima(m, order=c(4,0,0),seasonal = list(order=c(0,0,1),period=12),
            fixed=c(NA,NA,0,NA,NA,NA),transform.pars = FALSE)
mean

tsdiag(mean)

library(FinTS)
mean_res<-resid(mean)
ArchTest(mean_res, lags=2)

library(rugarch)
garchspec=ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1),submodel=NULL,
                     external.regressors=NULL,variance.targeting=FALSE),
                     mean.model = list(armaOrder=c(4,12),include.mean=T,archm=F,archpow=1,arfima=F,external.regressors=NULL,archex=F),
                     distribution.model = "norm",fixed.pars = list(ar3=0,ma1=0,ma2=0,ma3=0,ma4=0,ma5=0,ma6=0,ma7=0,ma8=0,ma9=0,ma10=0,ma11=0))
garchfit = ugarchfit(garchspec,data=m,solver = "solnp")
garchfit


egarchspec=ugarchspec(
  variance.model = list(model="eGARCH",garchOrder=c(3,2),submodel=NULL,
  external.regressors=NULL,variance.targeting=FALSE),
  mean.model = list(armaOrder=c(4,12),include.mean=T,
  archm=F,archpow=1,arfima=F,external.regressors=NULL,archex=F),
  distribution.model = "norm",
  fixed.pars = list(ar3=0,ma1=0,ma2=0,ma3=0,ma4=0,ma5=0,ma6=0,ma7=0,ma8=0,ma9=0,ma10=0,ma11=0,
  alpha1=0,alpha2=0))

egarchfit = ugarchfit(egarchspec,data=m,solver = "lbfgs")
egarchfit






