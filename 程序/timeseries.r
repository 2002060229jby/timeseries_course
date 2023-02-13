rm(list=ls())
setwd('D:/All/11-浙江工商大学/大三（上）/时间序列')
getwd()
data=read.table('./data/gdpr.txt',header=TRUE,encoding="utf-8")
gdpr=data[,2]
GDPR<-ts(gdpr,start=1978,frequency=1)  
# ts函数将数据类型转换为时间序列形式，frequency=1表名数据的频率是年度数据
plot(GDPR)

acf(GDPR)
pacf(GDPR)
EQ_GDPR<-arima(GDPR,order=c(5,0,0))
EQ_GDPR

EQ_GDPR01<-arima(GDPR,order=c(5,0,2))
EQ_GDPR01
a<-EQ_GDPR01$coef
se<-sqrt(diag(EQ_GDPR01$var.coef))
b<-a/se
b

EQ_GDPR02<-arima(GDPR,order=c(5,0,2),include.mean = T, 
                 fixed=c(NA,NA,0,0,NA,NA,NA,NA),transform.pars = F)
EQ_GDPR02
a <-EQ_GDPR02$coef
a <-c(a[1:2],a[5:length(a)])
a
se<-sqrt(diag(EQ_GDPR02$var.coef))
se
b <-a/se
b

EQ_GDPR03<-arima(GDPR,order=c(5,0,2),include.mean = T, 
                fixed=c(NA,NA,0,0,NA,0,NA,NA),transform.pars = F)
EQ_GDPR03
a<-EQ_GDPR03$coef
a<-c(a[1:2],a[5],a[7:8])
a
se<-sqrt(diag(EQ_GDPR03$var.coef))
b<-a/se
b
library(FindAllRoots)
allroots(c(0.7432,0.2568,0),c(2,1,0))
# ?allroots

# 单位根检验
library(urca)
urdf<-ur.df(GDPR,type='drift',lags=9,selectlags='BIC') 
# 进行ACF单位根检验,type='drift'表明检验中不包含时间趋势项，lags=9滞后期为9期；selectlags='BIC'根据BIC信息判断确定恰当的滞后期
summary(urdf)

