setwd('./')
# getwd()
data=read.table('./data/gdpr.txt',header=TRUE,encoding="utf-8")
gdpr=data[,2]
GDPR<-ts(gdpr,start=1978,frequency=1)  
# ts函数将数据类型转换为时间序列形式，frequency=1表名数据的频率是年度数据
plot(GDPR)


acf(GDPR)
pacf(GDPR)
EQ_GDPR<-arima(GDPR,order=c(5,0,0))
EQ_GDPR<-arima(GDPR,order=c(5,0,0),include.mean=T,fixed=c(NA,NA,0,0,NA,NA),transform.pars=F)
EQ_GDPR<-arima(GDPR,order=c(5,0,2))
EQ_GDPR
EQ_GDPR<-arima(GDPR,order=c(5,0,2),include.mean=T,fixed=c(NA,NA,0,0,NA,NA),transform.pars=F)
EQ_GDPR



# 单位根检验
library(urca)
urdf<-ur.df(GDPR,type='drift',lags=9,selectlags='BIC') 
# 进行ACF单位根检验,type='drift'表明检验中不包含时间趋势项，lags=9滞后期为9期；selectlags='BIC'根据BIC信息判断确定恰当的滞后期
summary(urdf)

##################随机数###################3
e <- rnorm(200)
rt(20,2)
plot(e, type = 'l')


#########################差分#####################
x=1
for(i in 1:9){
    x[i+1]=x[i]+1
}
x
d=1
diff(x,differences = d)
d=2
diff(x,differences = d)
L=4
diff(x,lag=L,differences=d)
diff(x,lag=L)
diff(diff(x,lag=4))
diff(diff(x),lag=4)

# data <- read.table('./data/gdpreal.txt',fileEncoding = 'gbk', header=T)
# colnames(data)=c('time','gdp')
# View(data)
# attach(data)
# acf(gdp)

y=11:20
lag(y)
diff(y)
n=length(y)
d=2
yt_d=y[1:(n-d)]
yt = y[(1+d):10]
cbind(yt,yt_d)
embed(y,4)
y
