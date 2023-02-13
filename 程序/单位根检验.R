
################3.1#################

rm(list=ls())
### 1.导入数据，做时序图
library(urca)
data <- read.table("./data/z1z2.txt",fileEncoding = 'gbk',header=T)
z1=data[,2]
Z1<-ts(z1)
plot(Z1)
### 2.单位根检验
# （1）首先采用只包含截距项的模型来对序列Z1进行初步的ADF单位根检验，根据BIC信息判断准则来确定检验模型的恰当的滞后期
urdflc<-ur.df(z1,type='trend',lags=12,selectlags ='BIC')
summary(urdflc)
# 检验结果表明，在最长为12期的滞后期中，根据BIC信息判断准则选择出的恰当滞后期为滞后1期。
urdf1c_1<-ur.df(z1,type='drift',lags=1,selectlags = 'Fixed')
summary(urdf1c_1)
# 检验结果表明，DF统计量为-1.5761，大于5%临界值-2.89，因此不能拒绝序列存在单位根的原假设。进一步考虑用phi1统计量来检验原假设gamma=alpha0=0,检验结果中phi1统计量为1.375，小于5%临界值4.71，因此不能拒绝原假设，从而应继续尝试不包含截距项的检验模型来进行ADF单位根检验。
# （2）采用无截距和趋势项的模型来对序列Z1进行ADF单位根检验，根据BIC信息判断准则确定检验模型的恰当的滞后期
urdf1n<-ur.df(z1,type='none',lags=12,selectlags ='BIC')
summary(urdf1n)
# 检验结果表明，在最长为12期的滞后期中，根据BIC信息判断准则选择出的恰当滞后期为滞后1期。
urdf1n_1<-ur.df(z1,type='none',lags=1,selectlags = 'Fixed')
summary(urdf1n_1)
# DF统计量为0.427，大于5%临界值-1.95，因此不能拒绝序列存在单位根的原假设。检验结论为，序列Z1存在单位根

# （3）进一步对序列Z1的一节差分序列进行单位根检验。
urdf1d<-ur.df(diff(z1),type='none',lags=0,selectlags = 'Fixed')
summary(urdf1d)
# DF统计量为-14.8789，小于1%临界值-2.6，因此能够拒绝序列存在单位根的假设，检验结论为，序列Z1的一阶差分序列平稳，原序列Z1为一阶单整序列。

################3.2#################
rm(list=ls())
### 1.导入数据，做时序图
library(urca)
data <- read.table("./data/z1z2.txt",fileEncoding = 'gbk',header=T)
z2=data[,3]
Z2<-ts(z2)
plot(Z2)
### 2.单位根检验
# （1）首先采用只包含截距项的模型来对序列Z2进行初步的ADF单位根检验，根据BIC信息判断准则来确定检验模型的恰当的滞后期
urdf2c<-ur.df(Z2,type='trend',lags=12,selectlags ='BIC')
summary(urdf2c)
# 检验结果表明，在最长为12期的滞后期中，根据BIC信息判断准则选择出的恰当滞后期为滞后1期。
urdf2c_1<-ur.df(Z2,type='trend',lags=1,selectlags = 'Fixed')
summary(urdf2c_1)
# 检验结果表明，DF统计量为-1.3524，大于5%临界值-3.45，因此不能拒绝序列存在单位根的原假设。进一步考虑用phi3统计量来检验原假设gamma=alpha2=0,检验结果中phi1统计量为1.5591，小于5%临界值6.49，因此不能拒绝原假设，从而应继续尝试只包含截距项的检验模型来进行ADF单位根检验。
# （2）采用无截距和趋势项的模型来对序列Z2进行ADF单位根检验，根据BIC信息判断准则确定检验模型的恰当的滞后期
urdf2c<-ur.df(Z2,type='none',lags=12,selectlags ='BIC')
summary(urdf2c)
# 检验结果表明，在最长为12期的滞后期中，根据BIC信息判断准则选择出的恰当滞后期为滞后1期。
urdf1n_1<-ur.df(z1,type='none',lags=1,selectlags = 'Fixed')
summary(urdf1n_1)
# DF统计量为0.427，大于5%临界值-1.95，因此不能拒绝序列存在单位根的原假设。检验结论为，序列Z1存在单位根

# （3）进一步对序列Z1的一节差分序列进行单位根检验。
urdf1d<-ur.df(diff(z1),type='none',lags=0,selectlags = 'Fixed')
summary(urdf1d)
# DF统计量为-14.8789，小于1%临界值-2.6，因此能够拒绝序列存在单位根的假设，检验结论为，序列Z1的一阶差分序列