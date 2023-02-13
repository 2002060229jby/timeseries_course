rm(list = ls())
data <- read.table("./数据/m2.txt", header = TRUE,fileEncoding = 'utf-8')
m = data[,2]
M2 <- ts(m, start = c(1999, 12), frequency = 12)
plot(M2, type = "l")

library(urca)
urdf <- ur.df(m, type = "drift", lags = 13, selectlags = "BIC")
summary(urdf)
urdf1 <- ur.df(m, type = "drift", lags = 3, selectlags = "Fixed")
summary(urdf)
