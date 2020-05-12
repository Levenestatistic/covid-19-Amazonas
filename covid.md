# covid-19-Amazonas
rm(list=ls())

#pacotes
require(forecast)
require(tseries)
require(fracdiff)

setwd("E:\\Análises Estatísticas\\Covid")
amcovid = read.csv("am.txt", h=T, sep="\t", dec=",")
attach(amcovid)
head(amcovid)


plot(casosn,type='o',lwd=2,xlab="mês",ylab="Titulo do Gráfico")


#Gráfico de autorrelações
m1 = acf(casosn,lag.max=50, type="correlation", plot=F)
m1$lag= m1$lag*12
plot(m1, xlab="defasagens", ylab="autocorrelações",main="")

m2 = pacf(casosn,lag.max=50, type="partial", plot=F)
m2$lag= m2$lag*12
plot(m2, xlab="defasagens", ylab="autocorrelações",main="")

#Localizando o modelo ideial
adf.test(casosn,alternative="stationary")# teste adf p raizunitária
kpss.test(casosn,null="Level") 

#Resultado: Indica um modelo ARIMA ou SARIMA

#Estimando d
d.GPH<- fdGPH(casosn)$d
d.R0<- fdSperio(casosn)$d

#Resultado: o d está próximo de 1

#Modelo ARIMA
x1=arima(casosn,order=c(0,1,0))
x2=arima(casosn,order=c(1,1,0))
x3=arima(casosn,order=c(2,1,0))
x4=arima(casosn,order=c(0,1,1))
x5=arima(casosn,order=c(0,1,2))
x6=arima(casosn,order=c(1,1,2))
x7=arima(casosn,order=c(2,1,2))
x8=arima(casosn,order=c(2,1,1))
x9=arima(casosn,order=c(1,1,1))

AIC=AIC(x1,x2,x3,x4,x5,x7,x8,x9)

#Prevendo
prev <- forecast(x3,15)
plot(prev)
