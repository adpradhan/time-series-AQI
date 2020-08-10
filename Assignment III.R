library(readxl)
AQI_ <- read_excel("C:/Users/AD/Desktop/AQI Delhi Monthly Data - Copy/AQI .xlsx", 
                   +     sheet = "final1")
View(AQI_)
aqits <- ts(AQI_$AQI, frequency = 12, start = c(2003, 1))
plot.ts(aqits)

#1 decompose
aqits_components <- decompose(aqits)
plot(aqits_components)
aqitssesadj <- aqits - aqits_components$seasonal
plot(aqitssesadj)

#2.a checking for non-stationarity
library(aTSA)
plot.ts(aqitsdiff1)
adf.test(aqits)
pp.test(aqits)
kpss.test(aqits)
aqitsdiff1 <- diff(aqits, differences = 1)
adf.test(aqitsdiff1)
pp.test(aqitsdiff1)
kpss.test(aqitsdiff1)

#2.b candidate ARIMA model
acf(aqitsdiff1)
pacf(aqitsdiff1)
library("urca")
library("forecast")
auto.arima(aqits)
auto.arima(aqits, ic = "bic")

#2.c forecast
aqitsarima <- arima(aqits, order = c(0,0,1), seasonal = list(order = c(0,1,1), period = 12))
aqitsforecasts <- forecast(aqitsarima, h = 24)
plot(aqitsforecasts)

#2.d Is the given predictive model is good?
acf(aqitsforecasts$residuals, lag.max = 20)
Box.test(aqitsforecasts$residuals, lag=20, type="Ljung-Box")
plot.ts(aqitsforecasts$residuals)
hist(aqitsforecasts$residuals)




