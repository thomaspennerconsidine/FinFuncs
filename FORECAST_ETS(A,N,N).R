library(htmlTable)
library(IBrokers)
library(Quandl)
library(quantmod)
library(RcppRoll)
library(rvest)
library(TTR)
library(xml2)
library(xts)
library(zoo)
library(forecast)

Y <- rnorm(1000)


forecast::auto.arima(Y)
plot(forecast(Y))
plot(forecast(Y, h = ifelse(frequency(Y) > 1, 2 * frequency(SIN2), 10) ,
              level=c(80,95), fan=FALSE, robust=FALSE, lambda=NULL, find.frequency=FALSE,
              allow.multiplicative.trend=FALSE, model=NULL))