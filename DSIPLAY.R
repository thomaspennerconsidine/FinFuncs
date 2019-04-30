

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

X1 <- rnorm(1000)
X2 <- rnorm(1000)
X3 <- rnorm(1000)
X4 <- rnorm(1000)
Y <- rnorm(1000)

MODEL1 <- data.frame(Y,X1,X2,X3,X4)



tsdisplay(diff(diff(diff(Y,8))), main = "forth 0rder")
tsdisplay(diff(diff(Y,4)), main = "third order")
tsdisplay(diff(Y,2), main = "DELTA-second order")
tsdisplay(Y, main = "BASE")



ccf(X1, Y, lag.max = NULL, type = c("correlation", "covariance"),
    plot = TRUE, na.action = na.fail)

forecast::auto.arima(Y)
plot(forecast(Y))
plot(forecast(Y, h = ifelse(frequency(Y) > 1, 2 * frequency(SIN2), 10) ,
              level=c(80,95), fan=FALSE, robust=FALSE, lambda=NULL, find.frequency=FALSE,
              allow.multiplicative.trend=FALSE, model=NULL))
