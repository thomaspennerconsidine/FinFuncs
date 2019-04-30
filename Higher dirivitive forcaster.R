

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

MODEL <- data.frame(Y,X1,X2,X3,X4)
MODEL1 <- lm(Y ~ X1 + X2 + X3 + X4 )
RESIDUALS <- MODEL1$residuals



tsdisplay(diff(diff(diff(Y,8))), main = "forth 0rder")
tsdisplay(diff(diff(Y,4)), main = "third order")
tsdisplay(diff(Y,2), main = "DELTA-second order")
tsdisplay(RESIDUALS, main = "BASE")

plot(density(RESIDUALS) , main = "Residual Density" )



