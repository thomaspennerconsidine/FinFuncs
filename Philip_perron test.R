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



CONV <- data.frame(Y,X1,X2)
Y_rate <- (Y)
X1_rate <- (X1)
X2_rate <- (X2)
MODEL_rate <- lm(Y_rate ~ X2_rate + X1_rate)
Y <- (ts(CONV$Y))
X1 <- (ts(CONV$X1))
X2 <- (ts(CONV$X2))
MODEL <- lm(Y ~ X2 + X1)

INPUT <- (MODEL_rate$residuals)
XX1 <- urca::ur.pp(INPUT, type = "Z-tau", model = "constant")

CRIT1 <-XX1@teststat
summary(XX1)

XX1@cval
CRIT1


# NULL = series is non stationary
#reject null if crit is bigger than listed values at condiance interval