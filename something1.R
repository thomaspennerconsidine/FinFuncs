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


CUTOFF <- .2
#INPUT <- ts(rnorm(1000))+(c(1:1000)/10)
INPUT <- ts(MODEL1$residuals)
WALK <- ROC(INPUT, n = 1, type = "discrete", na.pad = FALSE)
WALK1 <-ifelse(WALK<CUTOFF,WALK,0)
RAND_WALK <-ifelse(WALK1>-CUTOFF,WALK1,0)

acf(RAND_WALK, lag.max = 10000)


