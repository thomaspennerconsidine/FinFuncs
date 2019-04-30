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

open3d()
x <- X1
y <- X2
z <- Y
fit <- lm(z ~ x + y)
plot3d(x, y, z,  col = "red")

coefs <- coef(fit)
a <- coefs["x"]
b <- coefs["y"]
c <- -1
d <- coefs["(Intercept)"]
planes3d(a, b, c, d, alpha = 0.5)
