
library(stats)
library(lmtest)
library(quantmod)
library(randtests)
library(tseries)



X1 <- rnorm(1000)
X2 <- rnorm(1000)
X3 <- rnorm(1000)
X4 <- rnorm(1000)
Y <- rnorm(1000)

VARIABLES <- data.frame(Y,X1,X2,X3,X4)


lm.full <- lm(formula =  Y ~ X1 + X2, data =VARIABLES)




CONV <- data.frame(Y,X1,X2)
Y_MASTER <- CONV$Y
X_MASTER <- CONV$X1 + CONV$X2

plot(X_MASTER, Y_MASTER)