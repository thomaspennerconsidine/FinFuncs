
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



Y <- (ts(CONV$Y))
X1 <- (ts(CONV$X1))
X2 <- (ts(CONV$X2))
MODEL <- lm(Y ~ X2 + X1)
bgtest(MODEL, order = 100)