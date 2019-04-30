
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




y <- ts(CONV$Y)
x1 <- ts(CONV$X1)
x2 <- ts(CONV$X2)
y_hat <- lag(predict.lm(MODEL),1)
IC <- cor(y,y_hat, method = "pearson")



#lmtest::bptest(MODEL)
#forecast::Pacf(x1)
#forecast::Pacf(x2)
#forecast::Pacf(y)
#plot(RollingCorr(y,y_hat,2))
#
Y_rate <- ROC(Y,n = 1, type = "continuous")
X1_rate <- ROC(X1,n = 1, type = "continuous")
X2_rate <- ROC(X2,n = 1, type = "continuous")
MODEL_rate <- lm(Y_rate ~ X2_rate + X1_rate)

forecast::checkresiduals(MODEL_rate)
forecast::checkresiduals(MODEL)

#___________________________________________________________
summary(MODEL)
print(IC)
vif(MODEL)
forecast::accuracy(MODEL)
dwtest(MODEL) # probablity that true autocorrelation is greater than 0
dwtest(MODEL_rate)
