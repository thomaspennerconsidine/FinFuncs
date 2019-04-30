{
  library(corrplot)
  library(ggplot2)
  library(quantmod)
  library(xts)
  library(rms)
  #X library(glmulti)
  library(leaps)
  library(stats)
  library(stats4)
  library(MASS)
  library(scatterplot3d)
  library(rgl)
  library(RollingWindow)
  library(lmtest)
  library(randtests)
  library(tseries)
  library(egcm)
  library(urca)
} # package liberaries




X1 <- rnorm(1000)
X2 <- rnorm(1000)
X3 <- rnorm(1000)
X4 <- rnorm(1000)
Y <- rnorm(1000)



CONV <- data.frame(Y,X1,X2)
Y_MASTER <- CONV$Y
X_MASTER <- CONV$X1 + CONV$X2

Y <- (ts(CONV$Y))
X1 <- (ts(CONV$X1))
X2 <- (ts(CONV$X2))
MODEL <- lm(Y ~ X2 + X1)
forecast::checkresiduals(MODEL)

Y_rate <- ROC(Y,n = 1, type = "continuous")
X1_rate <- ROC(X1,n = 1, type = "continuous")
X2_rate <- ROC(X2,n = 1, type = "continuous")
MODEL_rate <- lm(Y_rate ~ X2_rate + X1_rate)
forecast::checkresiduals(MODEL_rate)


y_hat <- lag(predict.lm(MODEL),1)
IC <- cor(Y,y_hat, method = "pearson")
accuracy(MODEL)


XX@cval
XX@teststat

#Engle–Granger cointegration test, from package "egcm"


