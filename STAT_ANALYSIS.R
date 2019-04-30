library(corrplot)
library(ggplot2)
library(quantmod)
library(xts)
library(rms)
#library(glmulti)
library(leaps)
library(stats)
library(stats4)
library(MASS)
library(scatterplot3d)
library(rgl)
library(randtests)
library(lmtest)
Y  <- rnorm(100)
X1 <- rnorm(100)
X2 <- rnorm(100)







CONV <- data.frame(Y,X1,X2)
Y_MASTER <- CONV$Y
X_MASTER <- CONV$X1 + CONV$X2

fit <- lm(Y ~ X1+X2) 





library(betategarch)
library(fGarch)






Y_DW <- (c(CONV$Y))
X1_DW <- (c(CONV$X1))
X2_DW <- (c(CONV$X2))
MODEL <- glm(Y_DW ~ X2_DW + X1_DW)





y <- (CONV$Y)
y_hat <- lag(predict.glm(MODEL))
MAPE<- mean(abs((y - y_hat)/y))
IC <- cor(y,y_hat, method = "pearson")
GODFREY <- bgtest(MODEL, order = 10)
GODFREY_COEF <-coeftest(GODFREY)

summary(MODEL)
vif(MODEL)
dwtest(MODEL)
forecast::checkresiduals(MODEL)
GODFREY_COEF
MAPE
lmtest::bptest(MODEL)
lmtest::gqtest(MODEL)
lmtest::hmctest(MODEL)




MODEL$aic
