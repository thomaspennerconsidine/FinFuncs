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
MODEL1 <- lm(Y ~ X1 + X2 + X3 + X4 )





lm.full <- lm(formula = 
                #_____FORMULA SPACE
                Y ~ X1 + X2
              #____FORMULA SPACE
              , data =VARIABLES)




CONV <- data.frame(Y,X1,X2)
Y_MASTER <- CONV$Y
X_MASTER <- CONV$X1 + CONV$X2

plot(X_MASTER, Y_MASTER)

#scatterplot3d(X1,X2,Y, main="LOANS ")
#scatterplot3d(X1,X2,Y, pch=16, highlight.3d=TRUE,
#              type="h", main="LOANS ")

#s3d <-scatterplot3d(X1,X2,Y, pch=16, highlight.3d=TRUE,
#                    type="h", main="LOANS ")
#fit <- lm(Y ~ X1+X2) 
#s3d$plane3d(fit)



#plot3d(Y, X1, X2, col="red", size=3)

#open3d()
#x <- X1
#y <- X2
#z <- Y
#fit <- lm(z ~ x + y)
#plot3d(x, y, z,  col = "red")

#coefs <- coef(fit)
#a <- coefs["x"]
#b <- coefs["y"]
#c <- -1
#d <- coefs["(Intercept)"]
#planes3d(a, b, c, d, alpha = 0.5)

Y <- (ts(CONV$Y))
X1 <- (ts(CONV$X1))
X2 <- (ts(CONV$X2))
MODEL <- lm(Y ~ X2 + X1)

y <- ts(CONV$Y)
x1 <- ts(CONV$X1)
x2 <- ts(CONV$X2)
y_hat <- lag(predict.lm(MODEL),LAGGING_AUG_1)
IC <- cor(y,y_hat, method = "pearson")
GODFREY <- bgtest(MODEL, order = 100)
GODFREY_COEF <-coeftest(GODFREY)


lmtest::bptest(MODEL)
forecast::Pacf(x1)
forecast::Pacf(x2)
forecast::Pacf(y)
plot(RollingCorr(y,y_hat,2))

Y_rate <- ROC(Y,n = 1, type = "continuous")
X1_rate <- ROC(X1,n = 1, type = "continuous")
X2_rate <- ROC(X2,n = 1, type = "continuous")
MODEL_rate <- lm(Y_rate ~ X2_rate + X1_rate)

forecast::checkresiduals(MODEL_rate)
forecast::checkresiduals(MODEL)
#plot(y,y_hat)


summary(MODEL)
print(IC)
vif(MODEL)
print(bgtest(MODEL, order = 100))
forecast::accuracy(MODEL)

