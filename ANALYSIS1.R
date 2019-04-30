library(corrplot)
library(ggplot2)
library(quantmod)
library(xts)
library(rms)
library(glmulti)
library(leaps)
library(stats)
library(stats4)
library(MASS)
library(scatterplot3d)
library(rgl)
library(randtests)

Y  <- rnorm(100)
X1 <- rnorm(100)
X2 <- rnorm(100)




lm.full <- lm(formula = 
                #_____FORMULA SPACE
                Y ~ X1 + X2
              #____FORMULA SPACE
              , data =VARIABLES)




CONV <- data.frame(Y,X1,X2)
Y_MASTER <- CONV$Y
X_MASTER <- CONV$X1 + CONV$X2

plot(X_MASTER, Y_MASTER)

scatterplot3d(X1,X2,Y, main="DSPI  ")
scatterplot3d(X1,X2,Y, pch=16, highlight.3d=TRUE,
              type="h", main="DSPI  ")

s3d <-scatterplot3d(X1,X2,Y, pch=16, highlight.3d=TRUE,
                    type="h", main="DSPI  ")
fit <- lm(Y ~ X1+X2) 
s3d$plane3d(fit)



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


#summary(lm.full)
#vif(lm.full)


#library(betategarch)
#library(fGarch)



#gfit.fg <- garchFit(data=VARIABLES[,1], cond.dist="std")


# plot in-sample volatility estimates
#plot(sqrt(252) * gfit.fg@sigma.t, type="l")


#coef(gfit.fg)
#summary(gfit.fg)
#gfit.fg


#  gfit.fg <- garchFit(data=lm.full, cond.dist="std")

Y_DW <- (c(CONV$Y))
X1_DW <- (c(CONV$X1))
X2_DW <- (c(CONV$X2))
MODEL <- glm(Y_DW ~ X2_DW + X1_DW)

MODEL_FL <- glm(Y_DW ~ X2_DW + X1_DW)
d <- density(MODEL_FL$residuals) # returns the density data 
plot(d, main = "Y~X1+X2     Residual frequency distribution")

MODEL_X1 <- glm(Y_DW ~ X1_DW)
d <- density(MODEL_X1$residuals) # returns the density data 
plot(d, main = "Y~X1     Residual frequency distribution")

MODEL_X2 <- glm(Y_DW ~ X2_DW)
d <- density(MODEL_X2$residuals) # returns the density data 
plot(d, main = "Y~X2     Residual frequency distribution")


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
run



MODEL$aic
