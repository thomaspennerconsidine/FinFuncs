

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


X1 <- c(
  0,
  1,
  1,
  1,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  1,
  1,
  0,
  1,
  1,
  0,
  0,
  0,
  0
)



X2 <- c(
  40,
  38,
  42,
  38,
  27,
  28,
  29,
  33,
  30,
  55,
  45,
  43,
  41,
  40,
  38,
  26,
  29,
  29,
  34,
  63
)






Y <- c(
  105,
  86,
  46,
  55,
  39.5,
  47.6,
  48.2,
  49.2,
  50.3,
  55,
  87,
  115,
  118,
  76,
  59,
  65,
  88,
  76,
  48.6,
  56.6
  
  
  
  
)



MODEL1 <- data.frame(X1,X2,Y)


lm.full <- lm(formula = Y ~ X1 + X2 , data = MODEL1)
lm.null <- lm(formula = Y ~ 1 , data =MODEL1)


MODELA1 <- stepAIC(lm.null, direction="forward", scale ~ X1 + X2 , trace = 10, keep = NULL, use.start = FALSE, k = 2) 
MODELA2 <- stepAIC(lm.null, direction="both", scale ~ X1 + X2 , trace = 10, keep = NULL, use.start = FALSE, k = 2) 




## Show 5 best models (Use @ instead of $ for an S4 object)

regsubsets.out <-
  regsubsets(Y ~  X1 + X2 ,
             data = MODEL1,
             nbest = 1,       # 1 best model for each number of predictors
             nvmax = 1,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "exhaustive")




SAMPLE <- (lm(formula = Y ~ X1 + X2, data = MODEL1))




scatterplot3d(X1,X2,Y, main="3D Scatterplot")
scatterplot3d(X1,X2,Y, pch=16, highlight.3d=TRUE,
              type="h", main="3D Scatterplot")

s3d <-scatterplot3d(X1,X2,Y, pch=16, highlight.3d=TRUE,
                    type="h", main="3D Scatterplot")
fit <- lm(Y ~ X1+X2) 
s3d$plane3d(fit)



plot3d(Y, X1, X2, col="red", size=3)

summary(lm.full)
vif(lm.full)





summary(SAMPLE)
vif(SAMPLE)


