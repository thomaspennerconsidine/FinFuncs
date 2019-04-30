

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


X1 <- c(15,20,25,30,35,40
)



X2 <- c(
  40,
  38,
  42,
  38,
  27,
  28

)






Y <- c(62,72,82,82,102,112
  
  
  
)



MODEL1 <- data.frame(X1,Y)


lm.full <- lm(formula = Y ~ X1  , data = MODEL1)
lm.null <- lm(formula = Y ~ 1 , data =MODEL1)


MODELA1 <- stepAIC(lm.null, direction="forward", scale ~ X1 , trace = 10, keep = NULL, use.start = FALSE, k = 2) 
MODELA2 <- stepAIC(lm.null, direction="both", scale ~ X1 , trace = 10, keep = NULL, use.start = FALSE, k = 2) 




## Show 5 best models (Use @ instead of $ for an S4 object)




SAMPLE <- (lm(formula = Y ~ X1 , data = MODEL1))




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




summary(MODELA1)
vif(MODELA1)
#MODEL1
(MODELA1$call)
summary(MODELA1)
vif(MODELA1)
#MODEL2
(MODELA2$call)
summary(MODELA2)
vif(MODELA2)


summary(SAMPLE)
vif(SAMPLE)


