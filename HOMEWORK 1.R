library(ggplot2)

X <- c(15,18,21,22,24,26,31,32,31,30)
Y <- c(21,24,23,20,24,26,22,25,25,30)
N <- c(1:10)



TEST <- data.frame(X, Y)
TEST$X_BAR <- (mean(X))
TEST$Y_BAR <- (mean(Y))
TEST$X_XBAR <- (X-TEST$X_BAR)
TEST$Y_YBAR <- (Y-TEST$Y_BAR)
TEST$SSX_SSY <- (TEST$X_XBAR*TEST$Y_YBAR)
TEST$X_SQURE <- (TEST$X_XBAR^2)
TEST$Y_SQURE <- (TEST$Y_YBAR^2)

sum(TEST$SSX_SSY)/(  sqrt(sum(TEST$X_SQURE)*(sum(TEST$Y_SQURE))))

plot(Y~X) 
abline(lm(Y~X)) 

cor.test(X,Y)

summary(X)
summary(Y)
cor(X,Y, use = "everything", method = "pearson")
cov(X,Y)



chisq.test(X)
chisq.test(Y)








