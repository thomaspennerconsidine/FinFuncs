library(ggplot2)

X <- c(2300,5059,2874,2945,1518,1190,6500,1450,1100,1005)
Y <- c(31000,44600,40320,10100,9850,8725,38600,10550,11050,9880)
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








