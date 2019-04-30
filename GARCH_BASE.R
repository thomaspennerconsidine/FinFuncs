
library(betategarch)
library(fGarch)
BASE_DATA <- data.frame(rnorm(1000,0,10))


gfit.fg <- garchFit(data=BASE_DATA[,1], cond.dist="std")
coef(gfit.fg)

# plot in-sample volatility estimates
plot(sqrt(252) * gfit.fg@sigma.t, type="l")