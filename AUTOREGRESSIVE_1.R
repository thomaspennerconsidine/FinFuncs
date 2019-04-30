{
  library(corrplot)
  library(ggplot2)
  library(quantmod)
  library(xts)
  library(rms)
#  library(glmulti)
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
  library(forecast)
} # package liberaries

data <- new.env()
tickers <- c("TOTCINSA")
getSymbols(tickers, src = "FRED", env = data, adjust = TRUE)
RANGE <- '2015-07/2017-03'

  data$x0 <-     ts(data$TOTCINSA[RANGE])
  data$x1 <- (ROC(data$x0))
  data$x1[1] = data$x1[2]
  

  X1 <- ar(data$x1, aic = TRUE, order.max = NULL, method = "yule-walker")
  X2 <- ar(data$x1, aic = TRUE, order.max = NULL, method = "burg")
  X3 <- ar(data$x1, aic = TRUE, order.max = NULL, method = "ols")
  X4 <- ar(data$x1, aic = TRUE, order.max = NULL, method = "yw")
  
  
  Y1 <- ar(data$x1, aic = TRUE, order.max = NULL, method = "yule-walker")
  Y2 <- ar(data$x1, aic = TRUE, order.max = NULL, method = "burg")
  Y3 <- ar(data$x1, aic = TRUE, order.max = NULL, method = "ols")
  Y4 <- ar(data$x1, aic = TRUE, order.max = NULL, method = "yw")
  
  plot(forecast.ar(X1), main = "1")
  plot(X1, main = "1")
  plot(forecast.ar(X2), main = "2")
  plot(X1, main = "2")
  plot(forecast.ar(X2), main = "3")
  plot(X1, main = "3")
  plot(forecast.ar(X2), main = "4")
  plot(X1, main = "4")
  plot(forecast.ar(Y1), main = "5")
  plot(X1, main = "5")
  plot(forecast.ar(Y2), main = "6")
  plot(X1, main = "6")
  plot(forecast.ar(Y2), main = "7")
  plot(X1, main = "7")
  plot(forecast.ar(Y2), main = "8")
  plot(X1, main = "8")
  
  

  
  
#  plot(forecast.ar(ar(data$x0, aic = TRUE, order.max = NULL, method = "mle")))
#  ar(data$x0, aic = TRUE, order.max = NULL,
#     method = c("yule-walker", "burg", "ols", "mle", "yw"),
#     na.action, series)
  