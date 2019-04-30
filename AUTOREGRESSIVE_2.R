{
  library(corrplot)
  library(ggplot2)
  library(quantmod)
  library(xts)
  library(rms)
 # library(glmulti)
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
tickers <- c("DRCCLACBN"
             
             
)

# import data from FRED database

getSymbols( tickers
            , src = "FRED"  # needed!
            , env = data
            , adjust = TRUE
)



RANGE <- '2010-07/2017-03'

  data$x0 <-     ts(data$DRCCLACBN[RANGE])
  
  data$x1 <- (ROC(data$x0))
  data$x1[1] = data$x1[2]
  
  
  A <- data$x0
  B <- data$x1

  X2 <- ar(data$x0, aic = TRUE, order.max = NULL, method = "burg")
  X3 <- ar(data$x0, aic = TRUE, order.max = NULL, method = "ols")

  
  
  Y1 <- ar(data$x1, aic = TRUE, order.max = NULL, method = "yule-walker")
  Y2 <- ar(data$x1, aic = TRUE, order.max = NULL, method = "burg")
  Y3 <- ar(data$x1, aic = TRUE, order.max = NULL, method = "ols")
  Y4 <- ar(data$x1, aic = TRUE, order.max = NULL, method = "yw")
  
  
  plot(forecast.ar(X2), main = "4")
  plot(forecast.ar(X3), main = "5")
  
  plot(forecast.ar(Y1), main = "9") 
  plot(forecast.ar(Y2), main = "10")
  plot(forecast.ar(Y3), main = "11")
  plot(forecast.ar(Y4), main = "12")
  
  plot(X2)
  plot(X3)
  plot(Y1)
  plot(Y2)
  plot(Y3)
  plot(Y4)
  
  
#  plot(forecast.ar(ar(data$x0, aic = TRUE, order.max = NULL, method = "mle")))
#  ar(data$x0, aic = TRUE, order.max = NULL,
#     method = c("yule-walker", "burg", "ols", "mle", "yw"),
#     na.action, series)
  