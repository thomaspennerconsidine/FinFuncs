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
tickers <- c("WM2NS"
             , "DSPI"
             , "LOANS"
             , "STDCBSL" 
             , "TOTCINSA"
             
             
)

# import data from FRED database

getSymbols( tickers
            , src = "FRED"  # needed!
            , env = data
            , adjust = TRUE
)



RANGE <- '2015-07/2017-03'

  data$x0 <-     ts(data$TOTCINSA[RANGE])
  data$x1 <- (ROC(data$x0))
  data$x1[1] = data$x1[2]
  
  X1 <- ar(data$x0, aic = TRUE, order.max = NULL, method = "yule-walker")
  X2 <- ar(data$x0, aic = TRUE, order.max = NULL, method = "burg")
  X3 <- ar(data$x0, aic = TRUE, order.max = NULL, method = "ols")
  X4 <- ar(data$x0, aic = TRUE, order.max = NULL, method = "yw")
  
  
  Y1 <- ar(data$x1, aic = TRUE, order.max = NULL, method = "yule-walker")
  Y2 <- ar(data$x1, aic = TRUE, order.max = NULL, method = "burg")
  Y3 <- ar(data$x1, aic = TRUE, order.max = NULL, method = "ols")
  Y4 <- ar(data$x1, aic = TRUE, order.max = NULL, method = "yw")
  
  
  plot(forecast(data$x0))
  plot(forecast.Arima(auto.arima(data$x0)))
  plot(forecast.ar(X1)) 
  plot(forecast.ar(X2))
  plot(forecast.ar(X3))
  plot(forecast.ar(X4))
  
  
  plot(forecast(data$x1))
  plot(forecast.Arima(auto.arima(data$x1)))
  plot(forecast.ar(Y1)) 
  plot(forecast.ar(Y2))
  plot(forecast.ar(Y3))
  plot(forecast.ar(Y4))
  
  
#  plot(forecast.ar(ar(data$x0, aic = TRUE, order.max = NULL, method = "mle")))
#  ar(data$x0, aic = TRUE, order.max = NULL,
#     method = c("yule-walker", "burg", "ols", "mle", "yw"),
#     na.action, series)
  