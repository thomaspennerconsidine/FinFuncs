#DONE
{
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
  library(RollingWindow)
  library(lmtest)
  library(randtests)
  library(tseries)
  library(egcm)
  library(urca)
  library(forecast)
} # package liberaries






data <- new.env()
tickers <- c("WSAVNS"
             
             
)

# import data from FRED database

getSymbols( tickers
            , src = "FRED"  # needed!
            , env = data
            , adjust = TRUE
)



RANGE <- '2016-01/2017-02'

data$x0 <-     ts(data$WSAVNS[RANGE])

data$x1 <- ((data$x0))
data$x1[1] = data$x1[2]



X3 <- ar(data$x1, aic = TRUE, order.max = NULL, method = "ols")



plot(forecast.ar(X3))
grid(20, 20, lwd = 2) # grid only in y-direction
plot(X3)
plot(data$WSAVNS[RANGE], main = "Total Savings Deposits at all Depository Institutions")
# plot((data$WSAVNS[RANGE]), main = "Total Savings Deposits at all Depository Institutions")




#  plot(forecast.ar(ar(data$x0, aic = TRUE, order.max = NULL, method = "mle")))
#  ar(data$x0, aic = TRUE, order.max = NULL,
#     method = c("yule-walker", "burg", "ols", "mle", "yw"),
#     na.action, series)
