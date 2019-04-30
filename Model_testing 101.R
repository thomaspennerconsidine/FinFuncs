library(quantmod)
library(forecast)
CUTOFF <- .2
#INPUT <- ts(rnorm(1000))+(c(1:1000)/10)
INPUT <- ts(MODEL$residuals)
WALK <- ROC(INPUT, n = 1, type = "discrete", na.pad = FALSE)
WALK1 <-ifelse(WALK<CUTOFF,WALK,0)
RAND_WALK <-ifelse(WALK1>-CUTOFF,WALK1,0)

acf(RAND_WALK, lag.max = 10000)
acf(XX, lag.max = 10000)

