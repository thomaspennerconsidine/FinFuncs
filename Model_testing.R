library(quantmod)
library(forecast)

XX <- rnorm(1000)
CUTOFF <- .2
MODEL <- ts(rnorm(1000))+(c(1:1000)/10)
INPUT <- ts(MODEL)
WALK <- ROC(INPUT, n = 1, type = "discrete", na.pad = FALSE)
WALK1 <-ifelse(WALK<CUTOFF,WALK,0)
RAND_WALK <-ifelse(WALK1>-CUTOFF,WALK1,0)

acf(RAND_WALK, lag.max = 1000)
acf(XX, lag.max = 1000)

