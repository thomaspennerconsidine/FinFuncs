library(quantmod)
library(forecast)

Y  <- rnorm(100)



CUTOFF <- .01
#INPUT <- ts(rnorm(1000))+(c(1:1000)/10)
INPUT <- SMA((Y), n = 10)
INPUT = na.fill(INPUT, "extend")
WALK <- ROC(INPUT, n = 1, type = "discrete", na.pad = FALSE)
WALK1 <-ifelse(WALK<CUTOFF,WALK,0)
RAND_WALK <-ifelse(WALK1>-CUTOFF,WALK1,0)
acf(INPUT, lag.max = 100000, type = "correlation")






