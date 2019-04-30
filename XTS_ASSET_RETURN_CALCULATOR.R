library(xts)
library(quantmod)
LAG <- 0
LEN <- 10


n <- c(1:LEN)

market <- c(
  12,
  11,
  13,
  14,
  11,
  10,
  10,
  9,
  14,
  13)


position <- c(
  1,
  1,
  0,
  1,
  1,
  0,
  0,
  0,
  1,
  0)



EXTENSE <- list()
EXTENSE$market <- xts( market, as.Date((first(n)):(last(n))) )
EXTENSE$position <- xts( position, as.Date((first(n)):(last(n))) )
EXTENSE <- do.call("merge", EXTENSE)
#________________ BASE
s <- EXTENSE
s$position <- s$position
s$return <-  (dailyReturn(s$market)) * Lag(s$position)
s$market <- s$market
s
charts.PerformanceSummary(cbind(dailyReturn(s$market), s$return))




