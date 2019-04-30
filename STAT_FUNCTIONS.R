library()

LEN <- c(100)
PRICE <- rnorm(LEN,10,.5)
POSITION <- round(rnorm(LEN, 0, .5),0)
#NOTE zerofill is awesome function

mkt <- PRICE
pos <- POSITION


n <- c(1:LEN)
UNUSED_0 <- "NULL"
UNUSED_1 <- "NULL"
UNUSED_2 <- "NULL"




XTSS <- list()
XTSS$mkt <- xts( mkt, as.Date((first(n)):(last(n))) )
XTSS$pos <- xts( pos, as.Date((first(n)):(last(n))) )
XTSS <- do.call("merge", XTSS)
#________________ BASE
s <- XTSS
s$pos <- s$pos
s$mkt <-  s$mkt




s$POS_AUG <-     s$pos
s$POS_AUG[is.na(s$POS_AUG)] = 0
s$POS_AUG[is.infinite(s$POS_AUG)] = 0


#______STRATS____________STRATS____________STRATS____________STRATS
#______STRATS____________STRATS____________STRATS____________STRATS





s$STRAT_1 <- Lag( (ifelse((s$POS_AUG < 0) == TRUE, 1,  0)) , 1) * (dailyReturn(s$mkt , type = "arithmetic" ))
s$STRAT_1[1] <- 0
s$mkt_returns <- (dailyReturn(s$mkt , type = "arithmetic" ))

charts.PerformanceSummary(cbind((s$STRAT_1),   (dailyReturn(s$mkt , type = "arithmetic" )))   , main = "STRATIGY")
chart.ACFplus(s$STRAT_1)
chart.ECDF((s$STRAT_1))
chart.Boxplot((s$STRAT_1))
chart.CumReturns((s$STRAT_1))
chart.ECDF((s$STRAT_1))
chart.Histogram((s$STRAT_1))
chart.TimeSeries((s$STRAT_1))
PerformanceAnalytics::ActivePremium(s$STRAT_1, s$mkt_returns)
PerformanceAnalytics::AverageDrawdown(s$STRAT_1)
PerformanceAnalytics::SortinoRatio((s$STRAT_1))
mean(s$STRAT_1)
charts.RollingRegression(s$STRAT_1, s$mkt_returns)
PerformanceAnalytics::DownsideFrequency(s$STRAT_1)
PerformanceAnalytics::UpsideFrequency(s$STRAT_1, MAR = 0)
