library(quantmod)
library(PerformanceAnalytics)
library(forecast)
XX <- 2
MAG <- 1
s <- get(getSymbols('XIV', src = "google"))["2012::"]

cast <- (forecast.ar(ar( Cl(s), method = "burg", order.max = NULL, aic = TRUE)))
s$PRICE <- SMA(Cl(s),XX)
s$SIG_1 <-  ifelse( (diff(s$PRICE)) < 0 , diff(s$PRICE),-diff(s$PRICE))
s$SIG_2 <-  ifelse( (diff(diff((s$PRICE)))) < 0 , diff(diff((s$PRICE))),-(diff(diff((s$PRICE)))))
s$SIG_3 <-  ifelse( (diff(diff(diff(s$PRICE)))) < 0 , (diff(diff(diff(s$PRICE)))),-(diff(diff(diff(s$PRICE)))))
s$SIG_4 <-  ifelse( (diff(diff(diff(diff(s$PRICE))))) < 0 , (diff(diff(diff(diff(s$PRICE))))),-(diff(diff(diff(diff(s$PRICE))))))
s$ALL <- ( sin(s$SIG_1)+sin(s$SIG_2)+sin(s$SIG_3)+sin(s$SIG_4))
s$SIG <- sin(s$ALL )


s$position_A <- ifelse( ((s$SIG)) < 0 , 1,0)
s$position_B <- ifelse( ((s$SIG)) < 0 , 0,1)
s$position_C <- ifelse( ((s$SIG)) < 0 , 1,-1)
s$position_D <- ifelse( ((s$SIG)) > 0 , 1,0)
s$position_E <- ifelse( ((s$SIG)) > 0 , 0,-1)
s$position_F <- ifelse( ((s$SIG)) > 0 , 1,-1)

myReturn_A <- lag(s$position_A) * dailyReturn(s)
myReturn_B<- lag(s$position_B) * dailyReturn(s)
myReturn_C<- lag(s$position_C) * dailyReturn(s)
myReturn_D <- lag(s$position_D) * dailyReturn(s)
myReturn_E<- lag(s$position_E) * dailyReturn(s)
myReturn_F<- lag(s$position_F) * dailyReturn(s)


charts.PerformanceSummary(cbind(dailyReturn(s),myReturn_A, myReturn_B,myReturn_C, myReturn_D, myReturn_E, myReturn_F))