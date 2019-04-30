#------  BEGIN CODE
#::::
require(quantmod)
library(quantmod)
#::::
##  problem-01     IF YOU GET AN ERROR IT IS BECAUSE THE "quantmod" PKG DIDNT LOAD PROPERALY
##  problem-02     THE STOCK IS    "AMD"        THEY MAKE GRAPHICS CARDS ITS A STOCK TICKER
STOCK_NAME <-  "AMD"
getSymbols(Symbols = STOCK_NAME, src = "google")  # DONT EDIT
#::::
STOCK_NAME <- AMD   #  <<<<-------- MAKE THE SAME AS THE ONE IN QUOTES    
#____________________________________________________________________________
#::::
#::::
daysSinceHigh <- function(x, n){
  apply(embed(x, n), 1, which.max)-1
}
#::::
myStrat <- function(x, nHold=100, nHigh=200) {
  position <- ifelse(daysSinceHigh(x, nHigh)<=nHold,1,0)
  c(rep(0,nHigh-1),position)
}
#::::
myStock <- Cl(STOCK_NAME)
myPosition <- myStrat(myStock,100,200)
bmkReturns <- dailyReturn(myStock, type = "arithmetic")
myReturns <- bmkReturns*Lag(myPosition,1)
myReturns[1] <- 0
#::::
names(bmkReturns) <- 'SP500'
names(myReturns) <- 'Me'
#::::=
require(PerformanceAnalytics)
charts.PerformanceSummary(cbind(bmkReturns,myReturns))
#::::
Performance <- function(x) {
  #::::
  cumRetx = Return.cumulative(x)
  annRetx = Return.annualized(x, scale=252)
  sharpex = SharpeRatio.annualized(x, scale=252)
  winpctx = length(x[x > 0])/length(x[x != 0])
  annSDx = sd.annualized(x, scale=252)
  calmar = CalmarRatio(x, scale = 252)
  hurst = HurstIndex(x)
  kurtos = kurtosis(x, na.rm = FALSE, method = "excess") #, "moment", "fisher", "sample", "sample_excess"
  omega = Omega(x, L = 0 , method = "simple", Rf = 0)
  DDs <- findDrawdowns(x)
  maxDDx = min(DDs$return)
  maxLx = max(DDs$length)
  #::::
  Perf = c(cumRetx, annRetx, sharpex, winpctx, annSDx, maxDDx, maxLx, calmar, hurst, kurtos, omega)
  names(Perf) = c("Cumulative Return", "Annual Return","Annualized Sharpe Ratio",
                  "Win %", "Annualized Volatility", "Maximum Drawdown", "Max Length Drawdown", "CalmarRatio", "HurstIndex", "Kurtosis", "Omega")
  #::::
  return(Perf)
}
OUTPUT <- cbind(Me=Performance(myReturns),SP500=Performance(bmkReturns))
#::::
OUTPUT
#::::
write.csv(OUTPUT, file = "OUTPUT_STATISTICS.csv")
#::::
#------  END CODE


