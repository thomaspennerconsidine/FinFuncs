
library(htmlTable)
library(IBrokers)
library(Quandl)
library(quantmod)
library(RcppRoll)
library(rvest)
library(TTR)
library(xml2)
library(xts)
library(zoo)
price <- rnorm(100, mean = 50, sd=3)
n <- c(1:100)
ROUNDER <- (2)
Slow_Smoothing_Period <- c(5)
Fast_Smoothing_Period <- c(3)
Signal_Period <- c(2)
Slow_multiplier <- c(2/((n[Slow_Smoothing_Period])+1))
#_
Momentum <- momentum(price[n], n = 1, na.pad = TRUE) #right
Slow_EMA_Mo <- round(EMA(Momentum[n], n = Slow_Smoothing_Period, wilder = FALSE, ratio = NULL), ROUNDER)
Fast_EMA_EMA <- round( EMA(Slow_EMA_Mo, n = Fast_Smoothing_Period, wilder = FALSE, ratio = NULL), ROUNDER)
Abs_Momentum <- abs(Momentum[n])
Slow_EMA_AbsMo <- round(EMA(Abs_Momentum, n = Slow_Smoothing_Period, wilder = FALSE, ratio = NULL), ROUNDER)
Fast_EMA_AbsMo <- round(EMA(Slow_EMA_AbsMo, n = Fast_Smoothing_Period, wilder = FALSE, ratio = NULL), ROUNDER)
#_
TSI <- (100*( Fast_EMA_EMA[n]/Fast_EMA_AbsMo[n]) )
ERGODIC <- (TSI)
ERGODIC_SIGNAL_LINE <- round(EMA(TSI, n = Signal_Period, wilder = FALSE, ratio = NULL), ROUNDER)
Ergodic_Oscillator <- ((TSI) - (ERGODIC_SIGNAL_LINE))
write(price, file = "Price.csv",
ncolumns = 1,
append = TRUE, sep = "")
write(Ergodic_Oscillator, file = "SIG.csv",
ncolumns = 1,
append = TRUE, sep = "")
