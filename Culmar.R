#if (!require("TTR")) {
#     install.packages("TTR")
#     library(TTR)
#}
#if (!require("quantstrat")) {
#     if(!require("devtools")) {
#          install.packages("devtools")
#          require(devtools)
#     }
#     install_github("braverock/blotter") # dependency
#     install_github("braverock/quantstrat")
#}
#
#if (!require("IKTrading")){
#     install_github("IlyaKipnis/IKTrading", force=TRUE)
#}



library(devtools)
library(quantmod)
library(quantstrat)
library(TTR)
library(png)
#library(IKTrading)


stratStats <- function(rets) {
     stats <- rbind(table.AnnualizedReturns(rets), maxDrawdown(rets))
     stats[5,] <- stats[1,]/stats[4,]
     stats[6,] <- stats[1,]/UlcerIndex(rets)
     rownames(stats)[4] <- "Worst Drawdown"
     rownames(stats)[5] <- "Calmar Ratio"
     rownames(stats)[6] <- "Ulcer Performance Index"
     return(stats)
}



require(PerformanceAnalytics)

geomCalmar <- function(r) {
     rAnn <- Return.annualized(r)
     maxDD <- maxDrawdown(r)
     toHighwater <- 1/(1-maxDD) - 1
     out <- rAnn/toHighwater
     return(out)
}




getSymbols(c('AMZN', 'SPY', 'SHY'), from = '1990-01-01')
rets <- na.omit(cbind(Return.calculate(Ad(AMZN)), Return.calculate(Ad(SPY)), Return.calculate(Ad(SHY))))
compare <- rbind(table.AnnualizedReturns(rets), maxDrawdown(rets), CalmarRatio(rets), geomCalmar(rets))
rownames(compare)[6] <- "Geometric Calmar"
compare