#Setup 
rm(list = ls(all = TRUE)) 
set.seed(1)
nsims <- 50
library(quantmod) 
library(PerformanceAnalytics)
#Load Data 
getSymbols("TVIX", src = "google") 
spyReturns <- dailyReturn(TVIX$TVIX.Close , type = "arithmetic")
#Make 1000 random position vectors 
randomPositions <- sample(c(0,1),nsims*length(spyReturns),TRUE) 
randomPositions <- matrix(randomPositions,ncol=nsims)
#Determine trades and calculate trade costs 
trades <- apply(randomPositions,2,Lag,1) != randomPositions 
trades[1,] <- trades[1,]==1 
trades <- ifelse(trades,0.001,0)#Assume the commision is 0.5%
#Determine dailty returns of random strats 
randomStrats <- randomPositions*matrix(rep(spyReturns,nsims),ncol=nsims) 
randomStrats <- randomStrats-trades 
randomStrats.return <- apply(randomStrats,2,Return.annualized,scale=252) 
hist(randomStrats.return, freq=FALSE)
#Plot some of the strategies
library(PerformanceAnalytics)
charts.PerformanceSummary(na.omit(cbind(spyReturns,randomStrats)[,1:15]),colorset=redfocus)
#Same plot, removes buy&hold
#charts.PerformanceSummary(na.omit(cbind(spyReturns,randomStrats)[,2:15]),colorset=redfocus,)