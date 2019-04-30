
library(data.table)
library(TTR)
library(dplyr)
library(plyr)
library(quantmod)
library(PerformanceAnalytics)
library(xts)

SCALE<-(252) #TRADING DAYS PER YEAR 
#))))))))
#_____________
CON<-(100) #controll number
CON2<-(10) #decimal place rounder
CON7<-c(1) #FLIPPER
MovAvg<-(50) #ONLY FOR ROLLSUM OPTION#X>2 not less than 2
#_____________
MAX<-last(cumsum(count.fields("STOCK_1.csv", sep = "")))
THE_FIRST<-read.csv("STOCK_1.csv", header = FALSE, sep = "") #MAKE ONE BID ONE ASK

#LIBRIARIES

x<-THE_FIRST$V1
n<-c(1:MAX)
B <- 1:MAX
for(i in B )
{
  
  #_____________________
  
  #
  N <- (1:i)
  NEW <- data.frame(i,x[i])
  write.table(last(NEW),"export.csv", row.names=F,na="NA",append=T, sep=",", col.names=F)
  DATS <-read.csv("export.csv", header = FALSE, sep = ",")
  DATS = data.frame(tail(DATS, n = 5L))
  colnames(DATS)= c("N","x")
  
  
  print(DATS)
  
  #_____________________
  
  
}
write

