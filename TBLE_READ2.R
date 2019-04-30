
library(data.table)
library(TTR)
library(dplyr)
library(plyr)
library(quantmod)
library(PerformanceAnalytics)
library(xts)
library(MASS)



#_____________
WINDOW <- 10
MAX<-last(cumsum(count.fields("STOCK_1.txt", sep = "")))
inputFile <- "STOCK_1.txt"
con  <- file(inputFile, open = "r")
line <- as.numeric(readLines(con, n = MAX))
#read.table(con,skip=-1,nrow=2)
#LIBRIARIES
B <- (WINDOW:MAX)
for(i in B )
{ Sys.sleep(1)
  beg = Sys.time()
  #_____________________

DATS= NULL
#  NEXT= union(c(1:i-1), c(i-1))+1
N =tail(union(c(1:i-1), c(i-1))+1, n = WINDOW )
x <- line[N]
  DATS <- data.frame(N,x)

  
end = Sys.time()
  print(DATS)
  print(end-beg)
  beg = NULL
  end = NULL
  N = NULL
  X = NULL



  
  #_____________________
  
  
}


