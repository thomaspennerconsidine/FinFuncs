library(data.table)
library(TTR)
library(dplyr)
library(plyr)
library(quantmod)
library(PerformanceAnalytics)
library(xts)
library(MASS)
library(QuantTools)
#_____________
WINDOW <- 20
MAX<-last(cumsum(count.fields("STOCK_1.txt", sep = "")))
inputFile <- "STOCK_1.txt"
con  <- file(inputFile, open = "r")
line <- as.numeric(readLines(con, n = MAX))
B <- (WINDOW:MAX)

for(i in B )
{ #Sys.sleep(.4)
  #____________________
beg = Sys.time()
N =tail(union(c(1:i-1), c(i-1))+1, n = WINDOW )
x <- line[N]
DATS= NULL
DATS <- data.frame(x,N)
#___________________________________________________________
  write.table( (DATS),
               "DATA_STREAM.txt", 
               row.names=F,
               na="NA",
               append=F,
               sep=",", 
               col.names= c("STOCK_FEED", "N")
               )
#___________________________________________________________
XX <-read.table("DATA_STREAM.txt",
                header = TRUE,
                sep = ","
                )
#___________________________________________________________
  print(XX)
  end = Sys.time()
  print(end-beg)
  print(paste0("Itoration (",i, ")"," Time Differance of  (",round((end-beg)*10000,-1),")  MicroSeconds"))

#___________________________________________________________
  
}
