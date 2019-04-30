

#BREAK BEGIN

library(data.table)
library(TTR)
library(dplyr)
library(plyr)
library(plot3D)
library(MASS)
library(RcppRoll)
library(quantmod)
library(PerformanceAnalytics)
library(xts)
library(blotter)
library(quantreg)
library(QuantTools)
library(RollingWindow)
#LIBRIARIES


SCALE <- (252) #TRADING DAYS PER YEAR 
#))))))))
#_____________
CON <- (100) #controll number
CON2 <- (10) # decimal place rounder
CON7 <- c(1) # FLIPPER
#_____________
MovAvg <- (50) #ONLY FOR ROLLSUM OPTION        #   X>2   not less than 2
MovAvg2 <- (2) #FOR END ADUSTMENT   #KEEP LOW
BTA_LENG <- (13)
#_____________
MAX <- last(cumsum(count.fields("STOCK_1.csv", sep = ""))) #(25000)
N <-c(1:(MAX))
LEN <- MAX
N2 <- "NULL"
#__________



#OP1
THE_FIRST <-   read.csv("STOCK_1.csv", header = FALSE, sep = "") # MAKE ONE BID ONE ASK
THE_SECOND <-   read.csv("STOCK_1.csv", header = FALSE, sep = "") # MAKE ONE BID ONE ASK






price_1 <- (data.frame(THE_FIRST^2))
price_2 <- (data.frame(THE_SECOND^2))
COV_1 <- cov(price_1$V1,price_2$V1)
VAR_1 <- var(price_1$V1)
COV_2 <- cov(price_2$V1,price_1$V1)
VAR_2 <- var(price_2$V1)
BETA_A <- c( RollingBeta(price_1$V1, price_2$V1, BTA_LENG, pop = FALSE) )   #POP = TRUE/FALSE NOT RELAVENT
BETA_B <- c( RollingBeta(price_2$V1, price_1$V1, BTA_LENG, pop = FALSE) )   #POP = TRUE/FALSE NOT RELAVENT #BETA CALC


BETA_B