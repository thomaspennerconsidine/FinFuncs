
{
  library(corrplot)
  library(ggplot2)
  library(quantmod)
  library(xts)
  library(rms)
  library(glmulti)
  library(leaps)
  library(stats)
  library(stats4)
  library(MASS)
} # package liberaries






data <- new.env()
tickers <- c("M2"
             , "WPRIME"
             , "EMRATIO"
             , "UNEMPLOY"
             , "CIVPART"
             , "GS3M"
             
             
             
)

# import data from FRED database

getSymbols( tickers
            , src = "FRED"  # needed!
            , env = data
            , adjust = TRUE
)



RANGE <- '1985-01/2016-01'

ROC_TYPE <- "discrete"# "continuous"   DELTA TYPE
DATE_CONVERT_TO <- "days"   # PERIOD TYPE TO CONVERT TOO
SMOOTH <- (30)




{
  #_#_#_#_#_#_#_#_#_# 
  
  data$x1 <-  SMA(   (log(lag.xts( (data$GS3M)     , k = 0 , na.pad = TRUE)))      , n = SMOOTH)
  data$x2 <-  SMA(   (log(lag.xts( (data$WPRIME)   , k = 0 , na.pad = TRUE)))      , n = SMOOTH)
  data$x3 <-  SMA(   (log(lag.xts( (data$EMRATIO)   , k = 0 , na.pad = TRUE)))     , n = SMOOTH)
  data$x4 <-  SMA(   (log(lag.xts( (data$CIVPART)   , k = 0 , na.pad = TRUE)))     , n = SMOOTH)
  
} #
{
  #_#_#_#_#_#_#_#_#_# 
  INP_1 <- data$x1
  DAYCON_1 <- merge(INP_1, xts(,seq(start(first(INP_1)),end(last(INP_1)),DATE_CONVERT_TO)))
  YES_SMO_1 <-  (na.fill(DAYCON_1 , "extend")) 
  
  INP_2 <- data$x2
  DAYCON_2 <- merge(INP_2, xts(,seq(start(first(INP_2)),end(last(INP_2)),DATE_CONVERT_TO)))
  YES_SMO_2 <- (na.fill(DAYCON_2 , "extend"))
  
  INP_3 <- data$x3
  DAYCON_3 <- merge(INP_3, xts(,seq(start(first(INP_3)),end(last(INP_3)),DATE_CONVERT_TO)))
  YES_SMO_3 <- (na.fill(DAYCON_3 , "extend"))
  
  INP_4 <- data$x4
  DAYCON_4 <- merge(INP_4, xts(,seq(start(first(INP_4)),end(last(INP_4)),DATE_CONVERT_TO)))
  YES_SMO_4 <- (na.fill(DAYCON_4 , "extend"))
  
} #DATE PERIOD NORMALIZER


plot((YES_SMO_1[RANGE]))
plot((YES_SMO_2[RANGE]))
plot((YES_SMO_3[RANGE]))
plot((YES_SMO_4[RANGE]))

#_#_#_#_#_#_#_#_#_# 
#)[RANGE]) )
#EMA( x , n = ROLL_SMOOTH )
x1 <-   drop(    ((YES_SMO_1))[RANGE]     )#
x2 <-   drop(    ((YES_SMO_2))[RANGE]     )#
x3 <-   drop(    ((YES_SMO_3))[RANGE]     )
x4 <-   drop(    ((YES_SMO_4))[RANGE]     )


