{
  library(corrplot)
  library(ggplot2)
  library(quantmod)
  library(xts)
  library(rms)
 #X library(glmulti)
  library(leaps)
  library(stats)
  library(stats4)
  library(MASS)
  library(scatterplot3d)
  library(rgl)
  library(RollingWindow)
  library(lmtest)
  library(randtests)
  library(tseries)
  library(egcm)
  library(urca)
} # package liberaries






data <- new.env()
tickers <- c("M2"
             , "DSPI"
             , "LOANS"
             , "STDCBSL" 
             
             
             
)

# import data from FRED database

getSymbols( tickers
            , src = "FRED"  # needed!
            , env = data
            , adjust = TRUE
)



RANGE <- '1995-01/2016-01'

ROC_TYPE <- "discrete"# "continuous"   DELTA TYPE
DATE_CONVERT_TO <- "days"   # PERIOD TYPE TO CONVERT TOO
SMOOTH <- (1)
LAGGING_AUG_1 <- (0)
LAGGING_AUG_2 <- (0)
LAGGING_AUG_3 <- (0)
{
  #_#_#_#_#_#_#_#_#_# 
  
  data$x0 <-     (data$DSPI)
  data$x5 <-    (data$LOANS)
  data$x14 <-  (data$STDCBSL)
  
  
  
  #_#_#_#_#_#_#_#_#_# 
  INP_0 <- data$x0
  DAYCON_0 <- merge(INP_0, xts(,seq(start(first(INP_0)),end(last(INP_0)),DATE_CONVERT_TO)))
  Xx0 <-  (SMA((na.fill(DAYCON_0 , "extend")), n = SMOOTH))
  xx0 <- Xx0[RANGE]
  
  INP_5 <- data$x5
  DAYCON_5 <- merge(INP_5, xts(,seq(start(first(INP_5)),end(last(INP_5)),DATE_CONVERT_TO)))
  Xx5 <- lag.xts((SMA((na.fill(DAYCON_5 , "extend")), n = SMOOTH))   , k = LAGGING_AUG_1 , na.pad = TRUE)
  xx5 <- Xx5[RANGE]
  
  INP_14 <- data$x14
  DAYCON_14 <- merge(INP_14, xts(,seq(start(first(INP_14)),end(last(INP_14)),DATE_CONVERT_TO)))
  Xx14 <- lag.xts((SMA((na.fill(DAYCON_14 , "extend")), n = SMOOTH))   , k = LAGGING_AUG_2 , na.pad = TRUE)
  xx14 <- Xx14[RANGE]
  
  
  
  
  
  DSPI       <- drop   (xx0)
  LOANS   <- drop   (xx5)
  STDCBSL  <- drop   (xx14)
}

VARIABLES <- data.frame(DSPI,LOANS,STDCBSL)

#M2~A229RX0
#M2      =   A229RX0 #YES
#PCE     = CLF16OV #YES
#CLF16OV = PCE #YES
#DSPI    = A229RX0 #YES
#A229RX0 = DSPI#YES
#PCE     =  DSPI#YES
#A229RX0 =  M2 #YES
#LOANS   ~ DSPI + STDCBSL  #YES #YESS
#DSPI    ~ LOANS + STDCBSL  #YES YESS
#M2 ~  W055RC1 + U6RATE#YES #YESS
#SAVINGSL ~ EMRATIO + LOANS + STDCBSL#YES #YESS
#M2      =   A229RX0 #YES
#PCE     = CLF16OV #YES
#CLF16OV = PCE #YES
#DSPI    = A229RX0 #YES
#A229RX0 = DSPI#YES
#PCE     =  DSPI#YES
#A229RX0 =  M2 #YES
#LOANS   ~ DSPI + STDCBSL  #YES #YESS
#DSPI    ~ LOANS + STDCBSL  #YES YESS
#M2 ~  W055RC1 + U6RATE#YES #YESS
#  Regression(B)      LOANS ~ DSPI + STDCBSL


Y  <-VARIABLES$STDCBSL
X1 <-  VARIABLES$DSPI
X2 <- VARIABLES$LOANS

CONV <- data.frame(Y,X1,X2)
Y_MASTER <- CONV$Y
X_MASTER <- CONV$X1 + CONV$X2
plot(X_MASTER, Y_MASTER)


Y <- (ts(CONV$Y))
X1 <- (ts(CONV$X1))
X2 <- (ts(CONV$X2))
MODEL <- lm(Y ~ X2 + X1)
forecast::checkresiduals(MODEL)

Y_rate <- ROC(Y,n = 1, type = "continuous")
X1_rate <- ROC(X1,n = 1, type = "continuous")
X2_rate <- ROC(X2,n = 1, type = "continuous")
MODEL_rate <- lm(Y_rate ~ X2_rate + X1_rate)
forecast::checkresiduals(MODEL_rate)

#y_hat <- lag(predict.lm(MODEL),LAGGING_AUG_1)
#IC <- cor(y,y_hat, method = "pearson")

vif(MODEL_rate)
dwtest(MODEL_rate)
vif(MODEL)
dwtest(MODEL)
#accuracy(MODEL)
INPUT <- Y
XX <- urca::ur.pp(INPUT, type = "Z-tau", model = "constant")
XX@cval
XX@teststat

#Engle–Granger cointegration test, from package "egcm"


