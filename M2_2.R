
{
  library(corrplot)
  library(ggplot2)
  library(quantmod)
  library(xts)
  library(rms)
  #library(glmulti)
  library(leaps)
  library(stats)
  library(stats4)
  library(MASS)
  library(scatterplot3d)
  library(rgl)
  library(RollingWindow)
} # package liberaries






data <- new.env()
tickers <- c("M2"
             , "WPRIME"
             , "U6RATE"
             , "W055RC1"



             
             
             
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
LAGGING_AUG_1 <- (350)
LAGGING_AUG_2 <- (0)

{
  #_#_#_#_#_#_#_#_#_# 

  data$x0 <-  SMA(   (log(lag.xts( (data$M2)       , k = 0 , na.pad = TRUE)))     , n = SMOOTH)

  data$x5 <-  SMA(   (log(lag.xts( (data$U6RATE)   , k = LAGGING_AUG_2 , na.pad = TRUE)))     , n = SMOOTH)

  data$x14 <- SMA(   (log(lag.xts( (data$W055RC1)     , k = LAGGING_AUG_1 , na.pad = TRUE)))     , n = SMOOTH)

  
  
  #  , "UNEMPLOY"
  #  , "HOUST"
  #  , "TOTALSEC"
  
  
} #
{
  #_#_#_#_#_#_#_#_#_# 
  INP_0 <- data$x0
  DAYCON_0 <- merge(INP_0, xts(,seq(start(first(INP_0)),end(last(INP_0)),DATE_CONVERT_TO)))
  xx0 <-  (na.fill(DAYCON_0 , "extend"))[RANGE]

  
  INP_5 <- data$x5
  DAYCON_5 <- merge(INP_5, xts(,seq(start(first(INP_5)),end(last(INP_5)),DATE_CONVERT_TO)))
  xx5 <- (na.fill(DAYCON_5 , "extend"))[RANGE]
 
  INP_14 <- data$x14
  DAYCON_14 <- merge(INP_14, xts(,seq(start(first(INP_14)),end(last(INP_14)),DATE_CONVERT_TO)))
  xx14 <- (na.fill(DAYCON_14 , "extend"))[RANGE]
  

  
  
} #DATE PERIOD NORMALIZER

#M2~A229RX0






{
  M2       <- drop   (xx0)

  U6RATE   <- drop   (xx5)
  W055RC1  <- drop   (xx14)

  
  VARIABLES <- data.frame(
    
    M2,U6RATE,W055RC1
    
  )
  
}





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




library(stats)
library(lmtest)
library(quantmod)
library(randtests)
library(tseries)


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



#M2 ~  W055RC1 + U6RATE#YES #YESS

Y  <-M2 
X1 <-  W055RC1
X2 <- U6RATE







lm.full <- lm(formula = 
                #_____FORMULA SPACE
                Y ~ X1 + X2
              #____FORMULA SPACE
              , data =VARIABLES)




CONV <- data.frame(Y,X1,X2)
Y_MASTER <- CONV$Y
X_MASTER <- CONV$X1 + CONV$X2

#plot(X_MASTER, Y_MASTER)

#scatterplot3d(X1,X2,Y, main="M2")
#scatterplot3d(X1,X2,Y, pch=16, highlight.3d=TRUE,
#              type="h", main="M2t")

#s3d <-scatterplot3d(X1,X2,Y, pch=16, highlight.3d=TRUE,
#                    type="h", main="M2")
#fit <- lm(Y ~ X1+X2) 
#s3d$plane3d(fit)



#plot3d(Y, X1, X2, col="red", size=3)

#open3d()
#x <- X1
#y <- X2
#z <- Y
#fit <- lm(z ~ x + y)
#plot3d(x, y, z,  col = "red")

#coefs <- coef(fit)
#a <- coefs["x"]
#b <- coefs["y"]
#c <- -1
#d <- coefs["(Intercept)"]
#planes3d(a, b, c, d, alpha = 0.01)

Y <- (ts(CONV$Y))
X1 <- (ts(CONV$X1))
X2 <- (ts(CONV$X2))
MODEL <- lm(Y ~ X2 + X1)

y <- ts(CONV$Y)
x1 <- ts(CONV$X1)
x2 <- ts(CONV$X2)
y_hat <- lag(predict.lm(MODEL),LAGGING_AUG_1)
IC <- cor(y,y_hat, method = "pearson")
GODFREY <- bgtest(MODEL, order = 100)
GODFREY_COEF <-coeftest(GODFREY)


#lmtest::bptest(MODEL)
#forecast::Pacf(x1)
#forecast::Pacf(x2)
#forecast::Pacf(y)
#plot(RollingCorr(y,y_hat,2))
#
Y_rate <- ROC(Y,n = 1, type = "continuous")
X1_rate <- ROC(X1,n = 1, type = "continuous")
X2_rate <- ROC(X2,n = 1, type = "continuous")
MODEL_rate <- lm(Y_rate ~ X2_rate + X1_rate)

#forecast::checkresiduals(MODEL_rate)
#forecast::checkresiduals(MODEL)
plot(y,y_hat)
#dwtest(MODEL)  # probablity that true autocorrelation is greater than 0
#print(bgtest(MODEL, order = 100))
#vif(MODEL)
#print(IC)
#forecast::accuracy(MODEL)
#summary(MODEL)
#M2 ~ personal current taxes + Total unemployed, plus all marginally attached workers plus total employed part time for economic reasons



#summary(MODEL)
#print(IC)
#vif(MODEL)
#print(bgtest(MODEL, order = 100))
forecast::accuracy(MODEL)
dwtest(MODEL,)
dwtest(MODEL_rate)
