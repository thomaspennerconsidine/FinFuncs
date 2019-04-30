
{
  library(corrplot)
  library(ggplot2)
  library(quantmod)
  library(xts)
  library(rms)
 # library(glmulti)
  library(leaps)
  library(stats)
  library(stats4)
  library(MASS)
  library(scatterplot3d)
  library(lmtest)
  
} # package liberaries






data <- new.env()
tickers <- c("BPFADI03MXA637N"
             , "GDP"
             , "UNRATE"
             
)

# import data from FRED database

getSymbols( tickers
            , src = "FRED"  # needed!
            , env = data
            , adjust = TRUE
)

RANGE <- '1997-01/2010-01'

ROC_TYPE <- "discrete"# "continuous"   DELTA TYPE
DATE_CONVERT_TO <- "days"   # PERIOD TYPE TO CONVERT TOO
SMOOTH <- (2)

{
  #_#_#_#_#_#_#_#_#_# 
  
  data$x0 <-  SMA(   (log(lag.xts( (data$BPFADI03MXA637N)       , k = 0 , na.pad = TRUE)))     , n = SMOOTH)
  data$x1 <-  SMA(   (log(lag.xts( (data$GDP)     , k = 0 , na.pad = TRUE)))     , n = SMOOTH)
  data$x2 <-  SMA(   (log(lag.xts( (data$UNRATE)   , k = 0 , na.pad = TRUE)))     , n = SMOOTH)
  
  
  
  
  #  , "UNEMPLOY"
  #  , "HOUST"
  #  , "TOTALSEC"
  
  
} #
{
  #_#_#_#_#_#_#_#_#_# 
  INP_0 <- data$x0
  DAYCON_0 <- merge(INP_0, xts(,seq(start(first(INP_0)),end(last(INP_0)),DATE_CONVERT_TO)))
  xx0 <-  drop(na.fill(DAYCON_0 , "extend"))[RANGE]
  
  INP_1 <- data$x1
  DAYCON_1 <- merge(INP_1, xts(,seq(start(first(INP_1)),end(last(INP_1)),DATE_CONVERT_TO)))
  xx1 <-  drop(na.fill(DAYCON_1 , "extend"))[RANGE]
  
  INP_2 <- data$x2
  DAYCON_2 <- merge(INP_2, xts(,seq(start(first(INP_2)),end(last(INP_2)),DATE_CONVERT_TO)))
  xx2 <- drop(na.fill(DAYCON_2 , "extend"))[RANGE]
  
  
  
  
  
} #DATE PERIOD NORMALIZER

#M2~A229RX0






{
  
  
  
  VARIABLES <- data.frame(xx0, xx1, xx2
                          
                          
  )
  
}







Y  <- VARIABLES$xx0
X1 <- VARIABLES$xx1
X2 <- VARIABLES$xx2




lm.full <- glm(formula = 
                #_____FORMULA SPACE
                Y ~ X1 + X2 
              #____FORMULA SPACE
              , data =VARIABLES)



lm.null <- glm(formula = Y ~ 1 , data =VARIABLES)



dwtest(lm.full, order.by = NULL, alternative = c("greater", "two.sided", "less"),
       iterations = 15, exact = NULL, tol = 1e-10, data = list())




MODELA1 <- stepAIC(lm.null, direction="forward", scale ~ X1 + X2 , trace = 10, keep = NULL, use.start = FALSE, k = 2) 
MODELA2 <- stepAIC(lm.null, direction="both", scale ~ X1 + X2 , trace = 10, keep = NULL, use.start = FALSE, k = 2) 



CONV <- data.frame(Y,X1,X2)
Y_MASTER <- CONV$Y
X_MASTER <- CONV$X1 + CONV$X2 

plot(X_MASTER, Y_MASTER)

scatterplot3d(X1,X2,Y, main="3D Scatterplot")
scatterplot3d(X1,X2,Y, pch=16, highlight.3d=TRUE,
              type="h", main="3D Scatterplot")

s3d <-scatterplot3d(X1,X2,Y, pch=16, highlight.3d=TRUE,
                    type="h", main="3D Scatterplot")
fit <- lm(Y ~ X1+X2) 
s3d$plane3d(fit)



plot3d(Y, X1, X2, col="red", size=3)

summary(lm.full)
vif(lm.full)


