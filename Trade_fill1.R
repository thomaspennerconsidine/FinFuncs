#IF SHARES OVERBUY       "TRDES"    *(sqrt(6)) 


library(htmlTable)
library(IBrokers)
library(Quandl)
library(quantmod)
library(RcppRoll)
library(rvest)
library(TTR)
library(xml2)
library(xts)
library(zoo)

tws <-  twsConnect(clientId = 1, host = "127.0.0.1", port = 7496, verbose = TRUE, timeout = 2) 


B <- (c(1:5))
for(i in B )
{Sys.sleep(2) 
  
  
  
  POSITION_A <- ("BUY")
  POSITION_B <- ("SELL")
  QUANTITY  <- (1)
  SYMBOL <- "DGAZ"
  ID_REQ <- reqIds(tws)
  
  
  QUANTITY_A <- (QUANTITY)  # (-1) = BUY    (+1) = SELL
  QUANTITY_B <- abs(QUANTITY_A)
  ifelse( ( (QUANTITY_B) == (0) ), ("NULL"), ifelse( ((QUANTITY_A) > 0) == FALSE , ((placeOrder(twsconn=tws,Contract=twsSTK(SYMBOL),Order=twsOrder(  ID_REQ,POSITION_A,(QUANTITY_B),"MKT")))), ((placeOrder(twsconn=tws,Contract=twsSTK(SYMBOL),Order=twsOrder(  ID_REQ,POSITION_B,(QUANTITY_B),"MKT" ))))) )
  
  
  
  
  print(i)
  Sys.time()
}
write

