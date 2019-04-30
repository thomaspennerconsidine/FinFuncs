if(FALSE) {
  
  #______
  #    plot(N, Mkt_Cumulative_Return_Log  ,lwd = 1, type = "l", main = "CUMULATIVE_(%)_RETURNS {Log}",  col = ("black"))
  #    lines(N,MY_CUMULATIVE_RETURN_Log        , col= "red"  , type = "l",lwd = 3)
  #    lines( N,My_Cumulative_Return_Log  , col="blue" , type = "l",lwd = 1)
  #><><><><><><><><><><><><><><><><><><><><><><><><><><><>>}
  plot(N,NET_MARKET_CHANGE  ,type="l",col="black",lwd = 1,  main = "EQUITY VALUE")
  lines(N,TRADING ,col="blue",lwd = 1)
  #><><><><><><><><><><><><><><><><><><><><><><><><><><><>>}
  plot(N, ORDERS_3 ,col="dimgray",lwd = 1,main = "TRADE_COST (CF)", type = "l")
  #><><><><><><><><><><><><><><><><><><><><><><><><><><><>>}
  
}