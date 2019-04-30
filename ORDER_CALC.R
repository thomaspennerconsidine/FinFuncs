library(zoo)
BASE_CAPITAL <- 1000
Mkt_value_MV <- c(90,
                  97,
                  104,
                  114,
                  106,
                  110,
                  100,
                  93,
                  103,
                  111,
                  114,
                  110,
                  102,
                  102,
                  96)

pos_SIG_base <- c(0,
                  0,
                  1,
                  0,
                  1,
                  0,
                  1,
                  0,
                  1,
                  1,
                  1,
                  0,
                  1,
                  0,
                  0)


pos_SIG_lag <- c(pos_SIG_base[-1], 0)
ORDER_CHAIN <-  (c( 0,   (ifelse(pos_SIG_base == 1 & pos_SIG_lag == 0, "SELL", 
                                 ifelse(pos_SIG_base == 1 & pos_SIG_lag == 1, "HOLD",
                                        ifelse(pos_SIG_base == 0 & pos_SIG_lag == 1 , "BUY",0))) ) ))
ORDER_CHAIN =ORDER_CHAIN[-length(ORDER_CHAIN)]
ORDERS_1 <- cumsum(ifelse(ORDER_CHAIN == "BUY", (Mkt_value_MV*-1),
                          ifelse(ORDER_CHAIN == "SELL", (Mkt_value_MV),0)))
ORDERS_2 <- (ifelse(ORDER_CHAIN == "HOLD", (Mkt_value_MV*-1), ORDERS_1))
PORTFOLIO_NET <- ifelse(ORDER_CHAIN == "HOLD", (Mkt_value_MV*-1)+BASE_CAPITAL, (ORDERS_2)+BASE_CAPITAL)
REALIZED <- ifelse(pos_SIG_base == 1, PORTFOLIO_NET+ abs(ORDERS_2), ORDERS_2+BASE_CAPITAL)
CASH_LINKAGE <- REALIZED-BASE_CAPITAL
CASH_CHAINED <- rep(NA,length(CASH_LINKAGE))

for (i in length(CASH_LINKAGE)) {
  print(i)
  CASH_CHAINED <- ifelse(ORDER_CHAIN == "SELL", CASH_LINKAGE, NA)
  CASH_CHAINED <- na.locf(CASH_CHAINED, na.rm = FALSE, fromLast = FALSE, maxgap = Inf)
  CASH_CHAINED[is.na(CASH_CHAINED)] = 0
  
}
DE_CHAINED <- c(0,diff(CASH_CHAINED))
CUMULATIVE <- cumsum(DE_CHAINED )

#write.table( (DE_CHAINED),"MKT_WEIGHT.csv", row.names=F,na="NA",append=T, sep=",", col.names=F)
plot(CUMULATIVE, type = "l", main = "CASHCHAINED")


data.frame(ORDER_CHAIN,ORDERS_1,ORDERS_2,PORTFOLIO_NET,REALIZED,CASH_LINKAGE,CASH_CHAINED,DE_CHAINED,CUMULATIVE)



