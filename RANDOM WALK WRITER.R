library(data.table)
N1 <- (10)
N2 <- (N1+1)
Z_MEAN <- (0)
Z_SD <- (1)
MAX <- (4)
n <-(c(1:MAX))
for(i in n)
{Sys.sleep(.5)
  #_ 
  RETURN_MEAN <- rnorm(1,0,0.3)
  RETURN_SD <- rnorm(1,0,0.3)
  u <- (RETURN_MEAN) # Expected annual return (%)
  sd <- (RETURN_SD) # Expected annual standard deviation (%)
  s <- (100) # Starting price
  #_
  a <- 2 #LEAVE ALONE
  t <- (1:N2) # LEAVE ALONE -- TIME
  Z <- rnorm(N1,Z_MEAN,Z_SD) # Random normally distributed values, mean = 0, stdv = 1
  price <- c(s) # Price vector
  for(j in Z)
  {
    S = s + s*(u/N1 + sd/sqrt(N1)*j)
    price[a] <- S 
    s = S 
    a = a + 1
  }
  DELTA <- ifelse( (is.na(ROC(price[t], type = "discrete", na.pad = TRUE))) == TRUE , 0 , (ROC(price[t], type = "discrete", na.pad = TRUE)) )
  FUNC <- price
  #_ 
  DATA_A <- FUNC
  DATA_B <- DELTA
  A <- paste("DATA_A", i, sep = "_") #1A
  A_OUT <- data.frame(A) #2A
  A_MOD <- data.frame(DATA_A) #3A
  colnames(A_MOD) = A #4A
  B <- paste("DATA_B", i, sep = "_") #1B
  B_OUT <- data.frame(B) #2B
  B_MOD <- data.frame(DATA_B) #3B
  colnames(B_MOD) = B #4B
  UNKNOWN <- lapply((1:MAX)[i], function(i) data.frame(A_MOD, B_MOD)) 
  NAMING <- c(1:N1)[i]
  names(UNKNOWN) <- as.character(NAMING)
  lapply(1:length(UNKNOWN), function(i) write.csv(UNKNOWN[[i]], 
                                                  file = paste0(names(UNKNOWN[i]), ".csv"),
                                                  row.names = FALSE))
  #_
  print(A)
  print(B)
  print(i)
  Sys.time()
}
