
DATA_1 <- c(58.2,59.1,59,58.6,59.1)
DATA_2 <- c(253,256,257,256,258)
DATA_3 <- c(0.0411,0.0403,0.0395,0.0386,0.0385)
DATA_4 <- c(253,256,257,256,258)
DATA_5 <- (rnorm(10))

d <- data.frame(DATA_1,DATA_2,DATA_3,DATA_4)
M <- cor(d) # get correlations

library('corrplot') #package corrplot
corrplot(M, method = c("number") ) #plot matrix

X <- c(58.2,59.1,59,58.6,59.1)
Y <- c(253,256,257,256,258)

0.8824975
#_____


X1 <- c(58.2,59.1,59,58.6,59.1)
Y <- c(253,256,257,256,258) #

X2 <- c(0.0411,0.0403,0.0395,0.0386,0.0385)
Y <- c(253,256,257,256,258) 


cor(X1,X2)


