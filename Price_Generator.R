
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
set.seed(1)
MAX <- (10)
N <-c(1:MAX)
LEN <- (9)
N2 <- (MAX+1)
Z_MEAN_1 <- (0)
Z_MEAN_2 <- (0)
RETURN_MEAN <- rnorm( N , 0 , 0.001 )
RETURN_SD <- rnorm( N , 0 , 0.1 )
u <- (RETURN_MEAN)# Expected annual return (%)
sd <- (RETURN_SD)# Expected annual standard deviation (%)
s <- rnorm(2, 200, 15)
#_
a <- 0#LEAVE ALONE
t <- (1:N2)# LEAVE ALONE -- TIME
a <- 0#LEAVE ALONE
for(i in N )
{Sys.sleep(0.1) 
set.seed(1)
Z <- mvrnorm(n = MAX, mu = c(Z_MEAN_1,Z_MEAN_2), Sigma = matrix(c( 0.025 , .05 , .05 , 0.2 ), ncol = 2), empirical = TRUE)
STOCKS <- data.frame(Z)
s <- (rnorm(N, 200, 15))
price_1 <- (s[1])# Price vector
for(i_1 in STOCKS$X1)
{
S = s[1] + s[1]*(u/MAX + sd[1]/sqrt(MAX)*i_1)
price_1 <- S[1] 
s[1] = S 
a = a + 1
}
price_1 <- (S[i])
print(price_1)
#plot(price_1, y = NULL, type = "l")
#plot(price_2, y = NULL, type = "l")
Sys.time()
}
XXX <- data.frame(price_1 , N)
XXX