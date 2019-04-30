N1 <- (255)
N2 <- (N1+1)
Z_MEAN <- (0)
Z_SD <- (1)
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
# Time. Days to put on the x axis
for(i in Z)
{
  S = s + s*(u/N1 + sd/sqrt(N1)*i)
  price[a] <- S 
  s = S 
  a = a + 1
}
plot(t,price,main="Time series stock X",xlab="time",ylab="price", type="l",col="blue")
summary(price)
statistics<- c(sd(price),mean(price),(price[N2]-price[1])/price[1]*100)
names(statistics) <- c("Volatility","Average price)","Return %")
print(statistics)
u
