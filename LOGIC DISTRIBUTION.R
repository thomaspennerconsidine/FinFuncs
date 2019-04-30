# Create a vector filled with random normal values
library(timeSeries)
library(quantmod)
library(TTR)
library(randtests)
library(quantmod)
library(lmtest)
library(randtests)
library(urca)
library(stats4)
library(stats)
library(forecast)
library(xts)

A <- rnorm(1)
B <- rnorm(1)*2
C <- rnorm(1)
D <- rnorm(1)*2

MAX <- 500
N <- 1:MAX
WINDOW <- c(4)

SENSITIVITY <- 2
RANDOMIZER <- rnorm(MAX,A,abs(B))

OBJ_A <- rnorm(MAX,C,abs(D))
OBJ_B <- OBJ_A+RANDOMIZER
data <- data.frame(N,RANDOMIZER,OBJ_A,OBJ_B)


for(i in N) {
  Sys.sleep(.0001) 


data$BINARY <- (ifelse(data$OBJ_A[data$N] < data$OBJ_B[data$N],1,-1))


HIST <- hist(data$BINARY[data$N[1]:data$N[i]])
data$NO_BIN[data$N[i]] <- first(HIST$density)
data$YES_BIN[data$N[i]] <- last(HIST$density)

NO_CONFID<- (last(SMA(data$NO_BIN, n=WINDOW)))
data$NO_CONFID[data$N[i]] <- NO_CONFID[data$N[1]:data$N[i]]

YES_CONFID<- (last(SMA(data$YES_BIN, n=WINDOW)))
data$YES_CONFID[data$N[i]] <- YES_CONFID[data$N[1]:data$N[i]]

#print(c(data$YES_BIN[i],data$NO_BIN[i]))


#plot()
print(i)
Sys.time()
}
data

plot(data$YES_CONFID, type = "l")
plot(SMA(data$YES_BIN, WINDOW))
plot(SMA(data$NO_BIN, WINDOW))
