d <- data.frame()
N <- 50
RANGE <- c(1:N)
BASE_PROB <- 0.5
BINOM = 1
PR = BASE_PROB
BINOM_2 <- 1
for (i in RANGE) {
  
 X <- PR
  
  BINOM[i] <- rbinom(1,1,prob = BASE_PROB)
  PR[i] <- ifelse( (BINOM[i] == 1) , .40,.60)
  BINOM_2[i] <- rbinom(1,1,prob = PR)
  
  BINOM_WRITE <- rbind(BINOM)
  pr <- rbind(PR)
  MARKOV_WRITE <- rbind(BINOM_2)
}
write

par(mfrow=c(2,1))
BINOM_AG <- (cumsum(BINOM_WRITE))/RANGE
plot(BINOM_AG, type = "l")


MARKOV_AG <- (cumsum(MARKOV_WRITE))/RANGE
plot(MARKOV_AG, type = "l")
last(BINOM_AG)
last(MARKOV_AG)