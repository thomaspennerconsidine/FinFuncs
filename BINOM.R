require(data.table)
library(data.table)
d <- data.frame()
N <- 1000
RANGE <- c(1:N)
BASE_PROB <- .6
BINOM = 1
X = NULL
for (i in RANGE) {
  BINOM[i] <- rbinom(1,1,prob = BASE_PROB)
  d <- rbind(BINOM)
}
write

PROB_A <- (cumsum(d))/RANGE
plot(PROB_A, type = "l", col = "blue")
lines(PROB_A, type = "p", col = "red")
#last(PROB_A)

print(last(PROB_A))
print(BASE_PROB)
