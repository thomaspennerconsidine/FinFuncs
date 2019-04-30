set.seed(1)
#__________
#n <- (20)
#x <- rnorm(n, mean = 10, sd = 10)
#y <- rnorm(n, mean = 5, sd = 5)
#__________

n <- (10)
x <- c(23.7,23.9,24.2,24.1,24.3,24.3,24.5,24.4,24.6,25)
y <- c(41.3,41.3,41.5,41.6,41.9,42.2,42.3,42.5,42.7,42.7)

x_bar <- mean(x)
y_bar <- mean(y)

Sx <- (x-x_bar)
Sy <- (y-y_bar)

SSx <- sum(Sx^2)
SSy <- sum(Sy^2)

SSxy <- sum(  (Sx)*(Sy)  )


