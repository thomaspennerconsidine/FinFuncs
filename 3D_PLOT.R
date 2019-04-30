
open3d()

#Y  <-VARIABLES$DSPI
#X1 <-  VARIABLES$STDCBSL
#X2 <- VARIABLES$LOANS

X1 <- rnorm(1000)
X2 <- rnorm(1000)
X3 <- rnorm(1000)
X4 <- rnorm(1000)
Y <- rnorm(1000)

MODEL1 <- data.frame(Y,X1,X2,X3,X4)

x <- X1
y <- X2
z <- Y
fit <- lm(z ~ x + y)
plot3d(x, y, z,  col = "red")

coefs <- coef(fit)
a <- coefs["x"]
b <- coefs["y"]
c <- -1
d <- coefs["(Intercept)"]
planes3d(a, b, c, d, alpha = 0.5)
