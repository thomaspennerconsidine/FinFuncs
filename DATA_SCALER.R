N <- 1:1000

Z1 <- 1.1
normal_1_MU      <- 10
normal_1_SD_sqrd <- 2
lambda_1_messure <- 2
normal_1 <- rnorm(N,mean = normal_1_MU,sd = sqrt(normal_1_SD_sqrd) )
lambda_1 <- (round(runif(N, -1*lambda_1_messure, 1*lambda_1_messure),0))


Z2 <- 1/Z1
normal_2_SD_sqrd <- Z1*normal_1_SD_sqrd
normal_2_MU <- normal_1_MU*Z1
lambda_2_messure <- lambda_1_messure/Z1


normal_2 <-rnorm(N,mean = normal_2_MU,sd = sqrt( normal_2_SD_sqrd) )
lambda_2 <- (round(runif(N, -1*lambda_2_messure, lambda_2_messure),0))


SIDE_1_normal <- normal_1*Z1
SIDE_1_lambda <- lambda_1/Z1

SIDE_2_normal <- normal_2/Z1
SIDE_2_lambda <- lambda_2*Z1


SIDE_1 <- SIDE_1_normal*SIDE_1_lambda
SIDE_2 <- SIDE_2_normal*SIDE_2_lambda






#X_VAL <- SIDE_1
#Y_VAL <- SIDE_2
#REZ <- 100
#x_c <- cut(X_VAL, REZ)
#y_c <- cut(Y_VAL, REZ)
#z <- table(x_c, y_c)
#image2D(z=z)
#hist3D(z=z)

plot(density(SIDE_1))
lines(density(SIDE_2))
