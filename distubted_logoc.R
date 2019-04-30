N <- 10
n <- c(1:N)


SENSITIVITY <- 2



  
RANDOMIZER <- (rnorm(N,0,.5))
OBJ_A <- (rnorm(N,0,.5))
OBJ_B <- (OBJ_A+RANDOMIZER)
OBJ_DIST <- ifelse(OBJ_A< OBJ_B,1,-1)
DENSI <- hist(OBJ_DIST)
CONDIFANCE <- round(mean(DENSI$density*DENSI$mids),SENSITIVITY)
DECISION <- ifelse(CONDIFANCE== 0, "NULL",ifelse(CONDIFANCE>0,"YES", "NO"))

DECISION
CONDIFANCE
plot(density(OBJ_DIST))



