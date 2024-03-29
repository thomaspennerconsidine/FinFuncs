#set.seed(1)



ROUND <- 0
MEAN <- 10
SD <- 10
DISP <- 2
N <- 10000
DECAY_RATE <- .5

SAMPLE_1 <- round(N*DECAY_RATE, 0)
SAMPLE_2 <- round(SAMPLE_1*DECAY_RATE, 0)
SAMPLE_3<- round(SAMPLE_2*DECAY_RATE, 0)

DATA <- round(rnorm(1:N,MEAN,SD),ROUND)
MIN <- min(DATA)
MAX <- max(DATA)
MEAN <- mean(DATA)

MEAN_DISP <- (MEAN+DISP)

GENDER <- ifelse(DATA < MEAN_DISP, 1, 0)


FIRST_PROMOTION <- sample(GENDER,SAMPLE_1, replace = TRUE, prob = NULL)

SECOND_PROMOTION <- sample(FIRST_PROMOTION, SAMPLE_2, replace = TRUE, prob = NULL)

THIRD_PROMOTION <- sample(SECOND_PROMOTION, SAMPLE_3, replace = TRUE, prob = NULL)

hist(GENDER)
hist(FIRST_PROMOTION)
hist(SECOND_PROMOTION)
hist(THIRD_PROMOTION)

