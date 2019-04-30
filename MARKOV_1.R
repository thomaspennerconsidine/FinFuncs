library(markovchain)


STATE_REPLICATION_A_A <- 0.1
STATE_MOD <- .9
STATE_REPLICATION_B_B <- 0.2


statesNames=c(   "A_wins", "B_wins")

AA <- STATE_REPLICATION_A_A
BB <- STATE_REPLICATION_B_B
AB <- 1-STATE_REPLICATION_A_A
BA <- 1-STATE_REPLICATION_B_B
mcA<-new("markovchain", transitionMatrix=matrix(c(AA,AB,BA,BB),byrow=TRUE,
                                                nrow=2, dimnames=list(statesNames,statesNames)))


OUTPUT <- markovchainSequence(n=20, markovchain=mcA, include=TRUE)

OUTPUT