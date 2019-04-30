#BASE FRAME START
#_________________

B = 1
W = 0
while(W<10 & B<=100){
  i=sample(0:1,1)
  if(i==1){
    W<-W+B 
    B<-B
  }else{
    B<-2*B
  }
  print(B)
}

#________________
#BASE FRAME END

W_dyn <- c()      # will store cumulative Win/Lose dynamics
W. <- 0           # total sum of Win/Lose        
step <- 0         # number of bet round - a cycle of bets till one of 
# conditions to stop happen:   W > 10 or B >= 100
while (abs(W.) < 1000)
{ B <- 1
while (W < 10 & B <= 100)
{ i <- sample(0:1, 1)
if (i == 1)
{ W <- W + B 
B <- 1
} else
{ W <- W - B
B <- 2 * B
} 
print(B)
}
W. <- W. + W
W <- 0
step <- step + 1
W_dyn[step] <- W.
cat("we have", W., "after", step, "bet rounds\n")
}
# then we can visualize our way to wealth or poverty
plot(W_dyn, type = "l")

#SCRIPT END