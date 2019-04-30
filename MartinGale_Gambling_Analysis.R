moneyin <- 10000

gamble = function(moneyin){
  if( runif(1) < 0.5 ) return(moneyin) # result is black
  -moneyin # result is red
}

nextm = function(gamble, outcome){
  if(outcome < 0) return(2*gamble) #on lose we double the money
  1
}

simulate = function(maxt){
  df = data.frame(time=as.numeric(c()), money=as.numeric(c()))
  move = 0
  outcome = 0
  for(i in 1:maxt){
    move = nextm(move, outcome)
    outcome = gamble(move)
    df = rbind(df, data.frame(time=i, money=outcome))
  }
  df$money = cumsum(df$money)
  df
}

gamblersruin = function(num, maxsim){
  p = ggplot() 
  for(i in 1:num){
    df = simulate(maxsim)
    p = p + geom_line(data=df, aes(time,money), alpha=0.3)
  }
  p
}














set.seed(1)
gamblersruin(1,100) + ggtitle('single simulation, 100 bets')







f = function(k,n){
  1 - ( (2^k - 1) / ( 2^k ) )^n
}

df = data.frame(prob=f(9,1:10000), n=1:10000, limit=" 512")
df = rbind(df, data.frame(prob=f(10,1:10000), n=1:10000, limit='1024'))
df = rbind(df, data.frame(prob=f(11,1:10000), n=1:10000, limit='2048'))
df = rbind(df, data.frame(prob=f(12,1:10000), n=1:10000, limit='4096'))

p = ggplot()
p = p + geom_line(data=df, aes(n, prob, colour=limit))
p + ggtitle("probability of hitting budget limit after 'n' gambles")