

library(deSolve)
#simulating some population growth from the logistic equation and estimating the parameters using nls
log_growth <- function(Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dN <- R*N*(1-N/K)
    return(list(c(dN)))
  })
}
#the parameters for the logisitc growth
pars  <- c(R=0.2,K=1000)
#the initial numbers
N_ini  <- c(N=1)
#the time step to evaluate the ODE
times <- seq(0, 50, by = 1)
#the ODE
out   <- ode(N_ini, times, log_growth, pars)
#add some random variation to it
N_obs<-out[,2]+rnorm(51,0,50)
#numbers cannot go lower than 1
N_obs<-ifelse(N_obs<1,1,N_obs)
#plot
plot(times,N_obs)


#find the parameters for the equation
SS<-getInitial(N_obs~SSlogis(times,alpha,xmid,scale),data=data.frame(N_obs=N_obs,times=times))


#we used a different parametrization
K_start<-SS["alpha"]
R_start<-1/SS["scale"]
N0_start<-SS["alpha"]/(exp(SS["xmid"]/SS["scale"])+1)
#the formula for the model
log_formula<-formula(N_obs~K*N0*exp(R*times)/(K+N0*(exp(R*times)-1)))
#fit the model
m<-nls(log_formula,start=list(K=K_start,R=R_start,N0=N0_start))
#estimated parameters
summary(m)
SS

cor(N_obs,predict(m))


lines(times,predict(m),col="red",lty=2,lwd=3)


