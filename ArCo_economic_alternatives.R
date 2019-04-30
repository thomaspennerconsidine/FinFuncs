library(ArCo)
#https://www.r-bloggers.com/introducing-the-arco-package/
data("inflationNFP")
lapply(inflationNFP,dim)


fn=function(X,y){
  return(lm(y~X))
}
p.fn=function(model,newdata){
  b=coef(model)
  return(cbind(1,newdata) %*% b)
}

t0=34
ArCoNFP=fitArCo(data=inflationNFP,fn=fn,p.fn=p.fn,treated.unity=1,t0=t0,VCOV.type = "nw")
ArCoNFP$delta


ArCoNFP$p.value

plot(ArCoNFP,plot=1,display.fitted = TRUE)



FAHsp=inflationNFP$inflationFAH[,1]
real=cumprod(1+FAHsp/100)
cf=cumprod(1+c(FAHsp[1:(t0-1)],ArCoNFP$cf[,1])/100)
fitted=cumprod(1+fitted(ArCoNFP)[,1]/100)

plot(real,type="l",ylab="Y1",xlab="Time")
lines(c(rep(NA,t0-1),tail(cf,length(real)-t0+1)),col=4)
lines(fitted,col=2)
abline(v=t0,col=4,lty=2)
legend("topleft",legend=c("Observed","Fitted","Counterfactual"),col=c(1,2,4),lty=1,lwd=1,cex=1,seg.len = 1,bty="n")


