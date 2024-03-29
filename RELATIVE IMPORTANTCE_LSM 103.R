library(clusterGeneration)
library(nnet)
library(ggplot2)
library(plyr)
library(scales)
library(reshape)
ITORATIONS <- (1000) #A
a <- (rnorm(ITORATIONS, mean = 0, sd = 1) ) #A
b <- a*2 #A
M <- cbind(a,b) #A
OUTPUT_A <- cov(M) #A
num.vars<- 2 #number of variables #A
num.obs<- ITORATIONS # lenght of data #A
cov.mat<- OUTPUT_A #A
#ITORATIONS <- (8) #B
#a <- rnorm(ITORATIONS, 0,20) #B
#b <- rnorm(ITORATIONS, 0,10) #B
#c <- rnorm(ITORATIONS, 0,5) #B
#M <- rbind(a,b,c) #B
#num.vars<- ITORATIONS #B
#num.obs<- 3 #B
#OUTPUT_B <- cov(M) #B
#cov.mat<- OUTPUT_B #B
rand.vars<-mvrnorm(num.obs,rep(0,num.vars),Sigma=cov.mat) #Simulate from a Multivariate Normal Distribution
parms<-runif(num.vars,min(cov.mat),max(cov.mat)) #The Uniform Distribution
y <-rand.vars %*% matrix(parms) #y <-rand.vars %*% matrix(parms) + rnorm(num.obs,sd=20)
#prep data and create neural network
y <- data.frame((y-min(y))/(max(y)-min(y)))
names(y)<-'y'
rand.vars<-data.frame(rand.vars)
mod1<-nnet(rand.vars,y,size=2,linout=T) #Fit Neural Networks
gar.fun<-function(out.var,mod.in,bar.plot=T,struct=NULL,x.lab=NULL,
                  y.lab=NULL, wts.only = F){
  # function works with neural networks from neuralnet, nnet, and RSNNS package
  # manual input vector of weights also okay
  #sanity checks
  if('numeric' %in% class(mod.in)){
    if(is.null(struct)) stop('Three-element vector required for struct')
    if(length(mod.in) != ((struct[1]*struct[2]+struct[2]*struct[3])+(struct[3]+struct[2])))
      stop('Incorrect length of weight matrix for given network structure')
    if(substr(out.var,1,1) != 'Y' | 
       class(as.numeric(gsub('^[A-Z]','', out.var))) != 'numeric')
      stop('out.var must be of form "Y1", "Y2", etc.')
  }
  if('train' %in% class(mod.in)){
    if('nnet' %in% class(mod.in$finalModel)){
      mod.in<-mod.in$finalModel
      warning('Using best nnet model from train output')
    }
    else stop('Only nnet method can be used with train object')
  }
  #gets weights for neural network, output is list
  #if rescaled argument is true, weights are returned but rescaled based on abs value
  nnet.vals<-function(mod.in,nid,rel.rsc,struct.out=struct){
    if('numeric' %in% class(mod.in)){
      struct.out<-struct
      wts<-mod.in
    }
    #neuralnet package
    if('nn' %in% class(mod.in)){
      struct.out<-unlist(lapply(mod.in$weights[[1]],ncol))
      struct.out<-struct.out[-length(struct.out)]
      struct.out<-c(
        length(mod.in$model.list$variables),
        struct.out,
        length(mod.in$model.list$response)
      )  
      wts<-unlist(mod.in$weights[[1]]) 
    }
    #nnet package
    if('nnet' %in% class(mod.in)){
      struct.out<-mod.in$n
      wts<-mod.in$wts
    }
    #RSNNS package
    if('mlp' %in% class(mod.in)){
      struct.out<-c(mod.in$nInputs,mod.in$archParams$size,mod.in$nOutputs)
      hid.num<-length(struct.out)-2
      wts<-mod.in$snnsObject$getCompleteWeightMatrix()
      #get all input-hidden and hidden-hidden wts
      inps<-wts[grep('Input',row.names(wts)),grep('Hidden_2',colnames(wts)),drop=F]
      inps<-melt(rbind(rep(NA,ncol(inps)),inps))$value
      uni.hids<-paste0('Hidden_',1+seq(1,hid.num))
      for(i in 1:length(uni.hids)){
        if(is.na(uni.hids[i+1])) break
        tmp<-wts[grep(uni.hids[i],rownames(wts)),grep(uni.hids[i+1],colnames(wts)),drop=F]
        inps<-c(inps,melt(rbind(rep(NA,ncol(tmp)),tmp))$value)
      }
      #get connections from last hidden to output layers
      outs<-wts[grep(paste0('Hidden_',hid.num+1),row.names(wts)),grep('Output',colnames(wts)),drop=F]
      outs<-rbind(rep(NA,ncol(outs)),outs)
      #weight vector for all
      wts<-c(inps,melt(outs)$value)
      assign('bias',F,envir=environment(nnet.vals))
    }
    if(nid) wts<-rescale(abs(wts),c(1,rel.rsc))
    #convert wts to list with appropriate names 
    hid.struct<-struct.out[-c(length(struct.out))]
    row.nms<-NULL
    for(i in 1:length(hid.struct)){
      if(is.na(hid.struct[i+1])) break
      row.nms<-c(row.nms,rep(paste('hidden',i,seq(1:hid.struct[i+1])),each=1+hid.struct[i]))
    }
    row.nms<-c(
      row.nms,
      rep(paste('out',seq(1:struct.out[length(struct.out)])),each=1+struct.out[length(struct.out)-1])
    )
    out.ls<-data.frame(wts,row.nms)
    out.ls$row.nms<-factor(row.nms,levels=unique(row.nms),labels=unique(row.nms))
    out.ls<-split(out.ls$wts,f=out.ls$row.nms)
    assign('struct',struct.out,envir=environment(nnet.vals))
    out.ls
  }
  # get model weights
  best.wts<-nnet.vals(mod.in,nid=F,rel.rsc=5,struct.out=NULL)
  # weights only if T
  if(wts.only) return(best.wts)
  #get variable names from mod.in object
  #change to user input if supplied
  if('numeric' %in% class(mod.in)){
    x.names<-paste0(rep('X',struct[1]),seq(1:struct[1]))
    y.names<-paste0(rep('Y',struct[3]),seq(1:struct[3]))
  }
  if('mlp' %in% class(mod.in)){
    all.names<-mod.in$snnsObject$getUnitDefinitions()
    x.names<-all.names[grep('Input',all.names$unitName),'unitName']
    y.names<-all.names[grep('Output',all.names$unitName),'unitName']
  }
  if('nn' %in% class(mod.in)){
    x.names<-mod.in$model.list$variables
    y.names<-mod.in$model.list$response
  }
  if('xNames' %in% names(mod.in)){
    x.names<-mod.in$xNames
    y.names<-attr(terms(mod.in),'factor')
    y.names<-row.names(y.names)[!row.names(y.names) %in% x.names]
  }
  if(!'xNames' %in% names(mod.in) & 'nnet' %in% class(mod.in)){
    if(is.null(mod.in$call$formula)){
      x.names<-colnames(eval(mod.in$call$x))
      y.names<-colnames(eval(mod.in$call$y))
    }
    else{
      forms<-eval(mod.in$call$formula)
      x.names<-mod.in$coefnames
      facts<-attr(terms(mod.in),'factors')
      y.check<-mod.in$fitted
      if(ncol(y.check)>1) y.names<-colnames(y.check)
      else y.names<-as.character(forms)[2]
    } 
  }
  # get index value for response variable to measure
  if('numeric' %in% class(mod.in)){
    out.ind <- as.numeric(gsub('^[A-Z]','',out.var))
  } else {
    out.ind<- grep(out.var, y.names)
  }
  #change variables names to user sub 
  if(!is.null(x.lab)){
    if(length(x.names) != length(x.lab)) stop('x.lab length not equal to number of input variables')
    else x.names<-x.lab
  }
  if(!is.null(y.lab)){
    y.names<-y.lab
  } else {
    y.names <- y.names[grep(out.var, y.names)]
  }
  # organize hidden layer weights for matrix mult
  inp.hid <- best.wts[grep('hidden', names(best.wts))]
  split_vals <- substr(names(inp.hid), 1, 8)
  inp.hid <- split(inp.hid, split_vals)
  inp.hid <- lapply(inp.hid, function(x) t(do.call('rbind', x))[-1, ])
  # final layer weights for output
  hid.out<-best.wts[[grep(paste('out',out.ind),names(best.wts))]][-1]
  # matrix multiplication of output layer with connecting hidden layer
  max_i <- length(inp.hid)
  sum_in <- as.matrix(inp.hid[[max_i]]) %*% matrix(hid.out)
  # recursive matrix multiplication for all remaining hidden layers
  # only for multiple hidden layers
  if(max_i != 1){ 
    for(i in (max_i - 1):1) sum_in <- as.matrix(inp.hid[[i]]) %*% sum_in
    # final contribution vector for all inputs
    inp.cont <- sum_in 
  } else {
    inp.cont <- sum_in
  }
  #get relative contribution
  #inp.cont/sum(inp.cont)
  rel.imp<-{
    signs<-sign(inp.cont)
    signs*rescale(abs(inp.cont),c(0,1))
  }
  if(!bar.plot){
    out <- data.frame(rel.imp)
    row.names(out) <- x.names
    return(out)
  }
  to_plo <- data.frame(rel.imp,x.names)[order(rel.imp),,drop = F]
  to_plo$x.names <- factor(x.names[order(rel.imp)], levels = x.names[order(rel.imp)])
  out_plo <- ggplot(to_plo, aes(x = x.names, y = rel.imp, fill = rel.imp,
                                colour = rel.imp)) + 
    geom_bar(stat = 'identity') + 
    scale_x_discrete(element_blank()) +
    scale_y_continuous(y.names)
  return(out_plo)
}
#create a pretty color vector for the bar plot
#use the function on the model created above
par(mar=c(3,4,1,1),family='serif')
gar.fun('y',mod1)
a
