require(mvtnorm)
xy <- rmvnorm(1000,c(5,10),sigma=rbind(c(3,-2),c(-2,3)))


library(quantmod)


data_1 = getSymbols('AMD', src = 'google', from = '1980-01-01', auto.assign = F)
data_2 = getSymbols('NVDA', src = 'google', from = '1980-01-01', auto.assign = F)

nbins <- 20
x.bin <- seq(floor(min(xy[,1])), ceiling(max(xy[,1])), length=nbins)
y.bin <- seq(floor(min(xy[,2])), ceiling(max(xy[,2])), length=nbins)

freq <-  as.data.frame(table(findInterval(xy[,1], x.bin),findInterval(xy[,2], y.bin)))
freq[,1] <- as.numeric(freq[,1])
freq[,2] <- as.numeric(freq[,2])

freq2D <- diag(nbins)*0
freq2D[cbind(freq[,1], freq[,2])] <- freq[,3]

par(mfrow=c(1,2))
image(x.bin, y.bin, freq2D, col=topo.colors(max(freq2D)))
contour(x.bin, y.bin, freq2D, add=TRUE, col=rgb(1,1,1,.7))

palette(rainbow(max(freq2D)))
cols <- (freq2D[-1,-1] + freq2D[-1,-(nbins-1)] + freq2D[-(nbins-1),-(nbins-1)] + freq2D[-(nbins-1),-1])/4
persp(freq2D, col=cols)