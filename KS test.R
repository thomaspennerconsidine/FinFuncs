X <- c(2300,5059,2874,2945,1518,1190,6500,1450,1100,1005)
Y <- c(31000,44600,40320,10100,9850,8725,38600,10550,11050,9880)

X_REL <- (Y/X)
Y_REL <- (X/Y)

X_AVG_MU <- (mean(Y))/mean(X)
Y_AVG_MU <- (mean(X))/mean(Y)

DEV_X <- (X_REL-X_AVG_MU)
DEV_Y <- (Y_REL-Y_AVG_MU)




EXPECTED_X <- (Y_AVG_MU*Y)
EXPECTED_Y <- (X_AVG_MU*X)

DIF_X <- (EXPECTED_X-X)
DIF_Y <- (EXPECTED_Y-Y)



sample1 <- X
sample2 <- EXPECTED_Y
group <- c(rep("Donations", length(sample1)), rep("Expected Net worth", length(sample2)))
dat <- data.frame(KSD = c(sample1,sample2), group = group)
# create ECDF of data
cdf1 <- ecdf(sample1) 
cdf2 <- ecdf(sample2) 
# find min and max statistics to draw line between points of greatest distance
minMax <- seq(min(sample1, sample2), max(sample1, sample2), length.out=length(sample1)) 
x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )] 
y0 <- cdf1(x0) 
y1 <- cdf2(x0) 





ggplot(dat, aes(x = KSD, group = group, color = group))+
  stat_ecdf(size=1) +
  theme_bw(base_size = 28) +
  theme(legend.position ="top") +
  xlab("Sample") +
  ylab("ECDF") +
  #geom_line(size=1) +
  geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
               linetype = "dashed", color = "red") +
  geom_point(aes(x = x0[1] , y= y0[1]), color="red", size=8) +
  geom_point(aes(x = x0[1] , y= y1[1]), color="red", size=8) +
  ggtitle("K-S Test: Sample 1 / Sample 2") +
  theme(legend.title=element_blank())


