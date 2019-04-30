
library(onls)
#https://www.r-bloggers.com/introducing-orthogonal-nonlinear-least-squares-regression-in-r/
DNase1 <- subset(DNase, Run == 1)
DNase1$density <- sapply(DNase1$density, function(x) rnorm(1, x, 0.1 * x))
mod1 <- onls(density ~ Asym/(1 + exp((xmid - log(conc))/scal)),
               data = DNase1, start = list(Asym = 3, xmid = 0, scal = 1))

print(mod1)

plot(mod1, xlim = c(0, 0.5), ylim = c(0, 0.5))