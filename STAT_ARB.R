library(quantmod)
library(PerformanceAnalytics)
momersion <- function(R, n, returnLag = 1) {
  momentum <- sign(R * lag(R, returnLag))
  momentum[momentum < 0] <- 0
  momersion <- runSum(momentum, n = n)/n * 100
  colnames(momersion) <- "momersion"
  return(momersion)
}


asset_a <- "XIV"
asset_b <- "VXX"
num_a <- as.name(asset_a)
num_b <- as.name(asset_b)

xiv <- getSymbols(asset_a,src="google")
vxx <- getSymbols(asset_b,src="google")
INT_A <- paste(num_a,"$", num_a,".", "Close", sep = "")
INT_B <- paste(num_b,"$", num_b,".", "Close", sep = "")

ASSET_A <- XIV$XIV.Close
ASSET_B <- VXX$VXX.Close


xivRets <- Return.calculate(Cl(ASSET_A))
vxxRets <- Return.calculate(Cl(ASSET_B))

volSpread <- xivRets + vxxRets
volSpreadMomersion <- momersion(volSpread, n = 252)
plot(volSpreadMomersion)




#both sides
sig <- -lag(sign(volSpread))
longShort <- sig * volSpread
charts.PerformanceSummary(longShort['2011::'], main = 'long and short spread')

#long spread only
sig <- -lag(sign(volSpread))
sig[sig < 0] <- 0
longOnly <- sig * volSpread
charts.PerformanceSummary(longOnly['2011::'], main = 'long spread only')


#short spread only
sig <- -lag(sign(volSpread))
sig[sig > 0] <- 0
shortOnly <- sig * volSpread
charts.PerformanceSummary(shortOnly['2011::'], main = 'short spread only')

threeStrats <- na.omit(cbind(longShort, longOnly, shortOnly))["2011::"]
colnames(threeStrats) <- c("LongShort", "Long", "Short")

#______
rbind(table.AnnualizedReturns(threeStrats), CalmarRatio(threeStrats))
