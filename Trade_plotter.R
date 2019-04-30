library(depmixS4)
library(PerformanceAnalytics)
library(quantmod)
library(doMC)
getSymbols("SPY", src='google', adjust = TRUE)
spyRets <- na.omit(Return.calculate(Cl(SPY)))
set.seed(123)
hmm <- depmix(SPY.Close ~ 1, family = gaussian(), nstates = 3, data=spyRets)
hmmfit <- fit(hmm, verbose = FALSE)
post_probs <- posterior(hmmfit)
post_probs <- xts(post_probs, order.by=index(spyRets))
plot(post_probs$state)
summaryMat <- data.frame(summary(hmmfit))
colnames(summaryMat) <- c("Intercept", "SD")
bullState <- which(summaryMat$Intercept > 0)
bearState <- which(summaryMat$Intercept < 0)
hmmRets <- spyRets * lag(post_probs$state == bullState) - spyRets * lag(post_probs$state == bearState)
charts.PerformanceSummary(hmmRets)
table.AnnualizedReturns(hmmRets)
#_
table.AnnualizedReturns(hmmRets)
data(edhec)
#tmp &lt;- CVaR(edhec, portfolio_method = &quot;component&quot;)
#_
dailyHMM <- function(data, nPoints) {
subRets <- data[1:nPoints,]
hmm <- depmix(SPY.Close ~ 1, family = gaussian(), nstates = 3, data = subRets)
hmmfit <- fit(hmm, verbose = FALSE)
post_probs <- posterior(hmmfit)
summaryMat <- data.frame(summary(hmmfit))
colnames(summaryMat) <- c("Intercept", "SD")
bullState <- which(summaryMat$Intercept > 0)
bearState <- which(summaryMat$Intercept < 0)
if(last(post_probs$state) %in% bullState) {
state <- xts(1, order.by=last(index(subRets)))
} else if (last(post_probs$state) %in% bearState) {
state <- xts(-1, order.by=last(index(subRets)))
} else {
state <- xts(0, order.by=last(index(subRets)))
}
colnames(state) <- "State"
return(state)
}
#_
tmp <- LongSeeker(c("XLP", "TLT"), c(.8, .6), rebalance_on="weeks",
displayStats = FALSE, outputReturns = TRUE)
table.AnnualizedReturns(tmp)
maxDrawdown(tmp)
CalmarRatio(tmp)
