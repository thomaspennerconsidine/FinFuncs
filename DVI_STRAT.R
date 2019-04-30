

library(RCurl)
library(xts)
library(zoo)
library(quantmod) # data, plotting, quant modelling
library(PerformanceAnalytics) # performance and risk management
library(rugarch) #GARCH high volatility forecasting
library(knitr)


STOCK <- getSymbols("TVIX", from="2015-01-01", src = "google")

# Calculate DVI indicator
dvi <- DVI(Cl(TVIX))

# Construct trading rule
# Since this trading rule is simple--we're long 100% if the DVI is below 0.5 and short 100% otherwise--it can be written in a single line.

# Create signal: (long (short) if DVI is below (above) 0.5).
# Lag so yesterday's signal is applied to today's returns
sig <- Lag(ifelse(dvi$e1 < 0.5, 1, -1))

# The trading rules/equity curve
# The code below takes today's percentage return and multiplies it by yesterday's signal / position size (always +/- 100% in this example).

# Calculate signal-based returns
Trading_Strategy_Returns <- ROC(dvi$e1)*sig
# The trading rules/equity curve
Trading_Strategy_Returns <- ROC(Cl(TVIX))*sig

plot(Trading_Strategy_Returns)

retdrawDowns <- table.Drawdowns(Trading_Strategy_Returns, top=10)
kable(retdrawDowns)

retReturns <- table.CalendarReturns(Trading_Strategy_Returns); retReturns

retDownsideRisk <- table.DownsideRisk(Trading_Strategy_Returns)
kable(retDownsideRisk)

charts.PerformanceSummary(Trading_Strategy_Returns)
