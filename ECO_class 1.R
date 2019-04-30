#bivariant data
X <- read.csv("STOCK_1.csv", header = FALSE)
Y <- read.csv("STOCK_2.csv", header = FALSE)

A <- read.csv("STOCK_3.csv", header = FALSE)
B <- read.csv("STOCK_4.csv", header = FALSE)

A <- c(84,93,92,87,104)
B <- c(650,680,700,750,760)
#RANDOM VARIABLE
#BINOMIAL
#GRE
#DEFINEED X
#BIVARYING DATA = 2 RANDOM VARIABLES
# rho = true corelation coefficent of the population

# Multiple Linear Regression Example
fit <- c(Y$V1, X$V1)
summary(fit) # show results

cor(x = A, y = B)


cor.test(X$V1, Y$V1,
         alternative = c("two.sided", "less", "greater"),
         method = c("pearson", "kendall", "spearman"),
         exact = NULL, conf.level = 0.95, continuity = FALSE)


Z <- c(84,93,92,87,104)
Q <- c(650,680,700,750,760)
BB <- (Z-(mean(Z)))*(Q-mean(Q))
BBB <- 

summary(Z)



library(quantmod)
library(xts)

data <- new.env()

# set dates
date.start <- "2000-01-01"
date.end <- "2012-12-31"

# set tickers
tickers <- c("FEDFUNDS", "GDPPOT", "DGS10")

# import data from FRED database
library("quantmod")
getSymbols( tickers
            , src = "FRED"  # needed!
            , from = date.start  # ignored
            , to = date.end  # ignored
            , env = data
            , adjust = TRUE
)

head(data$FEDFUNDS)

head(data$DGS10)
