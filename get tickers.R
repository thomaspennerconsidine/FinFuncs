library(quantmod)

symbols = stockSymbols()

symbols = symbols[,1]

for (i in seq_along(symbols)) {
  hyph = gregexpr(pattern = "-", symbols[i])
  per = gregexpr(pattern = "[.]", symbols[i])
  
  if (hyph[[1]][1] > 0 ) {
    symbols[i] = substr(symbols[i], 1, hyph[[1]][1] - 1)
    
  } else if (per[[1]][1] > 0 ) {
    symbols[i] = substr(symbols[i], 1, per[[1]][1] - 1)
  }
}

symbols = unique(symbols)
















for (i in seq_along(symbols)){
  tryit <- try(getSymbols(symbols[i],from="2016-01-01", src='yahoo'))
  if(inherits(tryit, "try-error")){
    i <- i+1
  }
  else {
    stock = getSymbols(symbols[i], from="2016-01-01", src = "yahoo", auto.assign = FALSE)
    stocks[[i]] = as.data.frame(stock)  
  }
}