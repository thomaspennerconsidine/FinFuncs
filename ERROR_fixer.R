TICKER_TRY <- "LULCATS"



Tick.Check <- function(x) {
  tryCatch(
    getQuote(x)
    , error=function(e) 
      data.frame(1, row.names = "NOTHING HERE")
    
  )
  return(x[1])
  options(warn=-1)
}   #CHECK IF SYMBOL IS VALID VIA (getQuote)


Tick.Check(TICKER_TRY)