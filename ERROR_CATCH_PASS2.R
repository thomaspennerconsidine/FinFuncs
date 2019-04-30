VAR <- "cats"

f <- function(x) {
  
  tryCatch(
    sqrt(x)  # <-----   SOME OPERATION
    , error=function(e) print(
      print("NON_CURRENT")  # <------ DO THIS IF ERROR    
      ))
}
  
#Error in sqrt("a") : non-numeric argument to mathematical function

f("cats")
f(2)