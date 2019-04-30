library( quantmod )
library( fArma )

# Get S&P 500


getSymbols( "IBM", src = "google")

# Compute the daily returns
gspcRets = diff( log( Cl( IBM) ) )

# Use only the last two years of returns
gspcTail = as.ts( tail( gspcRets, 500 ) )

# Fit the model
gspcArma = armaFit( formula=~arma(2,2), data=gspcTail )

xx <- IBM

xxArma = armaFit( xx ~ arma( 5, 1 ), data=xx )
xxArma@fit$aic



armaSearch = function(
  xx,  
  minOrder=c(0,0),
  maxOrder=c(5,5),
  trace=FALSE )
{
  bestAic = 1e9
  len = NROW( xx ) 
  for( p in minOrder[1]:maxOrder[1] ) for( q in minOrder[2]:maxOrder[2] )
  {
    if( p == 0 && q == 0 )
    {    
      next
    }    
    
    formula = as.formula( paste( sep="", "xx ~ arma(", p, ",", q, ")" ) )
    
    fit = tryCatch( armaFit( formula, data=xx ),
                    error=function( err ) FALSE,
                    warning=function( warn ) FALSE )
    if( !is.logical( fit ) )
    {    
      fitAic = fit@fit$aic
      if( fitAic < bestAic )
      {    
        bestAic = fitAic
        bestFit = fit
        bestModel = c( p, q )
      }    
      
      if( trace )
      {    
        ss = paste( sep="", "(", p, ",", q, "): AIC = ", fitAic )
        print( ss ) 
      }    
    }    
    else
    {    
      if( trace )
      {    
        ss = paste( sep="", "(", p, ",", q, "): None" )
        print( ss ) 
      }    
    }    
  }
  
  if( bestAic < 1e9 )
  {
    return( list( aic=bestAic, fit=bestFit, model=bestModel ) )
  }
  
  return( FALSE )
}



library( quantmod )
library( fArma )


spyRets = diff( log( Cl( xx) ) )
spyArma = armaFit( ~arma(0, 2), data=as.ts( tail( spyRets, 500 ) ) )
as.numeric( predict( spyArma, n.ahead=1, doplot=F )$pred )



