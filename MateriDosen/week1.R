even_series <- function(n){
  if(n<=0){
    return("Error")
  } else
  {
    len = (n*2)-1
    for (i in 0:len) {
      if(i%%2==0){
        res[i] = i
      }
    }
    res = na.omit(res)
    return(res)
  }
 
}

even_series(5.2)
