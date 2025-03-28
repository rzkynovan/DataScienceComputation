LinearReg <- function(x,y){
  len = length(x)
  xbar = mean(x)
  ybar = mean(y)
  top = 0
  bottom = 0
  for (i in 1:len) {
    top = top + ((x[i] - xbar)*(y[i] - ybar))
    bottom = bottom + ((x[i]-xbar)*(x[i]-xbar))
    B1 = top/bottom
  }
  B0 = ybar - B1
  res = list(
    "BI" = B1,
    "B0" = B0
  )
  return(res)
}

x = c(1,2,3,4,5)
y = c(1.5,3,4.5,6,7.5)

LinearReg(x,y+0.33)

