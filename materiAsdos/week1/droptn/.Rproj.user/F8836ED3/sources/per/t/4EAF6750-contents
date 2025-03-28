NewtonRhap = function(a,b,c){
  x0 = 1.1
  fx <- function(x){
    res = (a * x^2) + (b*x) + c
    return(res)
  }
  #fax = (2*a*x) + b
  fax <- function(x){
    res = (2*a*x) + b
    return(res)
  }
  tol = 0.0000001
  x_old <- x0
  x_new <- 0
  while (abs(x_new - x_old) > tol) {
    x_new = x_old - (fx(x_old)/fax(x_old))
    x_old = x_new
  }
  return(x_new)
}

NewtonRhap(1,0,-2)

