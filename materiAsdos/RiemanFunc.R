Rieman <- function(f,a,b,N){
  SUM = 0
  X = 0
  deltaX = (b-a)/N
  for (i in 1:N) {
    X[i] = a + i*deltaX
    SUM = SUM + f(X[i])
  }
  SUM = SUM * deltaX
  return(SUM)
}

fx <- function(x){
  hasil = 1/sqrt(2*3.14) * exp(-1/2*x^2)
  return(hasil)
}
a = -4
b = 4
Rieman(fx, -4, 4, 1000)

