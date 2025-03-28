MATRIX <- function(DATA, KOLOM, BARIS, BYROW){
  res = matrix(0, KOLOM, BARIS)
  a=0
  if(BYROW == TRUE){
    for (i in 1:KOLOM) {
      for (j in 1:BARIS) {
        a = a+1
        res[i,j] = a
      }
    }

  } else {
    for (i in 1:BARIS) {
      for (j in 1:KOLOM) {
        a = a+1
        res[j, i] = a
      }
    }
  }
  return(res)
}

MATRIX(1:20, 4, 5, F)



