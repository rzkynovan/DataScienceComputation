markov <- function(init,mat,n,labels) {
  if (missing(labels)) labels <- 1:length(init)
  simlist <- numeric(n+1)
  states <- 1:length(init)
  simlist[1] <- sample(states,1,prob=init)
  for (i in 2:(n+1))
  { simlist[i] <- sample(states,1,prob=mat[simlist[i-1],]) }
  labels[simlist]
}
mat <- read.csv("./lungcancer.csv",header=T)
head(mat)
init <- c(rep(0,22),1,rep(0,27)) # Starting state 23 is Lung
n <- 8
markov(init,mat,n)
length(init)

nrow(mat)
