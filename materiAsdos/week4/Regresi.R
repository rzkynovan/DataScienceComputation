df <- read.csv('./M4.csv'); head(df)

# NR
#1 Determine log-likelihood
log_likelihood <- function(beta, X, y){
  p <- 1/(1+exp(-X%*%beta))
  ll <- sum(y*log(p) + (1-y) * log(1-p))
  return(ll)
}

#2. Determine Gradient Function
gradient <- function(beta, X, y){
  p <- 1/(1+exp(-X%*%beta))
  grad <- t(X)%*%(y-p)
  return(grad)
}


# 3. Determine Hessian Matrix
hessian <- function(beta, X){
  p <- 1/(1+exp(-X%*%beta))
  W <- diag(as.vector(p*(1-p)))
  H <- -t(X)%*%W%*%X
  return(H)
}

#4. NR Algorithm
nr <- function(X, y, tol=1e-5, max.iter=1000){
  beta <- rep(0, ncol(X))
  for (i in 1:max.iter) {
    grad <- gradient(beta, X,y)
    H <- hessian(beta, X)
    beta_new <- beta-solve(H)%*%grad

    if(sqrt(sum(beta_new-beta)^2)<tol){
      cat('Convergence reached in ', i, ' iterations\n')
      return(beta_new)
    }
    beta <- beta_new
  }
  warning("Maximum iteration reached without convergence")
  return(beta)
}

library(dplyr)
df <- df %>%
  select(-X)
X <- cbind(1, as.matrix(df %>% select(-Purchased)));X
y <- df$Purchased
a <- nr(X, y)
fit <- 1/(1+exp(-X%*%a));fit

predicted_values <- ifelse(fit > 0.5, 1, 0); predicted_values
accuracy <- mean(y == predicted_values); accuracy

iwls <- function(X, y, tol=1e-5, max.iter = 1000){
  beta <- rep(0, ncol(X))
  for(i in 1:max.iter){
    p <- 1/(1+exp(-X%*%beta))
    W <- diag(as.vector(p*(1-p)))
    z <- X%*%beta + solve(W)%*%(y-p)
    Xtw <- t(X)%*%W
    Xtwx_inv <- solve(t(X)%*%W%*%X)
    beta_new = Xtwx_inv %*% (Xtw%*%z)
    if(sqrt(sum(beta_new-beta)^2) < tol){
      cat('Converged in', i, 'iterations\n')
      return(beta_new)
    }
    beta <- beta_new
  }
  warning("Maximum iteration reached without convergence")
  return(beta)
}


iwls(X, y)
nr(X,y)
