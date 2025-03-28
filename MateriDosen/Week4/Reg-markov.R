# Fungsi untuk menghitung log-likelihood
log_likelihood <- function(beta, X, y) {
  p <- 1/(1+exp(-X %*% beta))
  ll <- sum(y * log(p) + (1-y) * log(1-p))
  return(ll)
}

# Fungsi gradien
gradient <- function(beta, X, y) {
  p <- 1/(1+exp(-X %*% beta))
  grad <- t(X) %*% (y - p)
  return(grad)
}

# Fungsi hessian
hessian <- function(beta, X) {
  p <- 1/(1+exp(-X %*% beta))
  W <- diag(as.vector(p * (1-p)))
  H <- -t(X) %*% W %*% X
  return(H)
}

# Metode Newton-Raphson (NR)
nr <- function(X, y, tol = 1e-5, max.iter = 1000) {
  beta <- rep(0, ncol(X))
  for (i in 1:max.iter) {
    grad <- gradient(beta, X, y)
    H <- hessian(beta, X)
    beta_new <- beta - solve(H) %*% grad
    if (sqrt(sum((beta_new - beta)^2)) < tol) {
      cat('Convergence reached in', i, 'iterations (NR)\n')
      return(beta_new)
    }
    beta <- beta_new
  }
  warning("Maximum iteration reached without convergence (NR)")
  return(beta)
}

# Metode IRLS
iwls <- function(X, y, tol = 1e-5, max.iter = 1000) {
  beta <- rep(0, ncol(X))
  for (i in 1:max.iter) {
    p <- 1/(1+exp(-X %*% beta))
    W <- diag(as.vector(p*(1-p)))
    z <- X %*% beta + solve(W) %*% (y-p)
    # Perhitungan beta baru dengan formula IRLS
    beta_new <- solve(t(X) %*% W %*% X) %*% (t(X) %*% W %*% z)
    if (sqrt(sum((beta_new - beta)^2)) < tol) {
      cat('Convergence reached in', i, 'iterations (IRLS)\n')
      return(beta_new)
    }
    beta <- beta_new
  }
  warning("Maximum iteration reached without convergence (IRLS)")
  return(beta)
}

# Fungsi utama untuk regresi logistik yang memilih metode berdasarkan input parameter
logistic_regression <- function(X, y, method = c("NR", "IRLS"), tol = 1e-5, max.iter = 1000) {
  method <- match.arg(method)
  if (method == "NR") {
    beta <- nr(X, y, tol, max.iter)
  } else if (method == "IRLS") {
    beta <- iwls(X, y, tol, max.iter)
  }
  fit <- 1/(1+exp(-X %*% beta))
  return(list(method = method, beta = beta, fit = fit))
}

# Contoh pemakaian:
# Misalnya, kita memiliki data 'df' dengan variabel penjelas dan variabel target 'Purchased'
# Persiapan data:
df <- read.csv('./M4.csv')
head(df)
df <- df %>% select(-X)
X <- cbind(1, as.matrix(df %>% select(-Purchased)))
y <- df$Purchased

# Panggil fungsi logistic_regression dengan pilihan metode, misalnya "NR" atau "IRLS"
result <- logistic_regression(X, y, method = "NR")
print(result)
