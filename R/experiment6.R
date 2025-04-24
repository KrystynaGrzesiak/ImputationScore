get_dat_ex6 <- function(d = 6, n = 2000, rho=0 ) {
  
  
  correlation_matrix <- matrix(0, nrow = d, ncol = d)
  for (i in 1:d) {
    for (j in 1:d) {
      correlation_matrix[i, j] <- rho^abs(i - j)  # Correlation decays with distance
    }
  }
  
  
  #X <- matrix(runif(n = d*n), nrow = n, ncol = d)
  
  require(MASS)  # For mvrnorm function
  Z <- mvrnorm(n = n, mu = rep(0, d), Sigma = correlation_matrix)
  
  # Step 2: Transform to uniform via probability integral transform
  X <-pnorm(Z)
  
  vectors <- matrix(c(
    rep(0, d),
    0, 1, rep(0,d-2),
    1, rep(0,d-1)
  ), nrow = 3, byrow = TRUE)
  
  M <- vectors[apply(X,1, function(x) sample(1:3, size = 1, prob = c(x[1]/3, 2/3-x[1]/3, 1/3), replace = TRUE)), ]
  
  M <- apply(M, 2, as.logical)
  
  X_miss <- X
  X_miss[M] <- NA
  
  list(X_miss = as.data.frame(X_miss),  X = as.data.frame(X))
}

