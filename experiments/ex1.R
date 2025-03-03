
## example Gaussian Mixture Model

library(miceDRF)
library(MASS)
library(dplyr)

set.seed(10)


get_ex1_dat <- function() {
  d <- 6
  n <- 500 # for each pattern

  patterns <- cbind(diag(1, 3), matrix(0, d - 3, d - 3))
  patterns <- apply(patterns, 2, as.logical)
  Sigma <- toeplitz(0.5^c(0, 1, 2))
  means <- rbind(rep(5, 3), rep(0, 3), rep(-5, 3))

  B <- matrix(c(0.5, 1, 1.5), byrow = TRUE, nrow = 3, ncol = 3)

  X <- lapply(1:3, function(ith_pattern) {
    obs <- mvrnorm(n = n, mu = means[ith_pattern, ], Sigma = Sigma)
    unobs <- obs %*% B + matrix(rnorm(ncol(obs) * n, 0, sqrt(4)), n, ncol(obs))
    cbind(unobs, obs)
  }) %>%
    do.call(rbind, args = .)

  M <- lapply(1:3, function(ith) {
    matrix(rep(patterns[ith, ], 500), nrow = n, byrow = TRUE)
  }) %>%  do.call(rbind, args = .)


  X_miss <- X

  X_miss[M] <- NA


  X_miss <- data.frame(X_miss)
  X <- data.frame(X)

  list(X_miss = X_miss,
       X = X)
}



# saveRDS(X, "./experiments/data/X_exp1.RDS")
# saveRDS(X_miss, "./experiments/data/X_miss_exp1.RDS")


