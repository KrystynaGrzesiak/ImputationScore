

get_dat_ex2 <- function(d = 6, n = 500) {

  patterns <- cbind(diag(1, 3), matrix(0, d - 3, d - 3))
  patterns <- apply(patterns, 2, as.logical)
  Sigma <- toeplitz(0.5^c(0, 1, 2))
  means <- rbind(rep(5, 3), rep(0, 3), rep(-5, 3))

  B <- matrix(c(0.5, 1, 1.5), byrow = TRUE, nrow = 3, ncol = 3)

  f <- function(x) {
    c(x[3] * sin(x[1] * x[2]), x[2] * (x[2] > 0), atan(x[1]) * atan(x[2]))
  }

  X <- lapply(1:3, function(ith_pattern) {
    obs <- mvrnorm(n = n, mu = means[ith_pattern, ], Sigma = Sigma)
    unobs <- t(apply(obs, 1, f)) + matrix(rnorm(ncol(obs) * n, 0, sqrt(4)), n, ncol(obs))
    cbind(unobs, obs)
  }) |>
    do.call(rbind, args = _)

  M <- lapply(1:3, function(ith) {
    matrix(rep(patterns[ith, ], 500), nrow = n, byrow = TRUE)
  }) %>%  do.call(rbind, args = .)

  X_miss <- X
  X_miss[M] <- NA

  X_miss <- data.frame(X_miss)
  X <- data.frame(X)

  list(X_miss = X_miss,  X = X)
}
