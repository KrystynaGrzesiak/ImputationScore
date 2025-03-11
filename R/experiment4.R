

get_dat_ex4 <- function(d = 6, n = 2000 ) {

  X <- matrix(runif(n = d*n), nrow = n, ncol = d)

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

