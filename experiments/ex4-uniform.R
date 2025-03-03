

impute_runif <- function(X) {
  X[is.na(X)] <- runif(sum(is.na(X)))
  X
}

set.seed(2)

n <- 1000
d <- 3

get_data <- function() {
  # independent uniform
  X <- matrix(runif(n = d*n), nrow = n, ncol = d)
  # uniform with Gaussian copula
  #X <- gaussian_copula_uniform_sim(n = n, d = d)$uniform_data

  vectors <- matrix(c(
    rep(0, d),
    0, 1, rep(0,d-2),
    1, rep(0,d-1)
  ), nrow = 3, byrow = TRUE)

  # Generate random draws
  # sample() will generate indices, which we use to select rows from the matrix
  M <- vectors[apply(X,1, function(x) sample(1:3, size = 1, prob = c(x[1]/3, 2/3-x[1]/3, 1/3), replace = TRUE)), ]

  M <- apply(M, 2, as.logical)


  X_miss <- X
  X_miss[M] <- NA

  # saveRDS(X, "./experiments/data/X_exp4.RDS")
  # saveRDS(X_miss, "./experiments/data/X_miss_exp4.RDS")

  list(X_miss = X_miss,  X = X)

}









