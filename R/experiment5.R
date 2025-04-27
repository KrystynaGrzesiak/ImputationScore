# get_dat_ex5 <- function(d = 4, n = 2000 ) {
#
#   X <- matrix(rnorm(n = (d+2)*n), nrow = n, ncol = d+2)
#
#   selections <- sample(1:3, size = n, replace = TRUE)
#
#   # Create the matrix with all zeros initially
#   M <- matrix(0, nrow = n, ncol = d+2)
#
#   # Set first column to 1 where selection was 2 (for vector (1,0,0))
#   M[selections == 2, 1] <- 1
#
#   # Set second column to 1 where selection was 3 (for vector (0,1,0))
#   M[selections == 3, 2] <- 1
#
#   M <- apply(M, 2, as.logical)
#
#   X_miss <- X
#   X_miss[M] <- NA
#
#   list(X_miss = as.data.frame(X_miss),  X = as.data.frame(X))
# }





get_dat_ex5 <- function(d = 6, n = 2000, rho = 0.7) {

  Sigma <- diag(d)
  Sigma[1, 2] <- rho
  Sigma[2, 1] <- rho

  X <- data.frame(MASS::mvrnorm(n, mu = rep(0, d), Sigma = Sigma))

  selections <- sample(1:3, size = n, replace = TRUE)

  # Create the matrix with all zeros initially
  M <- matrix(0, nrow = n, ncol = d)

  # Set first column to 1 where selection was 2 (for vector (1,0,0))
  M[selections == 2, 1] <- 1

  # Set second column to 1 where selection was 3 (for vector (0,1,0))
  M[selections == 3, 2] <- 1

  M <- apply(M, 2, as.logical)

  X_miss <- X
  X_miss[M] <- NA

  list(X_miss = as.data.frame(X_miss),  X = as.data.frame(X))
}
