
library(miceDRF)
library(MASS)

set.seed(10)

mu <- rep(5, 6)
Sigma <- 2 * toeplitz(0.5^(0:5))

X <- mvrnorm(1500, mu = mu, Sigma = Sigma)

M <- lapply(1:3, function(j) {
  probs <- 1/(1 + exp(- rowMeans(X[, -j])/4))
  as.logical(sapply(probs, function(i) rbinom(1, 1, i)))
}) |> do.call(cbind, args = _) |> cbind(FALSE, FALSE, FALSE)

X_miss <- X

X_miss[M] <- NA

X_miss <- data.frame(X_miss)
X <- data.frame(X)


saveRDS(X, "./experiments/data/X_exp3.RDS")
saveRDS(X_miss, "./experiments/data/X_miss_exp3.RDS")
