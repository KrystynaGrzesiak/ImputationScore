
library(miceDRF)
library(ggplot2)

set.seed(1500)

n <- 100
p <- 3

X <- matrix(rnorm(n * p, 0, 1), ncol = 3)
X_miss <- X
X_miss[runif(n) < 0.2, 1] <- NA

X_miss <- as.data.frame(X_miss)

rnorm_imp <- function(X_miss) {
  X_miss[is.na(X_miss)] <- rnorm(sum(is.na(X_miss)))
  X_miss
}

X_imp <- rnorm_imp(X_miss)

reps <- 10


plt_dat <- lapply(1:reps, function(i) {
  Ns <- seq(5, 100, 5)
  iscores <- unlist(lapply(Ns, function(N) {
    as.vector(miceDRF::Iscore(X_miss, X_imp, multiple = TRUE, N = N,
                              imputation_func = rnorm_imp, skip_if_needed = FALSE))
  }))

  cbind(N = Ns, IScore = iscores, rep = i)
}) |> do.call(rbind, args = _)


as.vector(miceDRF::Iscore(X_miss, X, multiple = TRUE, N = 50,
                          imputation_func = rnorm_imp, skip_if_needed = FALSE))


ggplot(plt_dat, aes(x = N, y = IScore, col = as.factor(rep))) +
  geom_point() +
  geom_path() +
  theme_minimal()

