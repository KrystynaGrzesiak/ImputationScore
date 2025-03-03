
set.seed(1)

d <- 6
n <- 500 # for each pattern

patterns <- cbind(diag(1, 3), matrix(0, d - 3, d - 3))
patterns <- apply(patterns, 2, as.logical)
Sigma <- toeplitz(0.5 ^ c(0, 1, 2))
means <- rbind(rep(5, 3), rep(0, 3), rep(-5, 3))

B <- matrix(c(0.5, 1, 1.5), byrow = TRUE, nrow = 3, ncol = 3)

X <- lapply(1:3, function(ith_pattern) {
  obs <- mvrnorm(n = n, mu = means[ith_pattern, ], Sigma = Sigma)
  unobs <- obs %*% B + matrix(rnorm(ncol(obs) * n, 0, sqrt(4)), n, ncol(obs))
  cbind(unobs, obs)
}) |>
  do.call(rbind, args = _)

M <- lapply(1:3, function(ith) {
  matrix(rep(patterns[ith, ], 500), nrow = n, byrow = TRUE)
}) |>  do.call(rbind, args = _)

X_miss <- X
X_miss[M] <- NA

X_miss <- data.frame(X_miss)
X <- data.frame(X)

X_imp <- mice::complete(mice::mice(X_miss, m = 1, method = "sample", printFlag = FALSE))


# drscore <- Iscores:::densityRatioScore(X = X_miss,
#                                        Xhat = X_imp,
#                                        num.proj = 1,
#                                        projection.function = function(X){1:ncol(X)})


X_imp <- impute_mixgb(X_miss)

miceDRF::Iscore(X = X_miss, X_imp = X_imp, multiple = TRUE, N = 50,
                imputation_func = impute_mixgb, skip_if_needed = FALSE)


mixgb.data <- mixgb::mixgb(data = missdf, m = 1)
mixgb.data <- mixgb.data[[1]]
as.data.frame(mixgb.data)
