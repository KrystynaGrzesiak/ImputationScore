
impute_runif <- function(X) {
  X[is.na(X)] <- runif(sum(is.na(X)))
  X
}


impute_dep_runif <- function(X) {
  # Transform back into Gaussian
  Y<-X
  Y[!is.na(X)]<-qnorm(X[!is.na(X)])
  
  imputed <- mice(Y, m = 1, method = "norm.nob", printFlag = FALSE)
  Yimp<-as.matrix(mice::complete(imputed))
  
  Ximp<-pnorm(Yimp)
  
  data.frame(Ximp)
}

impute_runifsq <- function(X) {
  X[is.na(X)] <- sqrt(runif(sum(is.na(X))))
  X
}

nrmse <- function(X, X_imp, X_miss) {
  observed <- X[is.na(X_miss)]
  imputed <- X_imp[is.na(X_miss)]

  sqrt(mean((observed - imputed)^2) / var(observed))
}

impute_mixgb <- function(missdf, ...){
  mixgb.data <- mixgb::mixgb(data = missdf, m = 1)
  mixgb.data <- mixgb.data[[1]]
  as.data.frame(mixgb.data)
}

impute_cart <- function(missdf, ...){
  imputed <- mice(missdf, m = 1, method = "cart", printFlag = FALSE)
  mice::complete(imputed)
}

impute_DRF <- function(missdf, ...){
  imputed <- mice(missdf, m = 1, method = "DRF", printFlag = FALSE)
  mice::complete(imputed)
}

impute_norm.predict <- function(missdf, ...){
  imputed <- mice(missdf, m = 1, method = "norm.predict", printFlag = FALSE)
  mice::complete(imputed)
}

impute_norm.nob <- function(missdf, ...){
  imputed <- mice(missdf, m = 1, method = "norm.nob", printFlag = FALSE)
  mice::complete(imputed)
}

impute_missForest <- function(missdf) {
  missForest::missForest(xmis = missdf)$ximp
}

impute_sample <- function(missdf) {
  imputed <- mice(missdf, m = 1, method = "sample", printFlag = FALSE)
  mice::complete(imputed)
}

impute_mean <- function(missdf) {
  imputed <- mice(missdf, m = 1, method = "mean", printFlag = FALSE)
  mice::complete(imputed)
}

impute_gaussian_indep <- function(X) {
  X[is.na(X)] <- rnorm(sum(is.na(X)))
  X
}

# impute_gaussian_corr <- function(X, rho = 0.7) {
#
#   n <- nrow(X)
#   p <- ncol(X)
#
#   # Sigma <- matrix(rho, nrow = p, ncol = p)
#   # diag(Sigma) <- 1
#
#   Sigma <- diag(p)
#   Sigma[1, 2] <- rho
#   Sigma[2, 1] <- rho
#
#   X_imp <- X
#   print("AAa")
#   for (i in 1:n) {
#     missing_idx <- which(is.na(X[i, ]))
#     observed_idx <- which(!is.na(X[i, ]))
#
#     if (length(missing_idx) == 0) next
#
#     mu_obs <- rep(0, length(observed_idx))
#     mu_mis <- rep(0, length(missing_idx))
#
#     Sigma_oo <- Sigma[observed_idx, observed_idx, drop = FALSE]
#     Sigma_mo <- Sigma[missing_idx, observed_idx, drop = FALSE]
#     Sigma_mm <- Sigma[missing_idx, missing_idx, drop = FALSE]
#
#     x_obs <- X[i, observed_idx]
#
#     cond_mean <- mu_mis + Sigma_mo %*% solve(Sigma_oo, x_obs - mu_obs)
#     cond_cov <- Sigma_mm - Sigma_mo %*% solve(Sigma_oo, t(Sigma_mo))
#
#     X_imp[i, missing_idx] <- rmvnorm(1, mean = cond_mean, sigma = cond_cov)
#   }
#
#   X_imp
# }

