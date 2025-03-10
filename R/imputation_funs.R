
impute_runif <- function(X) {
  X[is.na(X)] <- runif(sum(is.na(X)))
  X
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
