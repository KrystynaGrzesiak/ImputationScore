
library(dplyr)
library(ggplot2)

library(mice)
library(mixgb)
library(miceDRF)
library(missForest)
library(Iscores)

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




get_score_for_method <- function(X, X_miss, X_imp, imp_fun, method) {

  iscore <- miceDRF::Iscore(X = X_miss, X_imp = X_imp, multiple = TRUE, N = 50,
                            imputation_func = imp_fun, skip_if_needed = FALSE)
  # drscore <- Iscores:::densityRatioScore(X = X_miss, Xhat = X_imp,
  #                                        num.proj = 1,
  #                                        projection.function = function(X){1:ncol(X)})
  energy_val <- as.vector(miceDRF::energy_dist(X = X, X_imp = X_imp))
  nrmse_val <- nrmse(X, X_imp, X_miss)

  data.frame(iscore = iscore, energy = energy_val, NRMSE = nrmse_val,
             method = method)
}








