
impute_Gauss_ex <- function(X, beta=0){

  X[is.na(X[,1]),1]<-beta*X[is.na(X[,1]),2] + rnorm(sum(is.na(X[,1])), sd=sqrt(1-beta^2))
  X[is.na(X[,2]),2]<-beta*X[is.na(X[,2]),1] + rnorm(sum(is.na(X[,2])), sd=sqrt(1-beta^2))
  X
}

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

impute_rf <- function(missdf, ...){
  imputed <- mice(missdf, m = 1, method = "rf", printFlag = FALSE)
  mice::complete(imputed)
}

impute_missForest <- function(missdf) {
  missForest::missForest(xmis = missdf)$ximp
}

impute_sample <- function(missdf) {
  imputed <- mice(missdf, m = 1, method = "sample", printFlag = FALSE)
  mice::complete(imputed)
}

impute_mice <- function(missdf) {
  imputed <- mice(missdf, m = 1, printFlag = FALSE)
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


impute_knn <- function(X) {
  multiUS::KNNimp(X, k = 10, scale = TRUE, meth = "weighAvg", distData = NULL)
}



reticulate::source_python("python/miwae_gain.py")

make_integer_double <- function(data.frame){

  for (i in 1:ncol(data.frame)){
    if (is.integer(data.frame[[i]])){
      data.frame[[i]] <- as.double(data.frame[[i]])
    }
  }

  return(data.frame)

}

call_hyperimpute_fun <- function(missdf, method, ...) {
  reticulate::source_python("python/miwae_gain.py")
  saveRDS(missdf, "dupa5.RDS")
  seed <- sample(1:100000, 1)
  column_names <- colnames(missdf)
  imputed <- hyperimpute_imp(missdf, method = method, seed = seed, ...)
  if(is.null(imputed)) stop("Internal error. Function returned NULL")
  colnames(imputed) <- column_names
  imputed
}

impute_miwae <- function(missdf, ...){
  missdf <- make_integer_double(missdf)
  call_hyperimpute_fun(missdf, method = "miwae", ...)
}

impute_gain <- function(missdf, ...){
  missdf <- make_integer_double(missdf)
  call_hyperimpute_fun(missdf, method = "gain", ...)
}
