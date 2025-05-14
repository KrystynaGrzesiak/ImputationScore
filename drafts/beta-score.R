
library(miceDRF)
library(mice)
library(scoringRules)

targets::tar_source()

set.seed(10)



get_dat_ex41 <- function(d = 6, n = 2000, rho=0 ) {

  correlation_matrix <- matrix(0, nrow = d, ncol = d)
  for (i in 1:d) {
    for (j in 1:d) {
      correlation_matrix[i, j] <- rho^abs(i - j)  # Correlation decays with distance
    }
  }





# Step 1: Generate correlated normal variables
require(MASS)  # For mvrnorm function
Z <- mvrnorm(n = n, mu = rep(0, d), Sigma = correlation_matrix)

# Step 2: Transform to uniform via probability integral transform
X <-pnorm(Z)

  #X <- matrix(runif(n = d*n), nrow = n, ncol = d)

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



# impute_runifsqM <- function(X) {
#
#   ###Need to define a function that imputes using information of M.
#
#
#   #Problem: With projections we do not have the pattern information anymore!! (which I guess is exactly what we wanted)
#   M<-is.na(X)*1
#
#   ## X_1:
#   # pattern 1
#   X[which(rowSums(sweep(M, 2, c(0,0,0,0,0,0), "!=")) == 0), 1] <- sqrt(runif(sum(which(rowSums(sweep(M, 2, c(0,1,0,0,0,0), "!=")) == 0))))
#   # pattern 2
#   X[which(rowSums(sweep(M, 2, c(0,1,0,0,0,0), "!=")) == 0), 1] <- #sqrt(runif(sum(is.na(X)))) Continue here!!
#   # pattern 3
#   X[is.na(X[,2]), 2] <- runif(sum(is.na(X[,2])))
#
#   ## X_2:
#   # all patterns
#   X[is.na(X[,2]), 2] <- runif(sum(is.na(X[,2])))
# }

rho<-0

# uniform dataset
dat <- get_dat_ex41(rho=rho, n=2000)
X <- dat$X
X_miss <- dat$X_miss

if (rho==0){
X_imp <- impute_runif(X_miss)
Iscore_beta_v2(X_miss, X_imp, multiple = TRUE, N = 20,
             imputation_func = impute_runif, skip_if_needed = FALSE)

Iscore(X_miss, X_imp, multiple = TRUE, N = 20, imputation_func = impute_runif,
       skip_if_needed = FALSE)

}else{
  X_imp <- impute_dep_runif(X_miss)


  Iscore_beta(X_miss, X_imp, multiple = TRUE, N = 20,
               imputation_func = impute_dep_runif, skip_if_needed = FALSE)

  Iscore(X_miss, X_imp, multiple = TRUE, N = 20, imputation_func = impute_dep_runif,
         skip_if_needed = FALSE)

}


##Wrong method!
if (rho==0){

  #imputation.norm.nob <- miceDRF:::create_mice_imputation("norm.nob")
  X_imp_norm<-impute_runifsq(X_miss)

  Iscore_beta_v2(X_miss, X_imp_norm, multiple = TRUE, N = 20,
               imputation_func = impute_runifsq, skip_if_needed = FALSE)

  Iscore(X_miss, X_imp_norm, multiple = TRUE, N = 20, imputation_func = impute_runifsq,
         skip_if_needed = FALSE)

}else{

  imputation.norm.nob <- miceDRF:::create_mice_imputation("norm.nob")
  X_imp_norm<-imputation.norm.nob(X_miss)

  Iscore_beta(X_miss, X_imp_norm, multiple = TRUE, N = 20,
               imputation_func = imputation.norm.nob, skip_if_needed = FALSE)

  Iscore(X_miss, X_imp_norm, multiple = TRUE, N = 20, imputation_func = imputation.norm.nob,
         skip_if_needed = FALSE)

}




