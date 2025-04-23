
library(miceDRF)
library(mice)

targets::tar_source()

set.seed(10)

# uniform dataset
dat <- get_dat_ex4()
X <- dat$X
X_miss <- dat$X_miss

# below are imputation methods that we use in uniform example
paste0("impute_", c("norm.nob", "DRF", "cart", "norm.predict",
                    "runif", "runifsq", "missForest"))

X_imp <- impute_runif(X_miss)

Iscore_beta(X_miss, X_imp, multiple = TRUE, N = 20,
            imputation_func = impute_runif, skip_if_needed = FALSE)

Iscore(X_miss, X_imp, multiple = TRUE, N = 20, imputation_func = impute_runif,
       skip_if_needed = FALSE)




