
library(MASS)
library(miceDRF)
library(dplyr)
library(ggplot2)

minmax <- function(x) (x - max(x, na.rm = T))/abs(min(x, na.rm = T) - max(x, na.rm = T))

targets::tar_source()

############################################################ gaussian


set.seed(123)
dat <- get_dat_ex1()
X <- dat$X_miss

methods <- c("sample","DRF", "cart", "rf", "norm.nob", "norm.predict")

imp_list <- c(create_mice_imputations(methods))

N_vec <- c(1, 5, seq(10, 100, 10))


res <- lapply(1:10, function(ith_rep) {
  lapply(N_vec, function(N) {
    print(N)
    c(Iscores_compare(X = X, imputation_list = imp_list, N = N),
      N = N,
      rep = ith_rep)
  }) %>%  bind_rows()
})

saveRDS(res, "./results/N/resN_gaussian.RDS")


