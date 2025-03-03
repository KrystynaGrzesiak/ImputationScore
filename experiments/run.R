
source("./experiments/ex1.R")
source("./experiments/ex2.R")
source("./experiments/imputation.R")

methods <- c("DRF", "cart", "missForest", "norm.predict", "norm.nob")

set.seed(1)

# experiment 1

methods <- c("mixgb", "DRF", "cart", "missForest", "norm.predict", "norm.nob")

res_ex1 <- lapply(1:10, function(ith_rep) {
  dat <- get_ex1_dat()
  X_miss <- dat[["X_miss"]]
  X <- dat[["X"]]
  get_scores(X, X_miss, methods)

}) %>%
  bind_rows()

saveRDS(res_ex1, "./experiments/ex1.RDS")

# experiment 2

res_ex2 <- lapply(1:10, function(ith_rep) {
  dat <- get_ex2_dat()
  X_miss <- dat[["X_miss"]]
  X <- dat[["X"]]
  get_scores(X, X_miss, methods)

}) %>%
  bind_rows()

saveRDS(res_ex2, "./experiments/ex2.RDS")


# air quality data

source("./experiments/air_quality.R")

res_airquality <- lapply(1:10, function(ith_rep) {
  dat <- get_data()
  X_miss <- dat[["X_miss"]]
  X <- dat[["X"]]
  get_scores(X, X_miss, methods)

}) %>%
  bind_rows()

saveRDS(res_airquality, "./experiments/air_quality.RDS")

# experiment 4 - uniform

source("./experiments/ex4-uniform.R")

methods <- c("DRF", "cart", "norm.predict", "norm.nob", "sample", "runif")

res_ex4 <- lapply(1:10, function(ith_rep) {
  dat <- get_data()
  X_miss <- dat[["X_miss"]]
  X <- dat[["X"]]
  get_scores(X, X_miss, methods)

}) %>%
  bind_rows()

saveRDS(res_ex4, "./experiments/res_ex4.RDS")




