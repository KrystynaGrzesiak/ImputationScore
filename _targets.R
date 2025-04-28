
# options(clustermq.scheduler = "multiprocess")

library(targets)
library(tarchetypes)

library(ggplot2)

library(missForest)
library(mice)
library(miceDRF)
library(Iscores)
library(mvtnorm)

library(MASS)
library(dplyr)

tar_source()

methods <- c("norm.nob", "DRF", "cart", "missForest", "norm.predict")
methods_ex4 <- c("norm.nob", "DRF", "cart", "norm.predict", "runif", "runifsq", "missForest")
methods_ex5 <- c("norm.nob", "gaussian_indep")
methods_ex6 <- c("norm.nob", "DRF", "cart", "norm.predict", "runif", "runifsq", "missForest", "impute_dep_runif")
n_reps <- 10
N <- 50

set.seed(10)


list(
  tar_target(imp_fun_list,
             lapply(methods, function(ith_method) get(paste0("impute_", ith_method)))
  ),
  tar_target(imp_fun_list_ex4,
             lapply(methods_ex4, function(ith_method) get(paste0("impute_", ith_method)))
  ),
  tar_target(imp_fun_list_ex5,
             lapply(methods_ex5, function(ith_method) get(paste0("impute_", ith_method)))
  ),
  tar_target(imp_fun_list_ex6,
             lapply(methods_ex6, function(ith_method) get(paste0("impute_", ith_method)))
  ),
  # experiment 1: Gaussian Mixture Example
  tar_target(experiment_1,
             run_experiment(n_reps = n_reps, get_dat_fun = "get_dat_ex1",
                            imp_fun_list = imp_fun_list, methods_names = methods,
                            N = N)),
  # experiment 2: Nonlinear Mixture Example
  tar_target(experiment_2,
             run_experiment(n_reps = n_reps, get_dat_fun = "get_dat_ex2",
                            imp_fun_list = imp_fun_list, methods_names = methods,
                            N = N)),

  # experiment 4: Independent Uniform Example
  tar_target(experiment_4,
             run_experiment(n_reps = n_reps, get_dat_fun = "get_dat_ex4",
                            imp_fun_list = imp_fun_list_ex4,
                            methods_names = methods_ex4, num.proj = 1,
                            projection.function = function(X){2:ncol(X)},
                            N = N)),
  # experiment 5: Non-Strict Propriety Example
  tar_target(experiment_5,
             run_experiment(n_reps = n_reps, get_dat_fun = "get_dat_ex5",
                            imp_fun_list = imp_fun_list_ex5,
                            methods_names = methods_ex5, N = N)),
  # experiment 6: Dependent Uniform Example
  tar_target(experiment_6,
             run_experiment(n_reps = n_reps, get_dat_fun = "get_dat_ex6",
                            imp_fun_list = imp_fun_list_ex6,
                            methods_names = methods_ex4, num.proj = 1,
                            projection.function = function(X){2:ncol(X)},
                            N = N)),
  # datasets
  tar_target(data_ex1, get_dat_ex1()),
  tar_target(data_ex2, get_dat_ex2()),
  tar_target(data_ex4, get_dat_ex4()),
  tar_target(data_ex5, get_dat_ex5()),
  tar_target(data_ex6, get_dat_ex6())
)




