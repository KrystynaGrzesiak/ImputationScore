

library(targets)
library(tarchetypes)

library(ggplot2)

library(mixgb)
library(missForest)
library(mice)
library(miceDRF)
library(Iscores)

library(MASS)
library(dplyr)

tar_source()

methods <- c("norm.nob", "DRF", "cart", "missForest", "norm.predict")
n_reps <- 10

set.seed(10)


list(
  tar_target(imp_fun_list,
      lapply(methods, function(ith_method) get(paste0("impute_", ith_method)))
  ),
  # experiment 1
  tar_target(experiment_1,
    run_experiment(n_reps = n_reps, get_dat_fun = "get_dat_ex1",
                   imp_fun_list = imp_fun_list, methods_names = methods)),
  # experiment 2
  tar_target(experiment_2,
    run_experiment(n_reps = n_reps, get_dat_fun = "get_dat_ex2",
                   imp_fun_list = imp_fun_list, methods_names = methods)),
  # experiment 4
  tar_target(experiment_4,
             run_experiment(n_reps = n_reps, get_dat_fun = "get_dat_ex4",
                            imp_fun_list = imp_fun_list,
                            methods_names = c(methods, "runif")))
)




