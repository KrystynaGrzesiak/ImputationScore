
options(clustermq.scheduler = "multiprocess")

# suppress messages during fitting
lgr::get_logger("mlr3")$set_threshold("warn")

library(targets)
library(tarchetypes)
library(MASS)
library(dplyr)

# vis
library(ggplot2)
library(patchwork)

#imputation
library(missForest)
library(mice)
library(miceDRF)
library(Iscores)
library(mvtnorm)

#application
library(DoubleML)
library(ranger)
library(glmnet)
library(data.table)
library(mlr3)
library(mlr3learners)

tar_source()

set.seed(3)

# I-Score simulations params
methods <- c("norm.nob", "DRF", "cart", "missForest", "norm.predict")
methods_ex4 <- c("norm.nob", "DRF", "cart", "norm.predict", "runif", "runifsq",
                 "missForest")
methods_ex5 <- c("norm.nob", "gaussian_indep")
methods_ex6 <- c("norm.nob", "DRF", "cart", "norm.predict", "runif", "runifsq",
                 "missForest", "dep_runif")

# Application DML params
application_methods <- c("missForest", "sample", "knn", "rf", "gain", "cart")

# Global params
n_reps <- 10


list(
  # prepare imputation methods
  tar_target(imp_fun_list, get_methods(methods)),
  tar_target(imp_fun_list_ex4, get_methods(methods_ex4)),
  tar_target(imp_fun_list_ex5, get_methods(methods_ex5)),
  tar_target(imp_fun_list_ex6, get_methods(methods_ex6)),

  #################### beta score vol 1 ########################################

  # experiment 1: Gaussian Mixture Example
  tar_target(experiment_1,
             run_experiment(n_reps = n_reps,
                            get_dat_fun = "get_dat_ex1",
                            imp_fun_list = imp_fun_list,
                            methods_names = methods,
                            N = N),
             cue = tar_cue(mode = "never")),
  # experiment 2: Nonlinear Mixture Example
  tar_target(experiment_2,
             run_experiment(n_reps = n_reps,
                            get_dat_fun = "get_dat_ex2",
                            imp_fun_list = imp_fun_list,
                            methods_names = methods,
                            N = N),
             cue = tar_cue(mode = "never")),

  # experiment 4: Independent Uniform Example
  tar_target(experiment_4,
             run_experiment(n_reps = n_reps,
                            get_dat_fun = "get_dat_ex4",
                            imp_fun_list = imp_fun_list_ex4,
                            methods_names = methods_ex4,
                            num.proj = 1,
                            projection.function = function(X){2:ncol(X)},
                            N = N),
             cue = tar_cue(mode = "never")),
  # experiment 5: Non-Strict Propriety Example
  tar_target(experiment_5,
             run_experiment(n_reps = n_reps,
                            get_dat_fun = "get_dat_ex5",
                            imp_fun_list = imp_fun_list_ex5,
                            methods_names = methods_ex5,
                            N = N),
             cue = tar_cue(mode = "never")),
  # experiment 6: Dependent Uniform Example
  tar_target(experiment_6,
             run_experiment(n_reps = n_reps,
                            get_dat_fun = "get_dat_ex6",
                            imp_fun_list = imp_fun_list_ex6,
                            methods_names = methods_ex6,
                            num.proj = 1,
                            projection.function = function(X){2:ncol(X)},
                            N = N),
             cue = tar_cue(mode = "never")),
  # example datasets
  tar_target(data_ex1, get_dat_ex1()),
  tar_target(data_ex2, get_dat_ex2()),
  tar_target(data_ex4, get_dat_ex4()),
  tar_target(data_ex5, get_dat_ex5()),
  tar_target(data_ex6, get_dat_ex6()),

  #################### beta score vol 2 ########################################

  # experiment 1: Gaussian Mixture Example
  tar_target(experiment_1_beta,
             run_beta(n_reps = n_reps,
                      get_dat_fun = "get_dat_ex1",
                      imp_fun_list = imp_fun_list,
                      methods_names = methods,
                      N = N),
             cue = tar_cue(mode = "never")),
  # experiment 2: Nonlinear Mixture Example
  tar_target(experiment_2_beta,
             run_beta(n_reps = n_reps,
                      get_dat_fun = "get_dat_ex2",
                      imp_fun_list = imp_fun_list,
                      methods_names = methods,
                      N = N),
             cue = tar_cue(mode = "never")),
  # experiment 4: Independent Uniform Example
  tar_target(experiment_4_beta,
             run_beta(n_reps = n_reps,
                      get_dat_fun = "get_dat_ex4",
                      imp_fun_list = imp_fun_list_ex4,
                      methods_names = methods_ex4,
                      N = N),
             cue = tar_cue(mode = "never")),
  # experiment 5: Non-Strict Propriety Example
  tar_target(experiment_5_beta,
             run_beta(n_reps = n_reps,
                      get_dat_fun = "get_dat_ex5",
                      imp_fun_list = imp_fun_list_ex5,
                      methods_names = methods_ex5,
                      N = N),
             cue = tar_cue(mode = "never")),
  # experiment 6: Dependent Uniform Example
  tar_target(experiment_6_beta,
             run_beta(n_reps = n_reps,
                      get_dat_fun = "get_dat_ex6",
                      imp_fun_list = imp_fun_list_ex6,
                      methods_names = methods_ex6,
                      N = N),
             cue = tar_cue(mode = "never")),

  #################### DML application #########################################

  tar_target(application_data,
             DoubleML::fetch_401k(return_type = "data.table",
                                  instrument = TRUE)),
  tar_target(application_data_amputed, custom_ampute(application_data)),
  tar_target(results_full, {
    reticulate::source_python("python/miwae_gain.py")
    run_application(application_methods,
                    application_data,
                    application_data_amputed,
                    n_reps)
  })
)

