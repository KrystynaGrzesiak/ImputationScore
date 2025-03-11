

run_experiment <- function(n_reps, get_dat_fun, imp_fun_list, methods_names,
                           num.proj = 200, projection.function = NULL) {

  lapply(1:n_reps, function(ith_rep) {

    dat <- get(get_dat_fun)()
    X_miss <- dat[["X_miss"]]
    X <- dat[["X"]]

    X_imp_list <- lapply(imp_fun_list, function(ith_imp) ith_imp(X_miss) )

    scores <- get_score_for_methods(X, X_miss, X_imp_list, imp_fun_list, methods_names)

    X_imp_list <- lapply(X_imp_list, function(ith) list(ith))

    drscores <- Iscores:::Iscores(X.NA = X_miss, imputations = X_imp_list,
                                  methods = methods_names, num.proj = num.proj,
                                  projection.function = projection.function)
    rbind(scores,
          data.frame(method = colnames(drscores),
                     value = as.vector(drscores),
                     score = "DR-I-Score"))

  }) %>%
    bind_rows()
}


get_score_for_methods <- function(X, X_miss, X_imp_list, imp_fun_list, methods_names) {

  lapply(1:length(methods_names), function(i) {
    print(paste0("Obtaining I-Score for ", methods_names[i], "..."))

    iscore <- miceDRF::Iscore(X = X_miss, X_imp = X_imp_list[[i]], multiple = TRUE, N = 50,
                              imputation_func = imp_fun_list[[i]], skip_if_needed = FALSE)

    energy_val <- as.vector(miceDRF::energy_dist(X = X, X_imp = X_imp_list[[i]]))
    nrmse_val <- nrmse(X, X_imp_list[[i]], X_miss)

    data.frame(method = methods_names[i],
               value = c(iscore, energy_val, nrmse_val),
               score = c("I-Score", "energy", "NRMSE"))
  }) %>%  bind_rows()
}


