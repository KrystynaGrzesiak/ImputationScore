
get_res <- function(data, seed = 1) {
  set.seed(seed)
  features_base = c("age", "inc", "educ", "fsize", "marr", "twoearn", "db", "pira", "hown")

  # Initialize DoubleMLData (data-backend of DoubleML)
  data_dml_base = DoubleMLData$new(data, y_col = "net_tfa", d_cols = "e401", x_cols = features_base)
  data_dml_base

  # Set up a model according to regression formula with polynomials
  formula_flex = formula(" ~ -1 + poly(age, 2, raw=TRUE) +
                        poly(inc, 2, raw=TRUE) + poly(educ, 2, raw=TRUE) +
                        poly(fsize, 2, raw=TRUE) + marr + twoearn +
                        db + pira + hown")
  features_flex = data.frame(model.matrix(formula_flex, data))

  model_data = data.table("net_tfa" = data[, "net_tfa"], "e401" = data[, "e401"], features_flex)

  colnames(model_data)[1:2] <- c("net_tfa", "e401")

  # Initialize DoubleMLData (data-backend of DoubleML)
  data_dml_flex = DoubleMLData$new(model_data, y_col = "net_tfa", d_cols = "e401")

  # Random Forest
  randomForest = lrn("regr.ranger", max.depth = 7, mtry = 3, min.node.size = 3)
  randomForest_class = lrn("classif.ranger", max.depth = 5, mtry = 4, min.node.size = 7)

  # Initialize learners
  lasso = lrn("regr.cv_glmnet", nfolds = 5, s = "lambda.min")
  lasso_class = lrn("classif.cv_glmnet", nfolds = 5, s = "lambda.min")

  # Initialize DoubleMLPLR model
  dml_plr_lasso = DoubleMLPLR$new(data_dml_flex, ml_l = lasso, ml_m = lasso_class, n_folds = 3)
  dml_plr_lasso$fit()
  dml_plr_lasso$summary()
  # Random Forest
  randomForest = lrn("regr.ranger", max.depth = 7, mtry = 3, min.node.size = 3)
  randomForest_class = lrn("classif.ranger", max.depth = 5, mtry = 4, min.node.size = 7)

  dml_plr_forest = DoubleMLPLR$new(data_dml_base, ml_l = randomForest, ml_m = randomForest_class, n_folds = 3)
  dml_plr_forest$fit()
  dml_plr_forest$summary()

  # Trees
  trees = lrn("regr.rpart", cp = 0.0047, minsplit = 203)
  trees_class = lrn("classif.rpart", cp = 0.0042, minsplit = 104)

  dml_plr_tree = DoubleMLPLR$new(data_dml_base, ml_l = trees, ml_m = trees_class, n_folds = 3)
  dml_plr_tree$fit()
  dml_plr_tree$summary()

  # Boosted trees
  boost = lrn("regr.xgboost", objective = "reg:squarederror", eta = 0.1, nrounds = 35)
  boost_class = lrn("classif.xgboost", objective = "binary:logistic", eval_metric = "logloss", eta = 0.1, nrounds = 34)

  dml_plr_boost = DoubleMLPLR$new(data_dml_base, ml_l = boost, ml_m = boost_class, n_folds = 3)
  dml_plr_boost$fit()
  dml_plr_boost$summary()

  dml_plr_forest = DoubleMLPLR$new(data_dml_base, ml_l = randomForest, ml_m = randomForest_class, n_folds = 3)
  dml_plr_forest$fit()
  dml_plr_forest$summary()

  confints = rbind(dml_plr_lasso$confint(), dml_plr_forest$confint(), dml_plr_tree$confint(), dml_plr_boost$confint())
  estimates = c(dml_plr_lasso$coef, dml_plr_forest$coef, dml_plr_tree$coef, dml_plr_boost$coef)
  estimates.se = c(dml_plr_lasso$se, dml_plr_forest$se, dml_plr_tree$se, dml_plr_boost$se)

  result_plr = data.table("model" = "PLR", "ML" = c("glmnet", "ranger", "rpart", "xgboost"),
                          "Estimate" = estimates, "SE" = estimates.se, "lower" = confints[,1], "upper" = confints[,2])
  result_plr
}


get_imputations <- function(data, methods, data_amputed, scale = F, seed, N) {
  set.seed(seed)
  reticulate::source_python("python/miwae_gain.py")

  imputations_scaled <- lapply(methods, function(imp) {
    print(imp)
    imp_fun <- get(paste0("impute_", imp))
    reticulate::source_python("python/miwae_gain.py")
    imputed <- imp_fun(as.data.frame(data_amputed))

    score <- try({ miceDRF::Iscore(data_amputed, imputed, N = 20, multiple = TRUE,
                                   imputation_func = imp_fun, skip_if_needed = TRUE,
                                   scale = scale)})

    if(inherits(score, "try-error")) score <- NA

    X_means <- colMeans(data, na.rm = TRUE)
    X_sds <- apply(data, 2, sd, na.rm = TRUE)

    # Scale X
    data_scaled <- scale(data, center = X_means, scale = X_sds)

    # Scale X_imp using the same parameters
    data_imp_scaled <- scale(imputed, center = X_means, scale = X_sds)

    list(imputed = imputed,
         score = score,
         energy =  miceDRF::energy_dist(data, imputed),
         energy.scaled = miceDRF::energy_dist(data_scaled, data_imp_scaled))
  })

  imputations_scaled <- c(list(list(imputed = data, score = 0, energy =  0,energy.scaled=0)), imputations_scaled)
  names(imputations_scaled) <- c("true", methods)
  imputations_scaled
}

get_full_results <- function(methods, imputations_scaled) {
  lapply(methods, function(ith) {
    cbind(get_res(imputations_scaled[[ith]][["imputed"]]),
          method = ith,
          IScore = imputations_scaled[[ith]][["score"]],
          energy = imputations_scaled[[ith]][["energy"]],
          energy.scaled = imputations_scaled[[ith]][["energy.scaled"]])
  }) %>%  bind_rows
}


run_application <- function(methods, data, data_amputed, reps = 10) {
  lapply(1:reps, function(i) {
    print(i)
    imputations_scaled <- get_imputations(data, methods, data_amputed, seed = i)
    cbind(get_full_results(c("true", methods), imputations_scaled), rep = i)

  }) %>% bind_rows()
}


custom_ampute <- function(data) {
  data_amputed <- produce_NA(data = as.data.frame(data),
                             mechanism = "MAR",
                             perc.missing = 0.1)$data.incomp
  random_complete_cols <- sample(x = 1:ncol(as.data.frame(data)),
                                 size = 4,
                                 replace = FALSE)
  data_amputed[, c(random_complete_cols)] <-
    as.data.frame(data)[, c(random_complete_cols)]
  data_amputed
}
