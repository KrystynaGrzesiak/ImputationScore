# Double Machine Learning (DML) Example in R
# Estimating treatment effect in a partially linear model

# Install and load required packages
# install.packages(c("DoubleML", "ranger", "glmnet", "data.table", "mlr3", "mlr3learners"))
library(DoubleML)
library(ranger)
library(glmnet)
library(data.table)
library(mlr3)        # Add this
library(mlr3learners) # Add this

library(missForest)
library(mice)
library(miceDRF)

targets::tar_source()

# Generate synthetic data for demonstration
set.seed(123)
n_obs <- 2000
n_vars <- 10

# Create confounders
X <- matrix(rnorm(n_obs * n_vars), nrow = n_obs, ncol = n_vars)
colnames(X) <- paste0("X", 1:n_vars)

# Treatment variable (correlated with confounders)
D <- 0.5 * X[, 1] + 0.3 * X[, 2] + rnorm(n_obs)

# Outcome with true treatment effect = 2
true_theta <- 2
Y <- true_theta * D + 0.8 * X[, 1] + 0.5 * X[, 2] +
  0.3 * X[, 3] + 0.2 * X[, 4] + rnorm(n_obs)

# Convert to data.table
dml_data <- data.table(Y = Y, D = D, X)
print(head(dml_data))

data_orig <- copy(dml_data)

# amputation
data_amputed <- produce_NA(data = as.data.frame(data_orig), mechanism = "MAR", perc.missing = 0.1)$data.incomp
random_complete_cols <- sample(x = 1:ncol(as.data.frame(data_orig)), size = 5, replace = FALSE)
data_amputed[, c(random_complete_cols)] <- as.data.frame(data_orig)[, c(random_complete_cols)]

methods <- c("missForest", "sample", "DRF", "cart", "norm.predict", "norm.nob")

imputations <- lapply(methods, function(imp) {
  print(imp)
  imp_fun <- get(paste0("impute_", imp))
  imputed <- imp_fun(data_amputed)

  score <- try({ miceDRF::Iscore(data_amputed, imputed, N = 20, multiple = TRUE,
                                 imputation_func = imp_fun, skip_if_needed = TRUE)})
  if(inherits(score, "try-error")) score <- NA

  list(imputed = imputed, score = score)
})

names(imputations) <- methods


# Method 1: Using DoubleML package with different learners
# --------------------------------------------------------

# Initialize DoubleML data object

res1 <- lapply(methods, function(ith) {
  dml_data <- imputations[[ith]][["imputed"]]

  dml_data_obj <- DoubleMLData$new(dml_data,
                                   y_col = "Y",
                                   d_cols = "D",
                                   x_cols = paste0("X", 1:n_vars))

  ml_g_rf <- lrn("regr.ranger", num.trees = 100, mtry = 3, min.node.size = 5)
  ml_m_rf <- lrn("regr.ranger", num.trees = 100, mtry = 3, min.node.size = 5)

  # Initialize DML model for partially linear regression
  dml_plr_rf <- DoubleMLPLR$new(
    dml_data_obj,
    ml_l = ml_g_rf,  # ml_g renamed to ml_l
    ml_m = ml_m_rf,
    n_folds = 5,        # Number of cross-validation folds
    n_rep = 1,          # Number of repetitions
    score = "partialling out",  # Fixed: space instead of underscore
    dml_procedure = "dml2"
  )

  dml_plr_rf$fit()

  estimates <- data.frame(
    Method = c("True Value", paste0("DML (", ith,")")),
    Estimate = c(true_theta, dml_plr_rf$coef),
    Lower = c(NA, dml_plr_rf$confint()[1]),
    Upper = c(NA, dml_plr_rf$confint()[2]), row.names = NULL
  )
}) %>%  bind_rows()


res_plt <- cbind(unique(res1), score = c(0, sapply(methods, function(i) { imputations[[i]][["score"]]})))


res_plt %>%
  ggplot(aes(x = reorder(Method, score), y = Estimate)) +
  geom_point() +
  geom_text(aes(x = reorder(Method, score), y = Estimate - 0.01, label = paste0("estimate = ", round(Estimate, 4))), col = "blue") +
  geom_text(aes(x = reorder(Method, score), y = Estimate + 0.01, label = paste0("score = ", round(score, 2))), col = "purple") +
  geom_hline(yintercept = 2) +
  geom_errorbar(aes(x = reorder(Method, score), ymin = Lower, ymax = Upper),
                width = 0.2)
