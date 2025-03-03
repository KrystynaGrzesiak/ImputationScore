library(ggplot2)
library(dplyr)


generate_spiral <- function(n_points = 2000, noise = 0.05, turns = 3) {
  theta <- seq(0, turns * 2 * pi, length.out = n_points)

  r <- theta / (turns * 2 * pi)

  x <- r * cos(theta) + rnorm(n_points, 0, noise)
  y <- r * sin(theta) + rnorm(n_points, 0, noise)

  data.frame(X1 = x, X2 = y, X3 = runif(n_points))
}

set.seed(123)

X <- generate_spiral(n_points = 1000, noise = 0.05, 3)

X_miss <- X

X_miss[X[["X1"]] < -0.3 & X[["X1"]] > -0.6 & X[["X2"]] < 0.5 & X[["X2"]] > -0.5, 1] <- NA


saveRDS(X, "./experiments/data/X_exp5.RDS")
saveRDS(X_miss, "./experiments/data/X_miss_exp5.RDS")



X %>%
  mutate(is_na = is.na(X_miss[, 1])) %>%
  ggplot(aes(X1, X2, col = is_na)) +
  geom_point(alpha = 0.6, size = 1) +
  theme_minimal() +
  coord_fixed() +
  ggtitle("Spiral Point Cloud")



# experiment 5 - spiral

set.seed(11)

methods <- c("sample", "cart", "norm.predict", "norm.nob", "DRF")

X <- generate_spiral(n_points = 1000, noise = 0.05, 3)
X_miss <- X
X_miss[X[["X1"]] < -0.3 & X[["X1"]] > -0.6 & X[["X2"]] < 0.5 & X[["X2"]] > -0.5, 1] <- NA


res <- lapply(methods, function(i) {
  print(i)
  imp_fun <- get(paste0("impute_", i))
  X_imp <- imp_fun(X_miss)

  list(X_imp = X_imp, score = get_score_for_method(X, X_miss, X_imp, imp_fun = imp_fun, method = i))
})


scores_tbl <- lapply(res, function(ith_method) ith_method[[2]]) %>%  bind_rows()


plts <- lapply(res, function(ith_method) {

  ith_method[["X_imp"]] %>%
    mutate(is_na = is.na(X_miss[, 1])) %>%
    ggplot(aes(X1, X2, col = is_na)) +
    geom_point() +
    theme_minimal() +
    coord_fixed() +
    ggtitle(paste0("Method: ", ith_method[["score"]][["method"]],
                   ", I-Score: ", round(ith_method[["score"]][["iscore"]], 3),
                   ", Energy: ", round(ith_method[["score"]][["energy"]], 3)))
})


patchwork::wrap_plots(plts)

