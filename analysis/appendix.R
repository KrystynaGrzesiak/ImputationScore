tar_load(experiment_4)
tar_load(data_ex4)

X <- data_ex4$X
X_miss <- data_ex4$X_miss

colnames(X) <- paste0("X", 1:ncol(X))
colnames(X_miss) <- paste0("X", 1:ncol(X_miss))

methods_ex4 <- c("norm.nob", "DRF", "cart", "norm.predict", "runif", "runifsq", "missForest")

imp <- lapply(methods_ex4, function(ith_imp) {
  get(paste0("impute_", ith_imp))(X_miss)
})


plts <- lapply(1:length(imp), function(ith_imp) {
  X1_imp <- imp[[ith_imp]][, 1]
  X1_miss <- is.na(X_miss)[, 1]

  data.frame(X1 = X1_imp,
             missing = X1_miss) %>%
    ggplot() +
    geom_histogram(aes(x = X1, fill = missing, alpha = missing, after_stat(density)),
                   col = "black",
                   position ="identity",
                   bins = 10,
                   binwidth = 0.1,
                   boundary = 0) +
    ggtitle(methods_ex4[ith_imp]) +
    scale_alpha_manual(values = c(1, 0.7))
})

(patchwork::wrap_plots(plts , nrow = 2)) + plot_layout(guides = 'collect')


