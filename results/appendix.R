tar_load(experiment_1)
tar_load(data_ex1)

X <- data_ex1$X
X_miss <- data_ex1$X_miss

colnames(X) <- paste0("X", 1:ncol(X))
colnames(X_miss) <- paste0("X", 1:ncol(X_miss))

methods_ex1 <- c("norm.nob", "norm.predict", "missForest", "cart", "DRF")

imp <- lapply(methods_ex1, function(ith_imp) {
  get(paste0("impute_", ith_imp))(X_miss)
})

get_plots <- function(imp, i) {
  lapply(1:length(imp), function(ith_imp) {
    X1_imp <- imp[[ith_imp]][, i]
    X1_miss <- is.na(X_miss)[, i]

    p <- data.frame(X1 = X1_imp,
               missing = X1_miss) %>%
      ggplot() +
      geom_histogram(aes(x = X1, fill = missing, alpha = missing, after_stat(density)),
                     col = "black",
                     position ="identity",
                     bins = 20,
                     boundary = 0) +
      scale_alpha_manual(values = c(1, 0.7)) +
      coord_cartesian(ylim = c(0, 0.2)) +
      theme_light() +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            legend.position = "none")

    if(i == 1)
      p <- p + ggtitle(methods_ex1[ith_imp])

    if(ith_imp == 1)
      p <- p + theme(axis.title.y = element_text(angle = 90, vjust = 4),
                     axis.text.y = element_text()) + ylab(paste0("variable X", i))

    if(i == 2)
      p <- p + theme(legend.position = "right")
    p
  })
}

plts <- get_plots(imp, 1)
p1 <- (patchwork::wrap_plots(plts , nrow = 1)) + plot_layout(guides = 'collect')

plts <- get_plots(imp, 2)
p2 <- (patchwork::wrap_plots(plts , nrow = 1)) + plot_layout(guides = 'collect')

plts <- get_plots(imp, 3)
p3 <- (patchwork::wrap_plots(plts , nrow = 1)) + plot_layout(guides = 'collect')


(p1/p2/p3) + plot_layout(guides = 'collect')


