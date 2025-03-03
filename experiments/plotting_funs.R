
library(dplyr)
library(ggplot2)
library(patchwork)

draw_single_pattern <- function(X, X_miss, ith) {
  X %>%
    mutate(is_missing = is.na(X_miss[, ith])) %>%
    ggplot() +
    geom_histogram(aes(x = get(paste0("X", ith)), fill = is_missing), col = "black", position="identity") +
    xlab(bquote(X[.(ith)])) +
    theme_light() +
    scale_fill_manual("missing", values = c("white", "grey40")) +
    # scale_color_manual("missing", values = c("#48333F", "#CC7400")) +
    theme(axis.title.y = element_blank())
}


draw_pattern <- function(X, X_miss, vars = 1:3, experiment = 1) {
  plots_list <- lapply(vars, function(ith_var) draw_single_pattern(X, X_miss, ith_var))

  patchwork::wrap_plots(plots_list, 3) + plot_layout(guides = 'collect') + plot_annotation(
    title = paste0("Experiment ", experiment),
  )
}

draw_scores <- function(res) {

  plts <- lapply(colnames(res)[1:3], function(ith) {
    res %>%
      group_by(method) %>%
      mutate(mean_value = mean(get(ith))) %>%
      ggplot() +
      geom_boxplot(aes(x = reorder(method, mean_value), y = get(ith))) +
      theme_light() +
      theme(axis.text.x = element_text(angle = 20, size = 12, vjust = 0.8),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major.y = element_blank()) +
      ggtitle(ith)
  })

 (patchwork::wrap_plots(plts, 3) ) + plot_layout(guides = 'collect')

}


wrap_experiment <- function(p1, p2) {
  thm <- theme(plot.title = element_text(face = 2, size = 16))

  top_plot <- wrap_elements(p1 + plot_annotation(title = "A", theme = thm))
  bottom_plot <- wrap_elements(p2 + plot_annotation(title = "B", theme = thm))

  (top_plot / bottom_plot) + plot_layout(heights = unit(c(1, 1.5), c('null')))
}



draw_unifiorm <- function(X, X_miss) {

  plt_1 <- X %>%
    mutate(is_missing = is.na(X_miss[, 1])) %>%
    filter(is_missing) %>%
    ggplot() +
    geom_histogram(aes(x = X1, after_stat(density)),
                   col = "black",
                   fill = "grey40",
                   position="identity",
                   bins = 10,
                   binwidth = 0.1,
                   boundary = min(X$X1)) +
    theme_light() +
    xlab(expression(X[1] ~ "|" ~ M == m[3])) +
    theme(axis.title.x = element_text(size = 16))

  plt_2 <- X %>%
    mutate(m3 = !(is.na(X_miss[, 1]) | is.na(X_miss[, 2]))) %>%
    filter(m3) %>%
    ggplot() +
    geom_histogram(aes(x = X1, after_stat(density)),
                   col = "black",
                   fill = "grey40",
                   position="identity",
                   bins = 10,
                   binwidth = 0.1,
                   boundary = min(X$X1)) +
    theme_light() +
    xlab(expression(X[1] ~ "|" ~ M == m[1])) +
    theme(axis.title.x = element_text(size = 16),
          axis.title.y = element_blank())

  plt_3 <- X %>%
    mutate(is_missing = is.na(X_miss[, 1])) %>%
    filter(!is_missing) %>%
    ggplot() +
    geom_histogram(aes(x = X1, after_stat(density)),
                   col = "black",
                   fill = "grey40",
                   position="identity",
                   bins = 10,
                   binwidth = 0.1,
                   boundary = min(X$X1)) +
    theme_light() +
    xlab(expression(X[1] ~ "|" ~ M %in% group("{", m[1] * "," ~ m[2], "}"))) +
    theme(axis.title.x = element_text(size = 16),
          axis.title.y = element_blank())

  plt_1 + plt_2 + plt_3


}




