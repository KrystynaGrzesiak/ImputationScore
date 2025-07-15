
draw_boxplots <- function(dat, color_plt, scores) {
  # scores <- c(setdiff(unique(dat[["score"]]), c("NRMSE", "energy")), "energy")

  plts <- lapply(scores, function(ith_score) {
    dat_plt <- dat %>%
      filter(method != "DRF") %>%
      transform_data(ith_score = ith_score)

    dat_plt %>%
      ggplot() +
      geom_boxplot(aes(x = reorder(method, mean_score), y = value), fill = color_plt) +
      # facet_wrap(~score, scales = "free") +
      ggtitle(unique(dat_plt$score)) +
      theme_light(base_size  = 14) +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2))
  })

  (patchwork::wrap_plots(plts , nrow = 1)) + plot_layout(guides = 'collect')
}

draw_single_pattern <- function(X, X_miss, ith, obs_col) {
  X %>%
    mutate(is_missing = is.na(X_miss[, ith])) %>%
    ggplot() +
    geom_histogram(aes(x = get(paste0("X", ith)), fill = is_missing), col = "black", position="identity") +
    xlab(bquote(X[.(ith)])) +
    scale_fill_manual("missing", values = c(obs_col, "white")) +
    # scale_color_manual("missing", values = c("#48333F", "#CC7400")) +
    theme_light(base_size  = 12) +
    theme(axis.title.y = element_blank())
}


draw_pattern <- function(X, X_miss, vars = 1:3, obs_col) {
  plots_list <- lapply(vars, function(ith_var) draw_single_pattern(X, X_miss, ith_var, obs_col))

  patchwork::wrap_plots(plots_list, 3) + plot_layout(guides = 'collect')
}


wrap_experiment <- function(p1, p2, p1_w = 1, p2_w = 1.5) {
  thm <- theme(plot.title = element_text(face = 2, size = 16))

  top_plot <- wrap_elements(p1 + plot_annotation(title = "A", theme = thm))
  bottom_plot <- wrap_elements(p2 + plot_annotation(title = "B", theme = thm))

  (top_plot / bottom_plot) + plot_layout(heights = unit(c(p1_w, p2_w), c('null')))
}

draw_uniform <- function(X, X_miss) {

  plt_1 <- X %>%
    mutate(is_missing = is.na(X_miss[, 1])) %>%
    filter(is_missing) %>%
    ggplot() +
    geom_histogram(aes(x = X1, after_stat(density)),
                   col = "black",
                   fill = "lightsteelblue4",
                   position="identity",
                   bins = 10,
                   binwidth = 0.1,
                   boundary = min(X$X1)) +
    xlab(expression(X[1] ~ "|" ~ M == m[3])) +
    theme_light(base_size  = 12) +
    theme(axis.title.x = element_text(size = 14))

  plt_2 <- X %>%
    mutate(m3 = !(is.na(X_miss[, 1]) | is.na(X_miss[, 2]))) %>%
    filter(m3) %>%
    ggplot() +
    geom_histogram(aes(x = X1, after_stat(density)),
                   col = "black",
                   fill = "lightsteelblue4",
                   position="identity",
                   bins = 10,
                   binwidth = 0.1,
                   boundary = min(X$X1)) +
    xlab(expression(X[1] ~ "|" ~ M == m[1])) +
    theme_light(base_size  = 12) +
    theme(axis.title.x = element_text(size = 14),
          axis.title.y = element_blank())

  plt_3 <- X %>%
    mutate(is_missing = is.na(X_miss[, 1])) %>%
    filter(!is_missing) %>%
    ggplot() +
    geom_histogram(aes(x = X1, after_stat(density)),
                   col = "black",
                   fill = "lightsteelblue4",
                   position="identity",
                   bins = 10,
                   binwidth = 0.1,
                   boundary = min(X$X1)) +
    xlab(expression(X[1] ~ "|" ~ M %in% group("{", m[1] * "," ~ m[2], "}"))) +
    theme_light(base_size  = 12) +
    theme(axis.title.x = element_text(size = 14),
          axis.title.y = element_blank())

  plt_1 + plt_2 + plt_3
}


transform_data <- function(dat, ith_score) {
  dat %>%
    filter(score == ith_score) %>%
    mutate(score = ifelse(score == "I-Score", "energy-I-Score", score)) %>%
    mutate(score = ifelse(score == "energy", "Full information score", score)) %>%
    mutate(method = ifelse(method == "DRF", "mice-DRF", method)) %>%
    mutate(value = ifelse(score == "DR-I-Score", value, -value)) %>%
    group_by(score, method) %>%
    mutate(mean_score = mean(value, na.rm = TRUE)) %>%
    group_by(score) %>%
    mutate(value = minmax(value)) %>%
    ungroup()
}

