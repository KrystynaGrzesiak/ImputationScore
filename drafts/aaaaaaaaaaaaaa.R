


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


plts <- lapply(unique(experiment_4$score), function(ith_score) {
  experiment_4 %>%
    filter(score == ith_score) %>%
    mutate(value = ifelse(score == "DR-I-Score", value, -value)) %>%
    group_by(score, method) %>%
    mutate(mean_score = mean(value)) %>%
    ungroup() %>%
    ggplot() +
    geom_boxplot(aes(x = reorder(method, mean_score), y = value)) +
    facet_wrap(~score, scales = "free") +
    xlab("method")
})

(patchwork::wrap_plots(plts, 3) ) + plot_layout(guides = 'collect')




