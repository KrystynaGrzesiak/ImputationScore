
library(dplyr)
library(ggplot2)

# uniform example

N_vec <- c(1, 5, seq(10, 100, 10))
res <- readRDS("./results/N/resN_unif.RDS")

res  %>%
  bind_rows() %>% {
    colnames(.) <- c("mice norm.predict", "mice rf", "random", "mice DRF", "mice norm.nob", "runif", "mice CART", "N", "rep")
    .
  } %>%
  bind_rows() %>%
  tidyr::gather(method, score, -N, -rep) %>%
  filter(N > 1) %>%
  group_by(N, method) %>%
  mutate(score = -score) %>%
  mutate(mean_score = mean(score),
         upr_score = quantile(score, .75),
         lwr_score = quantile(score, .25)) %>%
  ggplot() +
  geom_point(aes(x = N, y = mean_score, col = method)) +
  # geom_path(aes(x = N, y = mean_score, col = method)) +
  geom_errorbar(
    aes(x = N, ymin = lwr_score,  ymax = upr_score, col = method)
  ) +
  scale_x_continuous(breaks = N_vec) +
  theme_light() +
  ylab("energy-I-Score") +
  ggtitle("Choice of N in the uniform example")


# gaussian example


res <- readRDS("./results/N/resN_gaussian.RDS")

res  %>%
  bind_rows() %>% {
    colnames(.) <- c("mice norm.predict", "mice norm.nob", "mice CART",
                     "mice rf", "runif", "random", "mice DRF", "N", "rep")
    .
  } %>%
  bind_rows() %>%
  tidyr::gather(method, score, -N, -rep) %>%
  filter(N > 1, method != "runif") %>%
  group_by(N, method) %>%
  mutate(score = -(score)) %>%
  mutate(mean_score = mean(score),
         upr_score = quantile(score, .75),
         lwr_score = quantile(score, .25)) %>%
  ggplot() +
  geom_point(aes(x = N, y = mean_score, col = method)) +
  # geom_path(aes(x = N, y = mean_score, col = method)) +
  geom_errorbar(
    aes(x = N, ymin = lwr_score,  ymax = upr_score, col = method),
  ) +
  scale_x_continuous(breaks = N_vec) +
  theme_light() +
  ylab("energy-I-Score") +
  ggtitle("Choice of N in the gaussian mixture model example")


