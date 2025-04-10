
library(MASS)
library(miceDRF)
library(dplyr)
library(ggplot2)

targets::tar_source()

impute_runif <- function(X) {
  X[is.na(X)] <- runif(sum(is.na(X)))
  X
}

set.seed(123)
dat <- get_dat_ex1()
X <- dat$X_miss

methods <- c("sample","DRF", "cart", "rf", "norm.nob", "norm.predict")

imp_list <- c(create_mice_imputations(methods), list(runif = impute_runif))

N_vec <- c(1, 5, seq(10, 100, 10))

########################################################## gaussian

res <- lapply(N_vec, function(N) {
  print(N)
  c(Iscores_compare(X = X, imputation_list = imp_list, N = N), N = N)
}) %>%  bind_rows()

# saveRDS(res, "resN.RDS")

res %>%
  tidyr::gather(method, score, -N) %>%
  filter(method != "runif") %>%
  ggplot() +
  geom_path(aes(x = N, y = score, col = method)) +
  scale_x_continuous(breaks = N_vec)


############################################################ runif


set.seed(123)
dat <- get_dat_ex4()
X <- dat$X_miss

methods <- c("sample","DRF", "cart", "rf", "norm.nob", "norm.predict")

imp_list <- c(create_mice_imputations(methods), list(runif = impute_runif))

N_vec <- c(1, 5, seq(10, 100, 10))


res <- lapply(1:10, function(ith_rep) {
  lapply(N_vec, function(N) {
    print(N)
    c(Iscores_compare(X = X, imputation_list = imp_list, N = N), N = N, rep = ith_rep)
  }) %>%  bind_rows()
})

saveRDS(res, "./results/N/resN_unif.RDS")

res  %>%
  bind_rows() %>% {
    colnames(.) <- c("mice norm.predict", "mice rf", "random", "mice DRF", "mice norm.nob", "runif", "mice CART", "N", "rep")
    .
  } %>%
  bind_rows() %>%
  tidyr::gather(method, score, -N, -rep) %>%
  filter(N > 1) %>%
  group_by(N, method) %>%
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
  ylab("m-I-Score")


res %>%
  bind_rows() %>% {
    colnames(.) <- c("mice norm.predict", "mice rf", "random", "mice DRF", "mice norm.nob", "runif", "mice CART", "N", "rep")
    .
  } %>%
  tidyr::gather(method, score, -N, -rep) %>%
  filter(N > 1) %>%
  group_by(N, rep) %>%
  mutate(rank = rank(score),
         line_id = paste0(method, rep)) %>%
  group_by(method, N) %>%
  mutate(mean_rank = mean(rank)) %>%
  ggplot() +
  geom_line(aes(x = N, y = mean_rank, col = method)) +
  # geom_line(aes(x = N, y = rank, col = method), alpha = 0.2) +
  scale_x_continuous(breaks = N_vec) +
  theme_light() +
  ylab("averaged rank based on m-I-Score")





