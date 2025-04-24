library(targets)
tar_source()

minmax <- function(x) (x - max(x, na.rm = T))/abs(min(x, na.rm = T) - max(x, na.rm = T))

scores <- c("m-I-Score", "DR-I-Score", "energy", "I-Score-beta")


####### example 1

tar_load(experiment_1)
tar_load(data_ex1)

X <- data_ex1$X
X_miss <- data_ex1$X_miss

colnames(X) <- paste0("X", 1:ncol(X))
colnames(X_miss) <- paste0("X", 1:ncol(X_miss))

p1 <- draw_boxplots(experiment_1, color_plt = "lightsteelblue4")
p2 <- draw_pattern(X, X_miss, obs_col = "lightsteelblue4")

wrap_experiment(p2, p1)


####### example 2

tar_load(experiment_2)
tar_load(data_ex2)

X <- data_ex2$X
X_miss <- data_ex2$X_miss

colnames(X) <- paste0("X", 1:ncol(X))
colnames(X_miss) <- paste0("X", 1:ncol(X_miss))

p1 <- draw_boxplots(experiment_2, color_plt = "lightsteelblue4")
p2 <- draw_pattern(X, X_miss, obs_col = "lightsteelblue4")

wrap_experiment(p2, p1)


#### uniform example

tar_load(experiment_4)
tar_load(data_ex4)

X <- data_ex4$X
X_miss <- data_ex4$X_miss

colnames(X) <- paste0("X", 1:ncol(X))
colnames(X_miss) <- paste0("X", 1:ncol(X_miss))

p1 <- draw_boxplots(experiment_4, color_plt = "lightsteelblue4")
p2 <- draw_uniform(X, X_miss)

wrap_experiment(p2, p1)


#### ex 5

tar_load(experiment_5)
tar_load(data_ex5)

X <- data_ex5$X
X_miss <- data_ex5$X_miss

colnames(X) <- paste0("X", 1:ncol(X))
colnames(X_miss) <- paste0("X", 1:ncol(X_miss))

plot(X)

draw_boxplots(experiment_5, color_plt = "lightsteelblue4")

norm.nob <- X_miss %>%
  impute_norm.nob() %>%
  mutate(missing = is.na(X_miss$X1) | is.na(X_miss$X2),
         method = "mice norm.nob")

gauss <- X_miss %>%
  impute_gaussian_indep() %>%
  mutate(missing = is.na(X_miss$X1) | is.na(X_miss$X2),
         method = "gaussian independent")

rbind(norm.nob, gauss) %>%
  ggplot() +
  geom_point(aes(x = X1, y = X2, col = missing)) +
  facet_grid(~method) +
  theme_light()



