


minmax <- function(x) (x - max(x))/abs(min(x) - max(x))

scores <- c("I-Score", "DR-I-Score", "energy")


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


