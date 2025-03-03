

source("./experiments/plotting_funs.R")

# p3 <- draw_pattern(readRDS("./experiments/data/X_exp3.RDS"),
#                    readRDS("./experiments/data/X_miss_exp3.RDS"),
#                    experiment = 3)



############ example 1

p1 <- draw_pattern(readRDS("./experiments/data/X_exp1.RDS"),
                   readRDS("./experiments/data/X_miss_exp1.RDS"),
                   experiment = 1)

ex1 <- readRDS("./experiments/ex1.RDS") %>%
  mutate(iscore = -iscore, energy = -energy, NRMSE = -NRMSE) %>%
  rename(`I-Score` = "iscore")

p2 <- draw_scores(ex1)

wrap_experiment(p1, p2)

############ example 2

p1 <- draw_pattern(readRDS("./experiments/data/X_exp2.RDS"),
                   readRDS("./experiments/data/X_miss_exp2.RDS"))

ex2 <- readRDS("./experiments/ex2.RDS") %>%
  mutate(iscore = -iscore, energy = -energy, NRMSE = -NRMSE) %>%
  rename(`I-Score` = "iscore")

p2 <- draw_scores(ex2)

wrap_experiment(p1, p2)


##### air quality

draw_scores(res_airquality)


##### example 4

X_miss <- data.frame(readRDS("./experiments/data/X_miss_exp4.RDS"))
X <- data.frame(readRDS("./experiments/data/X_exp4.RDS"))

p1 <- draw_unifiorm(X, X_miss)


ex4 <- readRDS("./experiments/res_ex4.RDS") %>%
  mutate(iscore = -iscore, energy = -energy, NRMSE = -NRMSE) %>%
  rename(`I-Score` = "iscore")

p2 <- draw_scores(ex4)


wrap_experiment(p1, p2)
