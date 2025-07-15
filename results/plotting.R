library(targets)
tar_source()

minmax <- function(x) (x - max(x, na.rm = T))/abs(min(x, na.rm = T) - max(x, na.rm = T))

scores <- c("I-Score", "I-Score-beta", "DR-I-Score", "energy")

scores <- c("I-Score", "DR-I-Score", "energy")


####### example 1

tar_load(experiment_1)
tar_load(data_ex1)

X <- data_ex1$X
X_miss <- data_ex1$X_miss

colnames(X) <- paste0("X", 1:ncol(X))
colnames(X_miss) <- paste0("X", 1:ncol(X_miss))

p1 <- draw_boxplots(experiment_1, color_plt = "lightsteelblue4", scores)
p2 <- draw_pattern(X, X_miss, obs_col = "lightsteelblue4")

wrap_experiment(p2, p1)


####### example 2

tar_load(experiment_2)
tar_load(data_ex2)

X <- data_ex2$X
X_miss <- data_ex2$X_miss

colnames(X) <- paste0("X", 1:ncol(X))
colnames(X_miss) <- paste0("X", 1:ncol(X_miss))

p1 <- draw_boxplots(experiment_2, color_plt = "lightsteelblue4", scores)
p2 <- draw_pattern(X, X_miss, obs_col = "lightsteelblue4")

wrap_experiment(p2, p1)


#### uniform example

tar_load(experiment_4)
tar_load(data_ex4)

X <- data_ex4$X
X_miss <- data_ex4$X_miss

colnames(X) <- paste0("X", 1:ncol(X))
colnames(X_miss) <- paste0("X", 1:ncol(X_miss))

p1 <- draw_boxplots(experiment_4, color_plt = "lightsteelblue4", scores)
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

experiment_5 %>%
  mutate(method = ifelse(method == "gaussian_indep", "rnorm", "mice norm.nob")) %>%
  draw_boxplots(color_plt = "lightsteelblue4", scores)

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
  geom_point(aes(x = X1, y = X2, col = missing), size = 1) +
  facet_grid(~method) +
  theme_light() +
  scale_color_manual(values = c("slateblue4", "orange"))

#### uniform example ex 6

tar_load(experiment_6)
tar_load(data_ex6)

X <- data_ex6$X
X_miss <- data_ex6$X_miss

colnames(X) <- paste0("X", 1:ncol(X))
colnames(X_miss) <- paste0("X", 1:ncol(X_miss))

p1 <- draw_boxplots(experiment_6, color_plt = "lightsteelblue4", scores)
p2 <- draw_uniform(X, X_miss)

wrap_experiment(p2, p1)



#### beta vol 2

scores <- c("energy-I-Score*", "energy")

tar_load(experiment_4_beta)
ex4 <- draw_boxplots(
  mutate(experiment_4_beta, score = ifelse(score == "I-Score-beta-v2", "energy-I-Score*", score)),
  color_plt = "lightsteelblue4", scores)

tar_load(experiment_5_beta)
ex5 <- draw_boxplots(
  mutate(experiment_5_beta, score = ifelse(score == "I-Score-beta-v2", "energy-I-Score*", score)),
  color_plt = "lightsteelblue4", scores)

tar_load(experiment_6_beta)
ex6 <- draw_boxplots(
  mutate(experiment_6_beta, score = ifelse(score == "I-Score-beta-v2", "energy-I-Score*", score)),
  color_plt = "lightsteelblue4", scores)

tar_load(experiment_1_beta)
ex1 <- draw_boxplots(
  mutate(experiment_1_beta, score = ifelse(score == "I-Score-beta-v2", "energy-I-Score*", score)),
  color_plt = "lightsteelblue4", scores)

tar_load(experiment_2_beta)
ex2 <- draw_boxplots(
  mutate(experiment_2_beta, score = ifelse(score == "I-Score-beta-v2", "energy-I-Score*", score)),
  color_plt = "lightsteelblue4", scores)


(wrap_elements(ex1) / wrap_elements(ex2) / wrap_elements(ex4) / wrap_elements(ex5) / wrap_elements(ex6)) +
  plot_annotation(tag_levels = c(1))


title_plot <- function(title) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = title, hjust = 0, vjust = 0,
             size = 4, fontface = "plain") +
    theme_void() +
    theme(plot.margin = margin(0, 0, 2, 0))  # trochę miejsca na dole
}


final_plot <- (
  title_plot("Gaussian Mixture Example") / wrap_elements(ex1) /
    title_plot("Nonlinear Mixture Example") / wrap_elements(ex2) /
    title_plot("Independent Uniform Example") / wrap_elements(ex4) /
    title_plot("Non-Strict Propriety Example") / wrap_elements(ex5) /
    title_plot("Dependent Uniform Example") / wrap_elements(ex6)
) + plot_layout(heights = c(
  0.08, 1, 0.08, 1, 0.08, 1, 0.08, 1, 0.08, 1
))

final_plot


#########################       DML     ########################################


# results_full <- readRDS("results/results_full.RDS")

tar_load(results_full)


true_dat <- results_full %>%
  filter(method == "true")

pp1 <- results_full %>%
  ggplot() +
  geom_hline(true_dat, mapping = aes(yintercept = Estimate, col = "black"), linewidth = 2.4, alpha = 0.3) +
  scale_colour_manual(name = element_blank(), values = c("black" = "limegreen"),
                      labels = c("black" = "full information result")) +
  geom_boxplot(aes(x = reorder(method, IScore), y = Estimate), fill = "lightsteelblue4") +
  facet_wrap(~ML, scales = "free", nrow = 1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_light(base_size = 14) +
  ylab(expression(hat(theta)[0])) +
  theme(axis.title.x = element_blank())


pp2 <- results_full %>%
  ggplot() +
  geom_hline(true_dat, mapping = aes(yintercept = SE, col = "black"), linewidth = 2.4, alpha = 0.3) +
  scale_colour_manual(name = element_blank(), values = c("black" = "limegreen"),
                      labels = c("black" = "full information result")) +
  geom_boxplot(aes(x = reorder(method, IScore), y = SE), fill = "lightsteelblue4") +
  facet_wrap(~ML, scales = "free", nrow = 1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_light(base_size = 14) +
  ylab(expression(SE(hat(theta)[0]))) +
  theme(axis.title.x = element_blank())


p1 <- results_full %>%
  ggplot(aes(x = reorder(method, IScore), y = IScore)) +
  geom_boxplot(fill = "lightsteelblue4") +
  theme_light() +
  theme(axis.title.x = element_blank()) +
  ylab("energy-I-Score")

p2 <- results_full %>%
  ggplot(aes(x = reorder(method, energy.scaled), y = energy.scaled)) +
  geom_boxplot( fill = "lightsteelblue4") +
  ylab("full information score") +
  theme_light() +
  theme(axis.title.x = element_blank())


library(patchwork)

plotlow <- pp1 / pp2 + plot_layout(guides = 'collect') & theme(legend.position = "none")

plotupp <- p1 + p2

wrap_experiment(plotupp, plotlow, p1_w = 1.2, p2_w = 3)

###########

library(gt)

tbl_plt <- results_full %>%
  filter(method != "true") %>%
  group_by(method) %>%
  summarise("energy-I-Score" = mean(IScore)) %>%
  arrange(`energy-I-Score`) %>%
  t() %>%
  as.data.frame()

colnames(tbl_plt) <- c("mice-rf", "mice-CART", "sample", "GAIN", "kNN", "missForest")

tbl_clean <- tbl_plt
tbl_clean <- tbl_clean[-1, ]

tbl_grob <- tableGrob(tbl_clean, rows = NULL, theme = ttheme_default())

tbl_grob$heights <- unit(rep(1, nrow(tbl_grob)), "null")
tbl_grob$widths  <- unit(rep(1, ncol(tbl_grob)), "null")

table_plot <- as.ggplot(function() grid.draw(tbl_grob))

wrap_table <- (plot_spacer() | table_plot | plot_spacer()) + plot_layout(widths = c(0.085, 0.915, 0))

thm <- theme(plot.title = element_text(face = 0, size = 16, margin = margin(b = 0)))

wrap_table <- wrap_elements(wrap_table + plot_annotation(title = "A: Averaged energy-I-Score", theme = thm))
plt_low <- wrap_elements(plotlow + plot_annotation(title = "B: Simulation results", theme = thm))


(wrap_table / plt_low) +  plot_layout(heights = unit(c(0.7, 4), c('null')))


