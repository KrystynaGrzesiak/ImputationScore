
library(caret)
library(ranger)
library(pROC)

library(mlbench)
library(dplyr)
library(e1071)
library(class)

source("./experiments/imputation.R")

data(Sonar)

methods <- c("missForest", "sample", "DRF", "cart")

# methods <- c("sample", "DRF", "cart", "norm.predict")

# methods <- c("norm.predict")

Sonar <- Sonar %>% mutate(Class = ifelse(Class == "R", 1, 0))

set.seed(123)

train_idx <- createDataPartition(Sonar$Class, p = 0.75, list = FALSE)
train_data <- Sonar[train_idx, ]
test_data <- Sonar[-train_idx, ]


res_imp <- lapply(1:10, function(ith_rep) {

  patterns <- rbind(c(sample(c(1, 0), ncol(train_data) - 2, replace = TRUE, prob = c(0.8, 0.2)), 1, 1),
                    c(sample(c(1, 0), ncol(train_data) - 2, replace = TRUE, prob = c(0.92, 0.08)), 1, 1))

  train_data_miss <- mice::ampute(train_data, patterns = patterns)$amp

  lapply(methods, function(i) {

    print(paste0("Imputing ", i))
    imp_fun <- get(paste0("impute_", i))

    imputed_train <- imp_fun(train_data_miss)

    scores <- get_score_for_method(train_data, train_data_miss, imputed_train, imp_fun, method = i)

    print("Random forest classification")
    model <- ranger(Class ~ ., data = imputed_train, probability = TRUE)

    pred_probs <- predict(model, test_data)$predictions[, 1]
    roc_obj <- roc(test_data$Class, pred_probs)
    auc_rf <- as.vector(auc(roc_obj))

    print("Naive Bayes classification")
    nb_model <- naiveBayes(Class ~ ., data = train_data)
    nb_pred <- predict(nb_model, test_data, type = "raw")[, 1]
    nb_auc <- auc(roc(test_data$Class, nb_pred))

    print("knn classification")
    knn_pred <- knn(train_data[, -ncol(train_data)],
                    test_data[, -ncol(test_data)],
                    cl = train_data$Class,
                    k = 5, prob = TRUE)
    knn_probs <- attr(knn_pred, "prob")
    knn_probs <- ifelse(knn_pred == 1, 1 - knn_probs, knn_probs)
    knn_auc <- auc(roc(test_data$Class, knn_probs))

    aucs <- data.frame(classifier = c("rf", "nb", "knn"),
                       auc = c(auc_rf, nb_auc, knn_auc))

    cross_join(scores, aucs)
  }) %>%
    bind_rows()

}) %>%
  bind_rows()


res_imp %>%
  bind_rows() %>%
  ggplot(aes(x = auc, y = iscore, col = method)) +
  geom_point() +
  facet_grid(~classifier, scales = "free_x")





