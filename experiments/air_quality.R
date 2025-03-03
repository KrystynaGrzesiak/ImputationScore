
library(mice)

get_data <- function() {
  load("./air_data_benchmark2.Rdata")

  dat <- cbind(Y, X[, -c(4, 8)])
  patterns <- (! cbind(diag(1, 4), 0, 0, 0, 0, 0, 0, 0)) * 1

  X <- dat[sample(1:nrow(dat), 2000, replace = FALSE), ]
  X_miss <- mice::ampute(X, patterns = patterns)$amp

  list(X_miss = X_miss, X = X)
}



