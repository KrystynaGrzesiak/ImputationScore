
get_methods <- function(methods) {
  lapply(methods, function(ith_method) get(paste0("impute_", ith_method)))
}
