##Changes to the original function:
## 1. Removed tryCatch from doevaluation around densityRatioScoreown
## 2. Fixed the projection.function in sample.vars.projown: Now it will add the missing variables of interest to the projected values from the function



sample.vars.projown<-function (ids.x.na, X, projection.function = NULL, normal.proj = T) 
{
  if (is.null(projection.function)) {
    if (normal.proj == TRUE) {
      if (length(ids.x.na) == 1) {
        num.var.na <- 1
        vars.na <- ids.x.na
      }
      else {
        num.var.na <- sample(1:length(ids.x.na), 1)
        vars.na <- sample(ids.x.na, num.var.na, replace = F)
      }
      vars.na <- sort(vars.na, decreasing = F)
      vars <- vars.na
      if (ncol(X) == 2) {
        dim.proj <- 1
      }
      else {
        dim.proj <- sample(1:(ncol(X) - length(vars)), 
                           size = 1, replace = FALSE)
      }
      avail <- 1:ncol(X)
      avail <- avail[-vars]
      if (length(avail) == 1) {
        if (dim.proj == 1) {
          vars <- c(vars, avail)
        }
      }
      else {
        vars <- c(vars, sample(avail, size = dim.proj, 
                               replace = FALSE))
      }
      vars <- sort(vars, decreasing = F)
    }
    else {
      if (length(ids.x.na) == 1) {
        num.var.na <- 1
        vars.na <- ids.x.na
      }
      else {
        num.var.na <- sample(1:length(ids.x.na), 1)
        vars.na <- sample(ids.x.na, num.var.na, replace = F)
      }
      vars.na <- sort(vars.na, decreasing = F)
      vars <- vars.na
    }
  }
  else {
    vars <- c(ids.x.na, projection.function(X))
    vars <- sort(vars, decreasing = F)
  }
  return(vars)
}









densityRatioScoreown<-function (X, Xhat, x = NULL, num.proj = 10, num.trees.per.proj = 1, 
          projection.function = NULL, min.node.size = 1, normal.proj = T) 
{
  M <- X
  M[!is.na(M)] <- 0
  M[is.na(M)] <- 1
  if (!is.null(x)) {
    ids.x.na <- which(is.na(x))
  }
  else {
    ids.x.na <- 0
  }
  list.rf <- lapply(1:num.proj, FUN = function(i) {
    vars <- sample.vars.projown(ids.x.na = ids.x.na, X = X, 
                             projection.function = projection.function, normal.proj = normal.proj)
    dim.proj <- length(vars)
    X.proj.complete <- stats::na.omit(X[, vars, drop = F])
    X.proj.complete <- matrix(X.proj.complete, ncol = length(vars), 
                              nrow = nrow(X.proj.complete), byrow = F)
    colnames(X.proj.complete) <- NULL
    ids.with.missing <- which(apply(M[, vars, drop = F], 
                                    1, function(x) sum(x) != 0))
    if (nrow(X.proj.complete) <= 2) {
      return(NA)
    }
    if (length(ids.with.missing) == 0) {
      return(NA)
    }
    if (normal.proj == TRUE) {
      patternxA <- matrix(x[vars], ncol = length(vars))
      patternxA[is.na(patternxA)] <- 0
      patternxA <- 1 - patternxA
      M.A <- M[ids.with.missing, vars, drop = F]
      kern <- rbfdot(sigma = 0.25)
      B <- kernelMatrix(kern, x = patternxA, y = M.A)
      drawA <- which(B == 1)
      Y.proj <- Xhat[ids.with.missing, vars, drop = F][drawA,
                                                       , drop = F]
    }
    else {
      Y.proj <- Xhat[ids.with.missing, vars, drop = F]
      drawA <- c()
    }


    
    cl.bl.output <- Iscores:::class.balancing(X.proj.complete = X.proj.complete, 
                                    Y.proj = Y.proj, drawA = drawA, Xhat = Xhat, ids.with.missing = ids.with.missing, 
                                    vars = vars)
    X.proj.complete <- cl.bl.output$X.proj.complete
    Y.proj <- cl.bl.output$Y.proj
    if (nrow(Y.proj) == 0) {
      return(NA)
    }
    colnames(Y.proj) <- NULL
    Y.proj <- as.matrix(Y.proj)
    colnames(X.proj.complete) <- NULL
    d <- data.frame(class = c(rep(1, each = nrow(X.proj.complete)), 
                              rep(0, each = nrow(Y.proj))), X = rbind(X.proj.complete, 
                                                                      Y.proj))
    st <- tryCatch({
      obj <- ranger::ranger(probability = TRUE, formula = class ~ 
                              ., data = d, num.trees = num.trees.per.proj, 
                            mtry = dim.proj, keep.inbag = TRUE, min.node.size = min.node.size)
      obj
    }, error = function(e) NA)
    if (any(is.na(st))) {
      warning("Forest for a projection was NA, will redo")
    }
    if (!any(is.na(st))) {
      st$var <- vars - 1
      st$full.vars <- paste("X.", 1:ncol(X), sep = "")
      return(st)
    }
  })
  inds <- lapply(list.rf, function(l) {
    if (length(l) == 1) {
      return(FALSE)
    }
    else {
      return(TRUE)
    }
  })
  list.rf <- list.rf[unlist(inds)]
  print(paste0("nr of projections ", length(list.rf)))
  object <- list()
  object$list.rf <- Iscores:::combineForests(list.rf = list.rf)
  return(object)
}





doevaluationown<-function (imputations, methods, X.NA, m, num.proj, num.trees.per.proj, 
                           min.node.size, n.cores = 1, projection.function = NULL) 
{
  if (!is.null(names(imputations))) {
    if (!identical(names(imputations), methods)) {
      stop("imcompatible names between imputations and methods.")
    }
    if (length(imputations) != length(methods)) {
      stop("different lengths of imputations and methods.")
    }
  }
  else {
    names(imputations) <- methods
  }
  X.NA <- as.matrix(X.NA)
  colnames(X.NA) <- NULL
  rownames(X.NA) <- NULL
  ind.candidates <- which(!stats::complete.cases(X.NA))
  ind.candidates <- sort(ind.candidates)
  nrofmissing <- sum(is.na(X.NA))
  scores.all.dr.kl <- list()
  for (method in methods) {
    scores.all.dr.kl[[method]] <- rep(NA, m)
  }
  NA.pat <- X.NA
  NA.pat[!is.na(NA.pat)] <- 1
  NA.pat.unique <- unique(NA.pat)
  NA.pat.groups <- apply(NA.pat.unique, 1, function(p) list(unname(which(apply(NA.pat, 
                                                                               1, function(pp) identical(pp, p))))))
  NA.pat.groups <- unlist(NA.pat.groups, recursive = F)
  V <- apply(NA.pat.unique, 1, sum, na.rm = T)
  NA.pat.unique <- NA.pat.unique[which(V != ncol(X.NA)), , 
                                 drop = F]
  NA.pat.groups <- NA.pat.groups[which(V != ncol(X.NA))]
  lengths.groups <- unlist(lapply(NA.pat.groups, FUN = function(l) length(l)))
  inds.groups.length1 <- which(lengths.groups == 1)
  if (length(inds.groups.length1) > 1) {
    average.diff <- TRUE
    obs.groups.length1 <- unlist(NA.pat.groups[inds.groups.length1])
    pat.groups.length1 <- ifelse(colSums(NA.pat.unique[inds.groups.length1, 
    ]) >= 1, 1, NA)
    NA.pat.groups <- NA.pat.groups[-inds.groups.length1]
    NA.pat.unique <- NA.pat.unique[-inds.groups.length1, 
    ]
    NA.pat.groups[[length(NA.pat.groups) + 1]] <- obs.groups.length1
    NA.pat.unique <- rbind(NA.pat.unique, pat.groups.length1)
    rownames(NA.pat.unique) <- NULL
  }
  else {
    average.diff <- FALSE
  }
  for (method in methods) {
    print(paste0("Evaluating method ", method))
    dat.scoredr.kl <- mclapply(1:nrow(NA.pat.unique), function(j) {
      if (average.diff == TRUE) {
        if (j == nrow(NA.pat.unique)) {
          parts <- 1
        }
        else {
          if (length(NA.pat.groups[[j]]) == 1) {
            parts <- 1
          }
          else {
            parts <- c(1, 2)
          }
        }
      }
      else {
        if (length(NA.pat.groups[[j]]) == 1) {
          parts <- 1
        }
        else {
          parts <- c(1, 2)
        }
      }
      scores.all <- list()
      for (part in parts) {
        if (j != nrow(NA.pat.unique)) {
          if (length(NA.pat.groups[[j]]) == 1) {
            ids.pattern.test <- NA.pat.groups[[j]]
            ids.pattern.train <- 1:nrow(X.NA)
          }
          else {
            if (part == 1) {
              ids.pattern.test <- NA.pat.groups[[j]][1:(floor(length(NA.pat.groups[[j]]) * 
                                                                0.5))]
            }
            else {
              ids.pattern.test <- NA.pat.groups[[j]][-c(1:(floor(length(NA.pat.groups[[j]]) * 
                                                                   0.5)))]
            }
            ids.pattern.train <- setdiff(1:nrow(X.NA), 
                                         ids.pattern.test)
          }
        }
        else {
          if (average.diff == T) {
            ids.pattern.train <- 1:nrow(X.NA)
            ids.pattern.test <- NA.pat.groups[[j]]
          }
          else {
            if (length(NA.pat.groups[[j]]) == 1) {
              ids.pattern.test <- NA.pat.groups[[j]]
              ids.pattern.train <- 1:nrow(X.NA)
            }
            else {
              if (part == 1) {
                ids.pattern.test <- NA.pat.groups[[j]][1:(floor(length(NA.pat.groups[[j]]) * 
                                                                  0.5))]
              }
              else {
                ids.pattern.test <- NA.pat.groups[[j]][-c(1:(floor(length(NA.pat.groups[[j]]) * 
                                                                     0.5)))]
              }
              ids.pattern.train <- setdiff(1:nrow(X.NA), 
                                           ids.pattern.test)
            }
          }
        }
        scores <- lapply(1:m, function(set) {
          X.h <- imputations[[method]][[set]]
          X.h <- as.matrix(X.h)[ids.pattern.train, , 
                                drop = F]
          if (any(is.na(X.h)) == TRUE) {
            stop(paste0("Method ", method, " returns NAs in imputation set ", 
                        set, ". Please check the imputation."))
          }
          if (average.diff == TRUE) {
            if (j != nrow(NA.pat.unique)) {
              normal.proj <- TRUE
            }
            else {
              normal.proj <- FALSE
            }
          }
          else {
            normal.proj <- TRUE
          }
          object.dr <- densityRatioScoreown(X = X.NA[ids.pattern.train, 
                                              , drop = F], Xhat = X.h, x = NA.pat.unique[j, 
                                              ], num.proj = num.proj, num.trees.per.proj = num.trees.per.proj, 
                                     min.node.size = min.node.size, projection.function = projection.function, 
                                     normal.proj = normal.proj)
          Z <- as.matrix(imputations[[method]][[set]])[ids.pattern.test, 
                                                       , drop = F]
          Z <- unname(Z)
          Z <- as.matrix(Z)
          if (any(is.na(object.dr)) || any(is.null(object.dr))) {
            scoredr.kl <- NA
          }
          else {
            if (j == nrow(NA.pat.unique)) {
              if (average.diff == FALSE) {
                scoredr.kl <- mean(Iscores:::compute_drScore(object = object.dr, 
                                                   Z = Z, num.trees.per.proj = num.trees.per.proj, 
                                                   num.proj = num.proj))
              }
              else {
                scoredr.kl <- unlist(Iscores:::compute_drScore(object = object.dr, 
                                                     Z = Z, num.trees.per.proj = num.trees.per.proj, 
                                                     num.proj = num.proj))
              }
            }
            else {
              scoredr.kl <- mean(Iscores:::compute_drScore(object = object.dr, 
                                                 Z = Z, num.trees.per.proj = num.trees.per.proj, 
                                                 num.proj = num.proj))
            }
          }
          return(list(scoredr.kl = scoredr.kl))
        })
        scores.all.part <- unlist(lapply(scores, function(l) l$scoredr.kl))
        scores.all[[part]] <- scores.all.part
      }
      if (length(parts) == 1) {
        dat.scoredr.kl <- unlist(scores.all)
      }
      else {
        dat.scoredr.kl <- mean(unlist(scores.all), na.rm = T)
      }
      return(dat.scoredr.kl)
    }, mc.cores = n.cores)
    scores.all.dr.kl[[method]] <- mean(unlist(dat.scoredr.kl), 
                                       na.rm = T)
  }
  print(paste0("done scoring"))
  return(evaluation = list(scores.all.dr.kl = scores.all.dr.kl))
}








Iscoresown<-function (imputations, methods, X.NA, m = length(imputations[[1]]), 
                      num.proj = 100, num.trees.per.proj = 5, min.node.size = 10, 
                      n.cores = 1, projection.function = NULL, rescale = TRUE) 
{
  Iscores.values <- doevaluationown(imputations = imputations, 
                                    methods = methods, X.NA = X.NA, m = m, num.proj = num.proj, 
                                    num.trees.per.proj = num.trees.per.proj, min.node.size = min.node.size, 
                                    n.cores = n.cores, projection.function = projection.function)
  Iscores.values <- do.call(rbind, Iscores.values)
  if (rescale == TRUE) {
    names.methods <- colnames(Iscores.values)
    Iscores.values <- lapply(Iscores.values, FUN = function(l) {
      l - max(unlist(Iscores.values))
    })
    Iscores.values <- do.call(cbind, Iscores.values)
    colnames(Iscores.values) <- names.methods
  }
  return(Iscores.values)
}


