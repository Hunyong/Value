listdtr2 <- function (y, a, x, stage.x, seed = NULL, kfolds = 5L, fold = NULL, 
          maxlen = 10L, zeta.choices = NULL, eta.choices = NULL) 
{
  if (!is.matrix(y)) {
    y <- as.matrix(y)
  }
  if (!is.data.frame(a)) {
    a <- as.data.frame(a)
  }
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (is.null(colnames(x))) {
    colnames(x) <- paste0("x", seq_len(ncol(x)))
  }
  stopifnot(nrow(y) == nrow(a) && nrow(y) == nrow(x))
  n <- nrow(y)
  stopifnot(ncol(y) == ncol(a))
  n.stage <- ncol(y)
  dtr <- vector("list", n.stage)
  future.y <- double(n)
  if (is.null(colnames(a))) {
    colnames(a) <- paste0("a", 1L:n.stage)
  }
  a.mm <- lapply(1L:n.stage, function(j) model.matrix(as.formula(sprintf("~ -1 + %s", 
                                                                         colnames(a)[j])), a))
  stage.a.mm <- rep.int(1L:n.stage, sapply(a.mm, ncol))
  a.mm <- do.call("cbind", a.mm)
  if (is.null(fold)) {
    if (!is.null(seed)) {
      set.seed(seed)
    }
    fold <- rep_len(1L:kfolds, n)[sample.int(n)]
  }
  for (i.stage in n.stage:1L) {
    current.x <- cbind(x[, which(stage.x <= i.stage), drop = FALSE], 
                       a.mm[, which(stage.a.mm < i.stage), drop = FALSE], 
                       y[, seq_len(i.stage - 1L), drop = FALSE])
    if (ncol(current.x) < 2L) {
      current.x <- cbind(x = current.x, dummy_ = 0)
    }
    current.a <- a[, i.stage]
    current.y <- y[, i.stage] + future.y
    model <- listdtr::krr(current.x, current.y, current.a)
    options <- model$options
    outcomes <- predict(model, current.x)
    regrets <- get.regrets(outcomes)
    obj <- build.rule.cv2(current.x, regrets, kfolds, fold, 
                         maxlen, zeta.choices, eta.choices)
    dtr[[i.stage]] <- obj
    future.y <- outcomes[cbind(1L:n, obj$action)]
  }
  class(dtr) <- "listdtr"
  dtr
}

build.rule.cv2 <- function (x, y, kfolds = 5L, fold = NULL, maxlen = 10L, zeta.choices = NULL, 
          eta.choices = NULL, cv.only = FALSE) 
{
  if (!is.matrix(x) || ncol(x) < 2) {
    x <- cbind(x = x, dummy_ = 0)
  }
  y <- as.matrix(y)
  stopifnot(ncol(y) > 1)
  stopifnot(nrow(x) == nrow(y))
  n <- nrow(x)
  simple.loss <- min(colMeans(y))
  if (simple.loss < 1e-08) {
    zeta.selected <- simple.loss * n
    eta.selected <- simple.loss * n
  }
  else {
    if (is.null(zeta.choices) || is.null(eta.choices)) {
      zeta.grid <- simple.loss * c(2, 0.75, 0.3, 0.12, 
                                   0.05)
      eta.grid <- simple.loss * n * c(0.3, 0.1, 0.03)
      zeta.choices <- rep(zeta.grid, times = 3L)
      eta.choices <- rep(eta.grid, each = 5L)
    }
    if (is.null(fold)) {
      kfolds <- as.integer(kfolds)
      fold <- rep_len(1L:kfolds, n)[sample.int(n)]
    }
    else {
      fold <- as.integer(fold)
      kfolds <- max(fold)
      if (any(tabulate(fold, kfolds) <= 5L)) {
        stop("Some fold(s) have too few observations.")
      }
    }
    fold <- fold - 1L
    
  print((fold=fold))
  print(zeta.choices)
  print(eta.choices)
    cv.loss <- .Call("R_cv_tune_rule", x, y, zeta.choices, 
                     eta.choices, maxlen, fold, kfolds)
    min.cv.loss <- min(cv.loss)
    if (min.cv.loss > simple.loss - 1e-08) {
      zeta.selected <- simple.loss * n
      eta.selected <- simple.loss * n
    }
    else {
      index <- which(cv.loss - min.cv.loss - 1e-08 <= 0)[1L]
      zeta.selected <- zeta.choices[index]
      eta.selected <- eta.choices[index]
    }
  }
  cv <- list(zeta.selected = zeta.selected, eta.selected = eta.selected, 
             metrics = data.frame(zeta.choices = zeta.choices, eta.choices = eta.choices, 
                                  cv.loss = cv.loss))
  if (cv.only) {
    object <- list(cv = cv)
  }
  else {
    object <- listdtr::build.rule(x, y, maxlen, zeta.selected, eta.selected)
    object$cv <- cv
  }
  object
}

%>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% 