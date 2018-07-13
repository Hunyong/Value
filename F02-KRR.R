krrdtr <- function (y, a, x, stage.x, seed = NULL, 
                    pop.0, pop.1
                    # kfolds = 5L, fold = NULL, 
                    # maxlen = Inf, zeta.choices = NULL, eta.choices = NULL) {
                    ) {
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
  # if (is.null(fold)) {
  #   if (!is.null(seed)) {
  #     set.seed(seed)
  #   }
  #   fold <- rep_len(1L:kfolds, n)[sample.int(n)]
  # }
  for (i.stage in n.stage:1L) {
    current.x <<- cbind(x[, which(stage.x <= i.stage), drop = FALSE], 
                       a.mm[, which(stage.a.mm < i.stage), drop = FALSE], 
                       y[, seq_len(i.stage - 1L), drop = FALSE])
    if (ncol(current.x) < 2L) {
      current.x <<- cbind(x = current.x, dummy_ = 0)
    }
    current.a <- a[, i.stage]
    current.y <- y[, i.stage] + future.y
    model <<- listdtr:::krr(current.x, current.y, current.a)
    options <- model$options
    outcomes <- predict(model, current.x)
    # regrets <- listdtr:::get.regrets(outcomes)
    # obj <- listdtr:::build.rule.cv(current.x, regrets, kfolds, fold, 
    #                      maxlen, zeta.choices, eta.choices)
    # dtr[[i.stage]] <- obj
    # future.y <- outcomes[cbind(1L:n, obj$action)]
    yhat <- predict(model,pop.1)
    value.approx <- mean(ifelse(yhat[,1] <= yhat[,2], pop.1$EY1, pop.0$EY0))
    # policy.hat <- ifelse(yhat[,1] <= yhat[,2], 1, 0)
  }
  class(dtr) <- "listdtr"
  dtr
  return(value.approx)
}

if (FALSE) {
  tmp <- data.generator (50)
  #tmp.b <- krrdtr(tmp$y, tmp$a, tmp[,c("x1","x2")], stage.x=rep(1,2), maxlen = 10L)  
  tmp.b <- krrdtr(tmp$y, tmp$a, tmp[,c("x1","x2")], stage.x=rep(1,2), pop.0 = pop.0.circle, pop.1 = pop.1.circle)
  model$options
}

