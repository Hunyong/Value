infl <- function(data, policy.hat,
                 propensity = rep(0.5, dim(data)[1]),
                 labels = c(y = "y", a = "a", EY1 = "EY1", EY0 = "EY0")) {
  W = (policy.hat == data[,labels["a"]])/propensity
  YW = W * data[,labels["y"]]
  W.bar = mean(W)
  YW.bar = mean(YW)
  psi.i = (YW - YW.bar)/W.bar - (W - W.bar)*YW.bar/W.bar^2
  psi.i
}


DBS <- function(data, ...) { # Dirichlet bootstrap
  psi.i = infl(data = data, ...)
  n = dim(data)[1]
  xi = rexp(n)
  xi = xi/mean(xi)
  dbs = psi.i * xi
  list(DBS.sample = dbs, mean.DBS = mean(dbs), mean.DBS.sq = mean(dbs^2),
       mean.raw = mean(psi.i), mean.raw.sq = mean(psi.i^2))
}

if (FALSE) {
  tmp <- data.generator (1000, type="circle")
  tmp.infl = infl(data=tmp, policy.hat = c(rep(1,500), rep(0,500)))
  DBS(data=tmp, policy.hat = c(rep(1,500), rep(0,500)))
  
}
