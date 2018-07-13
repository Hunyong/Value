source("F03-Analysis.R")
source("F04-DirichletBS.R")
value.object <- readRDS("output/value.distribution.180705.rds")
value.object %>% names

value.object$model %>% length # 500 training data sets

value.object$model[[1]] # training data 1.
value.object$model[[1]][[2]] # training data 1. - decision list of size 2

{
  tmp.seed = value.object$data.param$seed.fn(i=1, j=1)
  tmp.n.test = value.object$sim.param$n.test
  tmp.model = value.object$model[[1]][[10]] #1: tr1, 11: krr
  
  set.seed(tmp.seed)
  tmp.data = data.generator (tmp.n.test, type="circle")
  tmp.policy = predict(tmp.model, stage=1, xnew = tmp.data[,c("x1", "x2")] %>% as.matrix)
  infl(data=tmp.data, policy.hat=tmp.policy) %>% mean
  DBS(data=tmp.data, policy.hat=tmp.policy) %>% mean
}