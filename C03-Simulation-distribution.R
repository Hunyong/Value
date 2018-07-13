### library
### 0. library
library(ggplot2); library(dplyr); library(magrittr)
library(randomForest); library(rpart)
library(listdtr)
source("F01-Simulation-fn.R")
source("F02-KRR.R")
source("F03-listdtr.seq.R")
source("F03-Analysis.R")


### parameters (input here)
  n.sim = 500          # number of training sets : i
  m.sim = 500          # number of test sets     : j
  size.rule = c(1:10L)    #: k
  n.train =500         # training set size
  n.test = 500          # test set size
  seed.fn = function(i, j) {i*1000 + j}
  
  propensity = 0.5
  data.type = "circle"
  pop.0 = pop.0.circle
  pop.1 = pop.1.circle
  
### empty shells  
  value.array = array(NA, c(length(size.rule) + 2, 2, m.sim + 1), 
                      dimnames = list(c(size.rule, "krr", "rf"),
                                      c("size.actual", "value.hat"),
                                      paste0("test.",c(1:m.sim, "pop")))) # value of a rule
  value.list = vector("list", n.sim) # list of all results.
  names(value.list) = paste0("train.", 1:n.sim)
  model.list = vector("list", n.sim) # list of all results.
  
  value.object = list(sim.param = list(n.sim.train = n.sim, m.sim.test = m.sim,
                                       n.train = n.train, n.test = n.test),
                      data.param = list(data.type = data.type, propensity = propensity,
                                        seed.fn = seed.fn),
                      pop = list(pop.0 = pop.0, pop.1 = pop.1),
                      value = value.list,
                      model = model.list)
  tt(1)
  for (i in 1:n.sim) {
    cat("\nTraining sample i = ", i, " (Now training the rule) \n")
    value.array.tmp = value.array
    
    ## training (rule estimation)
    j = 0; set.seed(seed.fn(i, j))
    data.train = data.generator (n = n.train, propensity = propensity, type=data.type)
    
    model.list.i = tryCatch({ model.tmp = listdtr.seq(data.train$y, data.train$a, data.train[,c("x1","x2")], stage.x=rep(1,2), 
                                                      maxlen = size.rule, showFlag = FALSE)
                              model.tmp$rf = randomForest(y~x1+x2+a, data=data.train)
                              model.tmp
                            },
                             error=function(error_message) {
                             message(error_message)
                             result = NA
                             return(result)
                           })
    
    p.hat.inv = rep(1, n.test) # replace p.hat.inv with (1/estimated propensity), later on.
    
    if (class(model.list.i) != "list") { # if the class is not list, then it means error. So skip!
      value.list[[i]] <- value.array.tmp  # just plug in empty value.array and go to next.
      next
    }
    
    ## testing (value estimation)
    cat("Training sample i = ", i, "(Now value estimation 1:", m.sim, ")\n")
    for (j in c(1:m.sim,m.sim+1)) {
  cat("\nj=", j, ":")
      if (j <= m.sim) { # random test set (V.hat)
        set.seed(seed.fn(i, j))
        data.test = data.generator (n = n.test, propensity = propensity, type=data.type)
  cat("k=")      
        for (k in length(size.rule):1) {
  cat(k, "..")
          value.array.tmp[k,1,j] = dim(model.list.i[[k]][[1]]$rule)[1]           # actual size
          if (k < length(size.rule) & value.array.tmp[k,1,j] == value.array.tmp[k+1,1,j]) {
            value.array.tmp[k,2,j] = value.array.tmp[k+1,2,j]  # value hat
          } else {
            policy.hat <- predict(model.list.i[[k]], stage=1, xnew = data.test[,c("x1", "x2")] %>% as.matrix)
            policy.hat %<>% as.character %<>% as.numeric
            policy.hat = ifelse(policy.hat == data.test$a, p.hat.inv, 0) # = I(A=d)/P(A=1)
            value.array.tmp[k,2,j] = weighted.mean(data.test$y, w = policy.hat)  # value hat
          }
        } #k
        # for #krr and rf
        for (k in (length(size.rule) + 1:2)) { # krr and rf
  cat(k, "..")
          if (k == length(size.rule) + 1) { #krr
            policy.hat <- predict(model.list.i[[k]][[1]], data.test)
          } else { #rf
            policy.hat <- cbind(predict(model.list.i[[k]], data.test %>% mutate(a=0)),
                                predict(model.list.i[[k]], data.test %>% mutate(a=1)))
          }
          policy.hat <- (policy.hat[,1] <= policy.hat[,2])
          policy.hat %<>% as.numeric
          policy.hat = ifelse(policy.hat == data.test$a, p.hat.inv, 0) # = I(A=d)/P(A=1)
          value.array.tmp[k,2,j] = weighted.mean(data.test$y, w = policy.hat)  # value hat
          value.array.tmp[k,1,j] = Inf                                         # actual size 
        } #k
        
      } else { # population (V*)
        cat("(population). k=")
        for (k in length(size.rule):1) {
          cat(k, "..")
          value.array.tmp[k,1,j] = dim(model.list.i[[k]][[1]]$rule)[1]           # actual size
          if (k < length(size.rule) & value.array.tmp[k,1,j] == value.array.tmp[k+1,1,j]) {
            value.array.tmp[k,2,j] = value.array.tmp[k+1,2,j]  # value hat
          } else {
            policy.hat <- predict(model.list.i[[k]], stage=1, xnew = pop.1[,c("x1", "x2")] %>% as.matrix)
            policy.hat %<>% as.character %<>% as.numeric
            value.array.tmp[k,2,j] = mean(ifelse(policy.hat, pop.1$EY1, pop.0$EY0)) # value hat
          }
        } #k
        
        # for #krr and rf
        {
          k = length(size.rule) + 1    # krr
          policy.hat <- predict(model.list.i[[k]][[1]], stage=1, xnew = pop.1[,c("x1", "x2")] %>% as.matrix)
          policy.hat <- (policy.hat[,1] <= policy.hat[,2])
          value.array.tmp[k,2,j] = mean(ifelse(policy.hat, pop.1$EY1, pop.0$EY0)) # value hat
          value.array.tmp[k,1,j] = Inf           # actual size
          
          k = length(size.rule) + 2   # rf
          policy.hat <- (predict(model.list.i[[k]], pop.1) >= predict(model.list.i[[k]], pop.0))
          value.array.tmp[k,2,j] = mean(ifelse(policy.hat, pop.1$EY1, pop.0$EY0)) # value hat
          value.array.tmp[k,1,j] = Inf           # actual size
        }
    } #if-else statement
    } #j
    # When every testing (value estimation) is done,
    # then plug in value.array.tmp to the list[[i]]
    value.list[[i]] <- value.array.tmp
    model.list[[i]] <- model.list.i
    
    if (i %% 100 == 0) saveRDS(value.list, paste0("output/value.distribution.180705.val.",i,".rds"))
    if (i %% 100 == 0) saveRDS(model.list, paste0("output/value.distribution.180705.mod.",i,".rds"))
  } #i
  value.object$value = value.list
  value.object$model = model.list
value.object$value
tt(2)  
# saveRDS(value.object, "output/value.distribution.180705.rds")
# value.object <- readRDS("output/value.distribution.180705.rds")