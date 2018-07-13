### Aim: to see if "V(Tree) - V(Tree_n) > V(RF) - V(RF_n)", where
###       V is the true value function, Tree and RF are the true classifiers, _n is the estimated.

# time calculator
tt <- function(s){
  if (s==1) {time.tmp <<- Sys.time() # record time
  } else if (s==2) { # calculate time
    return(data.frame(begin = time.tmp, end = Sys.time(), elapsed = Sys.time() - time.tmp))
  }
}

### 1. data.generator
data.generator <- function(n = 1000, propensity = 0.5, type="circle") {
  
  # Features
  x1 = runif(n)*4 - 2                 #X1, X2 ~ U(-2, 2)
  x2 = runif(n)*4 - 2
  
  # Treatment and error
  a = rbinom(n, 1, propensity)
  error = rnorm(n)
  
  # Outcomes
  if (type == "circle") {
    EY1 = (x1 + x2) + 3 - x1^2 - x2^2   # decision boundary: Treat if (X1^2 + X2^2 - 2 < 0)
    EY0 = (x1 + x2)                     # value = pi*(int_0^3 (3-y) dy = 9/2) /16 = 9pi/32 = 0.8836
  } else if (type == "steps") {
    EY1 = (x1 + x2) + 1*(x2 <= ceiling(x1)) - 1*(x2  > ceiling(x1))
                                        # decision boundary: Treat if (X2 - ceiling(X1) <0)
    EY0 = (x1 + x2)                     # value = 10/16 = 5/8 = 0.6250
  } else stop("No valid type is chosen.")
  
  y = ifelse(a, EY1, EY0) + error
  
  return(data.frame(x1, x2, a, y, EY1, EY0, error))
}

  if (FALSE) {
    library(ggplot2); library(dplyr)
    tmp <- data.generator (1000)
    tmp %>% ggplot(aes(x1, x2, col=y)) + geom_point(size=1) + facet_grid(.~a)
  }

### 1.2 value approximator
expand.grid(seq(-2,2, by=0.01),seq(-2,2, by=0.01)) %>% 
  transmute(x1 = Var1, x2 = Var2, a=NA) -> pop

  ### circle data
  pop %>% mutate(a=1, EY1 = (x1 + x2) + 3 - x1^2 - x2^2)  -> pop.1.circle
  pop %>% mutate(a=0, EY0 = (x1 + x2))                    -> pop.0.circle
  ifelse(pop.1.circle$EY1 >= pop.0.circle$EY0, pop.1.circle$EY1, pop.0.circle$EY0) %>% mean #0.8791 /0.8836
  
  ### steps data
  pop %>% mutate(a=1, EY1 = (x1 + x2) + 1*(x2 <= ceiling(x1)) - 1*(x2  > ceiling(x1))) -> pop.1.steps
  pop %>% mutate(a=0, EY0 = (x1 + x2))  -> pop.0.steps
  ifelse(pop.1.steps$EY1 >= pop.0.steps$EY0, pop.1.steps$EY1, pop.0.steps$EY0) %>% mean #0.6244 /0.6250
  
### 2. policy estimators
  RF <- function(data, pop.0, pop.1,...) {
    rf <- randomForest(y~x1+x2+a, data=data, ...)
    RF.tmp <<- rf
  # cat("fitted. \n")  
    y1.hat <- predict(rf, pop.1)
  # cat("predicted1. \n")
    y0.hat <- predict(rf, pop.0)
  # cat("predicted0. \n")
    policy.hat <- (y1.hat >= y0.hat)
    value.hat = mean(pmax(y1.hat, y0.hat))
  # cat("val hat. \n")
    value.approx = mean(ifelse(policy.hat, pop.1$EY1, pop.0$EY0))
  # cat("val.approx. \n")  
    result = c(value.approx, value.hat)
    names(result) = c("value (RF_n_approx)", "value.hat (RF_n)")
    return(result)
  }
  if (FALSE) {
    RF(tmp, pop.0 = pop.0.circle, pop.1 = pop.1.circle)
    }
  
  DT <- function(data, size = 10, pop.0, pop.1) {
    n = dim(data)[1]
    dt <- rpart(y~x1+x2+a, data=data, 
                control = rpart.control(minsplit = n/(size/1.5), cp=.0001, maxcompete=1, maxsurrogate = 1))
    DT.tmp <<- dt # for diagnostic
    y1.hat <- predict(dt, pop.1)
    y0.hat <- predict(dt, pop.0)
    policy.hat <- (y1.hat >= y0.hat)
    value.hat = mean(pmax(y1.hat, y0.hat))
    value.approx = mean(ifelse(policy.hat, pop.1$EY1, pop.0$EY0))
    size.actual = dt$where %>% unique %>% length
    result = c(value.approx, value.hat, size, size.actual)
    names(result) = c("value (DT_n_approx)", "value.hat (DT_n)", "size.designed", "size.actual")
    return(result)
  }
  DT.vec <- Vectorize(DT, vectorize.args = "size")
  
  DT.size.seq <- function(data, size = 3:10, include.RF = TRUE, pop.0, pop.1) {
    result <- DT.vec(data = data, size = size, pop.0 = pop.0, pop.1 = pop.1) %>% 
      t %>% as_data_frame
    # remove duplicated trees
    result %>% filter(!duplicated(result$size.actual)) %>% select(-size.designed) -> result
    if (include.RF) {result <- rbind(result, c(RF(data,  pop.0 = pop.0, pop.1 = pop.1), Inf))}
    return(result)
  }
  if (FALSE) {
    tmp <- data.generator (1000, type="circle")
    DT(tmp, size=2, pop.0 = pop.0.circle, pop.1 = pop.1.circle)
    DT(tmp, size=3, pop.0 = pop.0.circle, pop.1 = pop.1.circle)
    DT(tmp, size=4, pop.0 = pop.0.circle, pop.1 = pop.1.circle)
    DT(tmp, size=5, pop.0 = pop.0.circle, pop.1 = pop.1.circle)
    DT.size.seq(tmp, size=3:10, pop.0 = pop.0.circle, pop.1 = pop.1.circle)
    
    tmp <- data.generator (1000, type="steps")
    DT(tmp, size=2, pop.0 = pop.0.steps, pop.1 = pop.1.steps)
    DT(tmp, size=3, pop.0 = pop.0.steps, pop.1 = pop.1.steps)
    DT(tmp, size=4, pop.0 = pop.0.steps, pop.1 = pop.1.steps)
    DT(tmp, size=5, pop.0 = pop.0.steps, pop.1 = pop.1.steps)
    DT.size.seq(tmp, size=3:10, pop.0 = pop.0.steps, pop.1 = pop.1.steps)
    }

  ### 2. policy estimators by list
  List <- function(data, size = 10, pop.0, pop.1) {
    size %<>% as.integer
    n = dim(data)[1]
    tryCatch(
      {LT = listdtr(data$y, data$a, data[,c("x1","x2")], stage.x=rep(1,2), maxlen = size)
      List.tmp <<- LT # for diagnostic
      policy.hat <- predict(LT, stage=1, xnew = pop.1[,c("x1", "x2")] %>% as.matrix)
      policy.hat %<>% as.character %<>% as.numeric
      value.approx = mean(ifelse(policy.hat, pop.1$EY1, pop.0$EY0))
      size.actual = dim(LT[[1]]$rule)[1]
      result = c(value.approx, size, size.actual)
      names(result) = c("value.list.approx", "max.length", "size.actual")
      return(result)
      },
      error=function(error_message) {
        message(error_message)
        result = c(value.list.approx = NA, max.length=size, size.actual=1)
        return(result)
      }
    )
  }
    
  
  List.size.seq <- function(data, size = 20, include.RF = TRUE, include.KRR = TRUE, pop.0, pop.1) {
    cat("Trying size ", size, ". ")
    result <- List(data = data, size = size, pop.0 = pop.0, pop.1 = pop.1)
    size = result["size.actual"] - 1
    result %<>% 
      matrix(nrow=1, dimnames = list(NULL,names(result))) %<>% 
      as.data.frame
# print(result)
    while (size > 2) {
      cat("Trying size ", size, ". ")
      result.new <- List(data = data, size = size, pop.0 = pop.0, pop.1 = pop.1)
      size = result.new["size.actual"] - 1
      result <- rbind(result, result.new)
# print(result)   
    }
    result %<>% as_data_frame(stringsAsFactors =FALSE)
# print(result)
    
    if (include.KRR) {
      cat("Trying KRR. ")
      krr.val = krrdtr(data$y, data$a, data[, c("x1", "x2")], stage.x=rep(1,2), pop.0 = pop.0, pop.1 = pop.1)
      result <- rbind(data.frame(value.list.approx=krr.val, max.length = Inf, size.actual = "KRR", stringsAsFactors =FALSE), result)
    }
    if (include.RF) {
      cat("Trying RF. ")
      rf.val = RF(data,  pop.0 = pop.0, pop.1 = pop.1)["value (RF_n_approx)"]
      result = rbind(data.frame(value.list.approx=rf.val, max.length = Inf, size.actual = "RF", stringsAsFactors =FALSE), result)
    }
    return(result)
  }
  
  if (FALSE) {
    tmp <- data.generator (1000, type="circle")
    List(tmp, pop.0=pop.0.circle,pop.1=pop.1.circle) -> tmp.list
    List.size.seq(tmp, size=10, pop.0 = pop.0.circle, pop.1 = pop.1.circle) -> tmp.list2
    List(tmp[1:30,], pop.0=pop.0.circle,pop.1=pop.1.circle) -> tmp.list.3
    
    set.seed(2)
    tmp <- data.generator (1000, type="circle")
    List.size.seq(tmp, size=10, pop.0 = pop.0.circle, pop.1 = pop.1.circle) -> tmp.list
    List.size.seq(tmp[1:100,], size=10, pop.0 = pop.0.circle, pop.1 = pop.1.circle) -> tmp.list4
    class(tmp.list4)
    
    set.seed(3)
    tmp <- data.generator (100, type="circle")
    List.size.seq(tmp, size=10, pop.0 = pop.0.circle, pop.1 = pop.1.circle) -> tmp.list
    
    
  }
  