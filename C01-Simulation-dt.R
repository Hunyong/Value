### 0. library
  library(ggplot2); library(dplyr); library(magrittr)
  library(randomForest); library(rpart)

### 1. simulation setting
  n.sim = 500
  n = c(30, 100, 300, 1000)
  type = "circle"
  
### 3. simulation runs
  
  # elementary matrix
  value.matrix <- matrix(NA, 21, n.sim, dimnames = list(c(1:20, RF), 1:n.sim)) %>% as_data_frame()
  
  # list of four (=length(n)) value matrices (and value.hat matrices)
  value.hat.DT <- value.DT <- lapply(n, function(x) value.matrix)
  
  tmp.NA <- data_frame(size.actual = c(1:20, Inf))
  if (type == "circle") {pop.1 = pop.1.circle; pop.0 = pop.0.circle} else
    if (type == "steps") {pop.1 = pop.1.steps; pop.0 = pop.0.steps}
  for (i in 1:n.sim) {
    set.seed(i*10)
    tmp <- data.generator (n[length(n)], type=type)
    
    for (j in 1:length(n)) {
      cat(i, "th replicate. sample size", n[j], "\n")
      result <- DT.size.seq(tmp[1:n[j],], size=3:15, pop.1 = pop.1, pop.0 = pop.0)
      print(result)
      tmp.NA %>% left_join(result, by="size.actual") -> result
      value.DT[[j]][,i] <- result$`value (DT_n_approx)`
      value.hat.DT[[j]][,i] <- result$`value.hat (DT_n)`
    }
    
  }
  saveRDS(value.DT, "output/value.DT.circle.180613.rds")
  saveRDS(value.hat.DT, "output/value.hat.DT.circle.180613.rds")
  
  sapply(value.DT, function(x) apply(x, 1, mean, na.rm = TRUE)) %>% 
    as.data.frame %>% setNames(paste0("n.",c(30, 100, 300, 1000))) %>% 
    mutate(model = c(paste0("DT",1:20), "RF")) -> value.mean
    
    #(function(x) {rownames(x) = c(paste0("DT",1:20), "RF"); x}) -> value.mean
  
  value.mean %>%
    reshape(varying = paste0("n.",c(30, 100, 300, 1000)), direction = "long", 
            sep =".", idvar="model") %>% 
    setNames(c("size", "n", "value")) %>% 
    filter(!is.na(value)) %>% 
    ggplot(aes(n, value, col=size)) + geom_line() +
    ggtitle("Value change along the sample size, for each size of trees")
  
  ggsave("R01.simulation-circle.DT.png")


### 1.B simulation setting B (step functions)
n.sim = 500
n = c(30, 100, 300, 1000)
type = "steps"

### 3.B simulation runs B (step functions)

  # elementary matrix
  value.matrix <- matrix(NA, 21, n.sim, dimnames = list(c(1:20, RF), 1:n.sim)) %>% as_data_frame()
  
  # list of four (=length(n)) value matrices (and value.hat matrices)
  value.hat.DT <- value.DT <- lapply(n, function(x) value.matrix)

  tmp.NA <- data_frame(size.actual = c(1:20, Inf))
  if (type == "circle") {pop.1 = pop.1.circle; pop.0 = pop.0.circle} else
    if (type == "steps") {pop.1 = pop.1.steps; pop.0 = pop.0.steps}
  
  for (i in 1:n.sim) {
    set.seed(i*10)
    tmp <- data.generator (n[length(n)], type=type)
    
    for (j in 1:length(n)) {
      cat(i, "th replicate. sample size", n[j], "\n")
      result <- DT.size.seq(tmp[1:n[j],], size=3:15, pop.1 = pop.1, pop.0 = pop.0)
      print(result)
      tmp.NA %>% left_join(result, by="size.actual") -> result
      value.DT[[j]][,i] <- result$`value (DT_n_approx)`
      value.hat.DT[[j]][,i] <- result$`value.hat (DT_n)`
    }
    
  }
  saveRDS(value.DT, "output/value.DT.steps.180614.rds")
  saveRDS(value.hat.DT, "output/value.hat.DT.steps.180614.rds")
  
  sapply(value.DT, function(x) apply(x, 1, mean, na.rm = TRUE)) %>% 
    as.data.frame %>% setNames(paste0("n.",c(30, 100, 300, 1000))) %>% 
    mutate(model = c(paste0("DT",1:20), "RF")) -> value.mean

  value.mean %>%
    reshape(varying = paste0("n.",c(30, 100, 300, 1000)), direction = "long", 
            sep =".", idvar="model") %>% 
    setNames(c("size", "n", "value")) %>% 
    filter(!is.na(value)) %>% 
    ggplot(aes(n, value, col=size)) + geom_line() +
    ggtitle("Value change along the sample size, for each size of trees")
  
  ggsave("R01.simulation-steps.DT.png")
