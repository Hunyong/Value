### 0. library
library(ggplot2); library(dplyr); library(magrittr)
library(randomForest); library(rpart); library(listdtr)
source("F01-Simulation-fn.R")

args = commandArgs(trailingOnly=TRUE)  # passed from script
copy = args[1] %>% as.numeric
type = args[2] %>% as.numeric
type = ifelse(type==1, "circle", "steps")

print(c(copy=copy, type=type))

### 1. simulation setting
n.sim = 500
n = c(30, 100, 300, 1000)

### 3. simulation runs

# elementary matrix
value.matrix <- matrix(NA, 11, n.sim, dimnames = list(c(1:10, RF), 1:n.sim)) %>% as_data_frame()

# list of four (=length(n)) value matrices (and value.hat matrices)
value.list <- lapply(n, function(x) value.matrix)

tmp.NA <- data_frame(size.actual = c(1:10, Inf))
if (type == "circle") {pop.1 = pop.1.circle; pop.0 = pop.0.circle} else
  if (type == "steps") {pop.1 = pop.1.steps; pop.0 = pop.0.steps}

# for (i in 1:n.sim) {
for (i in ((copy-1)*50 + 1:50)) {
  set.seed(i*10)
  tmp <- data.generator (n[length(n)], type=type)
  
  for (j in 1:length(n)) {
    cat(i, "th replicate. sample size", n[j], "\n")
    result <- List.size.seq(tmp[1:n[j],], size=10, pop.1 = pop.1, pop.0 = pop.0)
    print(result)
    tmp.NA %>% left_join(result, by="size.actual") -> result
    value.list[[j]][,i] <- result$`value.list.approx`
  }
  
}
saveRDS(value.list, paste0("output/value.list.180620.", type,".", copy,".rds"))


### 2. Simulation results
### 2.1 reading the simulation results
  value.list.circle <- value.list.steps <- value.list
  for (copy in 1:10) {
    value.list.tmp1 <- readRDS(paste0("output/value.list.180620.", "circle.", copy,".rds"))
    value.list.tmp2 <- readRDS(paste0("output/value.list.180620.", "steps.", copy,".rds"))
    for (j in 1:4) {
      value.list.circle[[j]][,((copy-1)*50 + 1:50)] <- value.list.tmp1[[j]][,((copy-1)*50 + 1:50)]
      value.list.steps[[j]][,((copy-1)*50 + 1:50)] <- value.list.tmp2[[j]][,((copy-1)*50 + 1:50)]
    }
  }
  value.list.circle %<>% lapply(as.data.frame)
  value.list.steps %<>% lapply(as.data.frame)

### 2.2 tables and figures - circle model
  # (optimal policy value should be 0.8836)
  value.list.circle %>% 
    sapply(function(x) apply(x,1,mean, na.rm=TRUE)) %>% 
    as.data.frame %>% setNames(paste0("n.",c(30, 100, 300, 1000))) %>%
    mutate(model = c(paste0("List",1:10), "RF")) -> value.mean.circle 
  
  value.mean.circle %>%
    reshape(varying = paste0("n.",c(30, 100, 300, 1000)), direction = "long", 
            sep =".", idvar="model") %>% 
    setNames(c("size", "n", "value")) %>% 
    filter(!is.na(value)) %>% 
    ggplot(aes(n, value, col=size, lty=ifelse(size=="RF","RF","List"))) + geom_line() +
    scale_linetype_discrete(name  ="RF/List") + theme_bw() +
    ggtitle("Value change along the sample size, \n for each size of decision lists (circle model)")
  
  ggsave("R01Simulation-circle-list.png")

### 2.2 tables and figures - circle model
  # (optimal policy value should be 0.6250)
  value.list.steps %>% 
    sapply(function(x) apply(x,1,mean, na.rm=TRUE)) %>% 
    as.data.frame %>% setNames(paste0("n.",c(30, 100, 300, 1000))) %>%
    mutate(model = c(paste0("List",1:10), "RF")) -> value.mean.steps
  
  value.mean.steps %>%
    reshape(varying = paste0("n.",c(30, 100, 300, 1000)), direction = "long", 
            sep =".", idvar="model") %>% 
    setNames(c("size", "n", "value")) %>% 
    filter(!is.na(value)) %>% 
    ggplot(aes(n, value, col=size, lty=ifelse(size=="RF","RF","List"))) + geom_line() +
    scale_linetype_discrete(name  ="RF/List") + theme_bw() +
    ggtitle("Value change along the sample size, \n for each size of decision lists (step model)")
  
  ggsave("R01Simulation-steps-list.png")
  