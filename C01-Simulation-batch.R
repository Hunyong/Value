### 0. library
library(ggplot2); library(dplyr); library(magrittr)
library(randomForest); library(rpart); library(listdtr)
source("F01-Simulation-fn.R")
source("F02-KRR.R")

# args = commandArgs(trailingOnly=TRUE)  # passed from script
# copy = args[1] %>% as.numeric
# type = args[2] %>% as.numeric
# type = ifelse(type==1, "circle", "steps")

copy = 1; type="circle"
print(c(copy=copy, type=type))

### 1. simulation setting
n.sim = 500
n = c(30, 100, 300, 1000)

### 3. simulation runs

# elementary matrix
value.matrix <- matrix(NA, 12, n.sim, dimnames = list(c("RF","KRR",1:10), 1:n.sim)) %>% as_data_frame()

# list of four (=length(n)) value matrices (and value.hat matrices)
value.list <- lapply(n, function(x) value.matrix)

tmp.NA <- data_frame(size.actual = c("RF", "KRR", 1:10))
if (type == "circle") {pop.1 = pop.1.circle; pop.0 = pop.0.circle} else
  if (type == "steps") {pop.1 = pop.1.steps; pop.0 = pop.0.steps}

# for (i in 1:n.sim) {
for (i in ((copy-1)*500 + 1:500)) {
  set.seed(i*10)
  tmp <- data.generator (n[length(n)], type=type)
  
  for (j in 1:length(n)) {
    cat(i, "th replicate. sample size", n[j], "\n")
    result <- List.size.seq(tmp[1:n[j],], size=10, pop.1 = pop.1, pop.0 = pop.0)
    print(result)
    tmp.NA %>% left_join(result, by="size.actual") -> result
    value.list[[j]][,i] <- result$`value.list.approx`
  }
  if (i %% 100 == 0) {saveRDS(value.list, paste0("output/value.list.180623.", type,".cumulative.i.",i,".rds"))}
  
}
saveRDS(value.list, paste0("output/value.list.180623.", type,".rds"))


value.list.circle <- value.list.steps <- value.list
### reading the simulation results
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


value.list.circle %>% sapply(function(x) apply(x,1,mean, na.rm=TRUE)) -> value.mean.circle
value.list.steps %>% sapply(function(x) apply(x,1,mean, na.rm=TRUE)) -> value.mean.steps
