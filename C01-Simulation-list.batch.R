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
  # if (i<3) next
  set.seed(i*10)
  tmp <- data.generator (n[length(n)], type=type)
  
  for (j in 1:length(n)) {
    cat(i, "th replicate. sample size", n[j], "\n")
    result <- List.size.seq(tmp[1:n[j],], size=10, pop.1 = pop.1, pop.0 = pop.0)
    print(result)
    tmp.NA %>% left_join(result, by="size.actual") -> result
    value.list[[j]][,i] <- result$`value.list.approx`
  }
  if (i %% 10 == 0) {saveRDS(value.list, paste0("output/value.list.180623.", type,".cumulative.i.",i,".rds"))}
  
}
saveRDS(value.list, paste0("output/value.list.180623.", type,".rds"))


### reading the simulation results
  value.list.circle <- readRDS(paste0("output/value.list.180623.circle.rds"))
  value.list.circle %<>% lapply(as.data.frame)
  value.list.circle %>% sapply(function(x) apply(x,1,mean, na.rm=TRUE)) -> value.mean.circle
  
  value.list.circle %>% 
    sapply(function(x) apply(x,1,mean, na.rm=TRUE)) %>% 
    as.data.frame %>% setNames(paste0("n.",c(30, 100, 300, 1000))) %>%
    mutate(model = c("RF", "KRR", paste0("List",1:10))) -> value.mean.circle 
  
  value.mean.circle %>%
    reshape(varying = paste0("n.",c(30, 100, 300, 1000)), direction = "long", 
            sep =".", idvar="model") %>% 
    setNames(c("size", "n", "value")) %>% 
    filter(!is.na(value)) %>% 
    #ggplot(aes(n, value, col=size, lty=ifelse(size=="RF","RF","List"))) + geom_line() +
    ggplot(aes(n, value, col=size, lty=match(size,c("a","RF","KRR"),nomatch=1) %>% factor(labels=c("List", "RF", "KRR")))) + geom_line() +
    scale_linetype_discrete(name  ="RF/KRR/List") + theme_bw() +
    geom_abline(intercept=0.8836, slope=0, col="red") +
    ggtitle("Value change along the sample size, \n for each size of decision lists (circle model)")
  
  ggsave("R01Simulation-circle-list.png")
  