# max size to actual size
size.by.actual <- function(value.list) {
  n.sim = length(value.list)
  kj = dim(value.list[[1]])
  sizes = kj[1] # k
  m.sim = kj[3] # j
  Inf.index = which(rownames(value.list[[1]][,,1]) %in% c("krr", "rf"))
  
  value.list %<>% lapply(function(x) { # loop over i = 1:n.sim
    x.tmp = x                       # making an empty shell.
    x.tmp[-c(1,Inf.index),,] <- NA  # null model, krr and rf remain.
    for (j in 1:m.sim) { # loop over j (test set)
      for (k in 2:sizes) {
        if (k %in% Inf.index) next
        if (x[k,"size.actual",j] != x[k-1,"size.actual",j]) {
          x.tmp[k,,j] <- x[k,,j]
        }
      }
    }
    x.tmp
  })
  value.list
}

# convert value.list object into a list of test x train matrix for each model.
train.test.by.model <- function(value.list, size = c("actual", "max")) {
  if (size[1] == "actual") value.list %<>% size.by.actual
  
  # list of 3-dim arrays => into 4-dim array
  # model x (size/value) x test x train
  value.list = do.call({function(...) abind:::abind(..., along=4)}, value.list)
  
  # 4-dim array => into list of 2-dim matrix (list by models)
  models = 1:dim(value.list)[1]
  value.list = lapply(models, function(i) value.list[i,2,,])  # 2: value only (drop size)
  value.list
}

if (FALSE) {
  size.by.actual(value.object$value)
  train.test.by.model(value.object$value) ->a
}


distn <- function(value.matrix, type = c("Vdn", "Vmd", "Vmdn"), train=1) {
  m.test = dim(value.matrix)[1]-1
  n.train = dim(value.matrix)[2]
  if (type[1] == "Vdn") { # value fixed, training data variable
    result = value.matrix[m.test+1,]
  } else if (type[1] == "Vmd") { # value variable, training data fixed
    result = value.matrix[1:m.test, train]
  } else if (type[1] == "Vmdn") { # Both train and test sets are variable
    result = as.vector(value.matrix[1:m.test, ])
  }
  result
}

density.plot <- function(value.matrix, type = c("Vdn", "Vmd", "Vmdn"), train=1, model = NULL, xlim=NULL, ylim=NULL) {
  vec <- distn(value.matrix= value.matrix, type = type, train = train)
  
  stat = c(mean = mean(vec, na.rm=T), sd = sd(vec, na.rm=T))
  stat.text = paste0(c("Mean = ", "SD = "), round(stat,4), collapse = ", ")
  
  if (type[1] == "Vmd") {type = paste0(type, " for training set #", train)}
  
  data.frame(x=vec) %>% ggplot(aes(x)) +
    geom_density() + 
    ggtitle(paste0("The distribution of ", type, " / model = ", model)) -> p
  if (!is.null(xlim)) {
    p = p + xlim(xlim) + annotate("text", x = mean(xlim), y=10, col="blue", label = stat.text)
  } else {
    p = p + annotate("text", x = stat[1], y=10, col="blue", label = stat.text)  
  }
  if (!is.null(ylim)) p = p + ylim(ylim)
  p
}


if (FALSE) {
  distn(value.list.maxSize[[11]])
  # Vdn
  density.plot(value.list.maxSize[[10]], xlim=c(0.6, 0.9), model = "List size 10")
  density.plot(value.list.maxSize[[11]], xlim=c(0.6, 0.9), model = "KRR")
  density.plot(value.list.maxSize[[12]], xlim=c(0.6, 0.9), model = "RF")
  # Vmd
  density.plot(value.list.maxSize[[11]], type = "Vmd", train = 2, model = "KRR")
  density.plot(value.list.maxSize[[11]], type = "Vmd", train = 3, model = "KRR")
  
  density.plot(value.list.maxSize[[11]], type = "Vmdn", model = "KRR")
  density.plot(value.list.maxSize[[5]], type = "Vmdn", model = "List size 5")
  
}




