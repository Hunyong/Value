### value.object structure
# value.object = list(sim.param = list(n.sim.train = n.sim, m.sim.test = m.sim,
#                                      n.train = n.train, n.test = n.test),
#                     data.param = list(data.type = data.type, propensity = propensity,
#                                       seed.fn = seed.fn),
#                     pop = list(pop.0 = pop.0, pop.1 = pop.1),
#                     value = value.list,
#                     model = model.list)

## value.list[[i]] = {value.array}   i = 1:n.sim, Each i corresponds to a training set.
### value.array[k, 1:2, j] = {size or value}
                                      # k = 1:12 rule size, krr, rf
                                      # j = 1:m.sim, Each j corresponds to a test set.


source("F03-Analysis.R")
library(grid); library(gridExtra)
value.object <- readRDS("output/value.distribution.180705.rds")

value.list.actualSize <- train.test.by.model(value.object$value, size="actual")
value.list.maxSize <- train.test.by.model(value.object$value, size="max")

model = c(paste0("List of size ", 1:10), "KRR", "RF")

# V(dn)
  p <- lapply(c(1,4,7,10,11,12), function(i) {
          density.plot(value.list.maxSize[[i]], xlim=c(0, 1), ylim=c(0,70), model = model[i])
        })
  p <- marrangeGrob(p, nrow=3, ncol=2)
  ggsave(paste0("P0201.Vdn.pdf"), p, width = 10, height=12)

# Vm(d1)
  p <- lapply(c(1,4,7,10,11,12), function(i) {
    density.plot(value.list.maxSize[[i]], type = "Vmd", train=1, xlim=c(0.1, 0.9), ylim=c(0,70), model = model[i])
  })
  p <- marrangeGrob(p, nrow=3, ncol=2)
  ggsave(paste0("P0201.Vmd1.pdf"), p, width = 10, height=12)
  
  
# Vm(dn)
  p <- lapply(c(1,4,7,10,11,12), function(i) {
    density.plot(value.list.maxSize[[i]], type = "Vmdn", xlim=c(0.1, 1.1), ylim=c(0,70), model = model[i])
  })
  p <- marrangeGrob(p, nrow=3, ncol=2)
  ggsave(paste0("P0201.Vmdn.pdf"), p, width = 10, height=12)
  
  # rescaled
  p <- lapply(c(1,4,7,10,11,12), function(i) {
    density.plot(value.list.maxSize[[i]], type = "Vmdn", model = model[i])
  })
  p <- marrangeGrob(p, nrow=3, ncol=2)
  ggsave(paste0("P0201.Vmdn.r.pdf"), p, width = 10, height=12)
  