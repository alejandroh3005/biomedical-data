## Looking at permutations ##
rm(list=ls())
library(ggplot2)
library(dplyr)
set.seed(1)

n <- 500

### Generate data where the groups are identical ###
outcome.g1 <- runif(n)
outcome.g2 <- runif(n)
group <- c(rep(1,n), rep(2,n))

sim.data <- data.frame(outcome=c(outcome.g1, outcome.g2), group=group)

### Plot estimated histograms ###
ggplot(data=sim.data, aes(x=outcome,y=..density.., colour = as.factor(group))) + geom_density()

### Permute the group labels ###
perm.data <- sim.data
perm.data$group <- sample(sim.data$group)

ggplot(data=perm.data, aes(x=outcome,y=..density.., colour = as.factor(group))) + geom_density()


#### Generate data where the groups are very different ####
outcome.g1.diff <- runif(n)
outcome.g2.diff <- runif(n) + 3
group.diff <- c(rep(1,n),rep(2,n))

sim.data.diff <- data.frame(outcome = c(outcome.g1.diff, outcome.g2.diff), group = group.diff)

### Plot estimated histograms ###
ggplot(data = sim.data.diff, aes(x=outcome,y=..density.., colour = as.factor(group))) + geom_density()

### Permute the group labels ###
perm.data.diff <- sim.data.diff
perm.data.diff$group <- sample(sim.data.diff$group)

ggplot(data = perm.data.diff, aes(x=outcome,y=..density.., colour = as.factor(group))) + geom_density()

