rm(list=ls())

library(dplyr)
library(ggplot2)
options(digits = 3)

setwd("~/Dropbox/Teaching/UW/BIOST544/current/lectures/lecture2/code")
set.seed(1)

### Initial read-in of data ###

nsclc <- read.table("../../../data/nsclc-data/nsclc-modified.txt", header = TRUE)

### Do treated patients have a longer average survival/censoring time than untreated patients? ###

### We first get some graphical summaries ###

ggplot(data = nsclc, mapping = aes(x=obstime, y=after_stat(density))) +
  geom_histogram(binwidth = 30) + geom_density()

ggplot(data = nsclc, aes(x=obstime,y=after_stat(density), fill = as.factor(tx))) + 
    geom_histogram(alpha = 0.4, position="identity", binwidth = 30)

ggplot(data = nsclc, aes(x=obstime,y=after_stat(density), colour = as.factor(tx))) + geom_density()

ggplot(data = nsclc, aes(x=as.factor(tx), y=obstime, fill=as.factor(tx))) + geom_boxplot()

### Getting numerical summary of survival/censoring times ###

(time.summ <- nsclc %>%
             group_by(tx) %>%
             summarise(avg.time = mean(obstime), median.time = median(obstime)))

(diff.summ <- time.summ[2,2:3] - time.summ[1,2:3])

### PAUSE HERE -- BACK TO LECTURE ###


### Let's evaluate things via permutation ###

simulate.perm.trial <- function(data){

    ## create the permuted data ##
    perm.data <- data
    perm <- sample(1:nrow(data), replace = FALSE)
    perm.data$tx <- data$tx[perm]

    ## calculate the mean/median differences on permuted data
    perm.mean.diff <- with(perm.data,
                           mean(obstime[tx == 1]) - mean(obstime[tx == 0]))
    perm.median.diff <- with(perm.data,
                             median(obstime[tx == 1]) - median(obstime[tx == 0]))
    return( c(perm.mean.diff, perm.median.diff) )
}


### Running a large number of simulated trials for the mean and median ###
nsim <- 1e4
set.seed(1)
permuted.stats <- data.frame(t(replicate(nsim, simulate.perm.trial(nsclc)))) ## note we do a little formatting here
colnames(permuted.stats) <- c("mean.diff","median.diff")

## Graphically evaluating

ggplot(permuted.stats, aes(x=mean.diff, y=after_stat(density))) +
    geom_density() +
    geom_vline(xintercept=diff.summ$avg.time, colour = "red")

ggplot(permuted.stats, aes(x=median.diff, y=after_stat(density))) +
    geom_density() +
    geom_vline(xintercept=diff.summ$median.time, colour = "red")

## Calculating tail probabilities
mean(permuted.stats$mean.diff >= diff.summ$avg.time)
mean(permuted.stats$median.diff >= diff.summ$median.time)

