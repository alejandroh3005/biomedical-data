options(digits = 3) ## Formats output to 3 digits
library(ggplot2)
library(dplyr)
library(readr)
library(data.table)

##
## part I: practice sampling distributions
##

# Let's write a fn to get the sampling distribution of pi-hat

# As before, we first write a fn to simulate one trial
sim_one <- function(n, pi){
  prop.resp <- rbinom(n = 1, size = n, prob = pi) / n
  return(prop.resp)
}

n <- 100
nsims <- 1e4  # 10,000 simulations
pi <- 0.3

set.seed(1)
pihat_dist <- replicate(nsims, sim_one(n, pi))
hist(pihat_dist)


##
## part II: practice building CIs using the plugin principle
##

nsclc <- read.table("data/nsclc-modified.txt", header=TRUE)
nsclc.treated <- nsclc %>% filter(tx == 1)

resp.prop.treat <- mean(nsclc.treated$survival.past.400)

distance.away <- function(pi, n, lower.p=0.05, upper.p=0.95, nsim=1e4){
  samples <- rbinom(nsim, n, pi) / n
  samples <- samples**3
  cutoffs <- quantile(samples, c(lower.p, upper.p))
  dists <- cutoffs - pi
  return(list(distances=dists, samples=samples))
}

n <- nrow(nsclc.treated)
temp <- distance.away(pi=resp.prop.treat, n=n)
dist.best.est <- temp$distances
hist(temp$samples)
abline(v = dist.best.est[1], col = "red")
CIs <- dist.best.est + resp.prop.treat
c(Estimate = resp.prop.treat, CIs)

##
## part III: practice building CIs using (nonparametric) bootstrap
##
  
# we will build CIs for median age in nsclc data

# write a function to calculate the summary statistic of interest  
calc_summ <- function(data){
  summ <- median(data$age)
  return(summ)
}

medage <- calc_summ(nsclc.treated)

## write a (general) fn to do one bootstrap replicate
do_one_boot <- function(data){
  ind_boot <- sample(nrow(data), replace=TRUE)
  resampled_data <- data[ind_boot,]
  resampled_summary <- calc_summ(resampled_data)
  return(resampled_summary)
}

## calculate the sampling distribution
calc_sampling_dist <- function(data, n_boot=1e3){
  samples <- replicate(n_boot, do_one_boot(data)) # complete this line
  return(samples)
}

## calculate confidence intervals 
form_CI <- function(data, prob_bounds=c(0.05,0.95)){
  sampling_dist <- calc_sampling_dist(data)
  quants <- quantile(sampling_dist, prob_bounds)
  center <- calc_summ(data)
  upper_dist <- quants[2] - center
  lower_dist <- center - quants[1]
  CI <- c(center - upper_dist, center + lower_dist)
  names(CI) <- prob_bounds
  return(CI)
}

sampling_dist <- calc_sampling_dist(nsclc.treated)
CIs <- form_CI(nsclc.treated)

hist(sampling_dist)
abline(v = CIs, col = "red")
