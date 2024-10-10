## Here we test if the statement pi_T \leq pi_C is consistent with the data ##

rm(list=ls())

library(dplyr)
options(digits = 3)

setwd("~/Dropbox/Teaching/UW/BIOST544/current/lectures/lecture2/code")
set.seed(1)

### Initial read-in of data ###
data <- read.table("../../../data/nsclc-data/nsclc-modified.txt", header = TRUE)

### Get Response Proportions ###
(resp.prop.overall <- data %>%
                      summarise(prop = mean(survival.past.400)) %>%
                      .$prop)

(resp.prop.treat <- data %>%
                    filter(tx == 1) %>%
                    summarise(prop = mean(survival.past.400)) %>%
                    .$prop)

(resp.prop.control <- data %>%
                      filter(tx == 0) %>%
                      summarise(prop = mean(survival.past.400)) %>%
                      .$prop)

(num.per.arm <- data %>%
                group_by(tx) %>%
                summarise(number = n()))

(num.control <- num.per.arm$number[1])
(num.treat <- num.per.arm$number[2])

### Calculating an empirical measure of effectiveness ###
(prop.diff <- resp.prop.treat - resp.prop.control)


### Writing a function to run one simulated trial ###
simulate.trial <- function(pi.treat, pi.control, n.treat, n.control){
    patients.treat <- rbinom(1,n.treat,pi.treat)
    patients.control <- rbinom(1,n.control,pi.control)

    prop.diff <- patients.treat/n.treat - patients.control/n.control

    return(prop.diff)
}

### Run a large number of simulated trials ###
ntrial <- 10000

simulated.prop.diffs <- replicate(ntrial,
                                  simulate.trial(resp.prop.overall,
                                                   resp.prop.overall,
                                                   num.treat,
                                                   num.control))

### Plot the results ###
hist(simulated.prop.diffs)
abline(v = prop.diff, col = "red")

mean(simulated.prop.diffs >= prop.diff)

