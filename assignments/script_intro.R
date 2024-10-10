### ALWAYS USE A SCRIPT ###

### The "#" indicates that something is a comment, nothing to the right of the # will be executed ###

print("hello") #THIS COMMENT WILL NOT BE EXECUTED (though the print command will)

### Comments should be used liberally to document your code! ###

### First Load the libraries you want (REMEMBER TO INSTALL THEM THE FIRST TIME YOU USE THEM) ###

#install.packages("dplyr")
library(dplyr)

### Then set any global options ###
rm(list=ls())
options(digits = 3)
setwd("~/Dropbox/Teaching/UW/BIOST544/2024/lectures/lecture1/code/")

### Then set a seed if you do _anything_ random in your code ###
set.seed(1)

### Now we read our data ###
data <- read.table("../../../data/nsclc-data/nsclc-modified.txt", header=TRUE)

### And even when we are just exploring our data we use the script ###
names(data)
head(data)
nrow(data)

data$age
data %>% select(age)
select(data, age)

data %>% summarise(prop.treated = mean(tx))
mean(data$tx)

##
## Key dplyr fns
##  
## filter(): Pick observations by their values
## arrange(): Reorder the rows
## select(): Pick variables by their names
## mutate(): Create new variables with functions of existing variables
## summarise(): Collapse many values down to a single summary
##

data %>% group_by(tx) %>%
         summarise(avg.age = mean(age))

data %>% group_by(tx,male) %>%
         summarise(avg.age = mean(age))

data %>% group_by(tx,male) %>%
         filter(europe == 0) %>%
         summarise(avg.age = mean(age), var.age = var(age))

data.europe <- data %>% filter(europe == 1)

data.europe.old <- data %>% 
                   filter(europe == 1) %>%
                   filter(age > 60)

data.europe.old %>% select(ptid, age, tx, response)

data %>% 
  filter(europe == 1) %>%
  filter(age > 60) %>% 
  select(ptid, age, tx, response)

data.europe.mod <- data.europe %>% mutate(is.old = (age > 60)*1)
data.europe.mod$is.old
