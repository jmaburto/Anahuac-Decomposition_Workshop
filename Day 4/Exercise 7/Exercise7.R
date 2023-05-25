# Setup -------------------------------------------------------------------

library(here)
wd <- here()
setwd(wd)

load("Day 4/Exercise 7/vanRaalteNepomuceno.RData")

library(tidyverse)
library(DemoDecomp)

source("Day 4/Exercise 7/Functions_D4.R")

# Data extraction -----------------------------------------------------------

data

# Sullivan function from rates vector
Sullivan.fun


start.age <- 65
open.age <- 85

# First year
mx1 <- data %>% 
  filter(year==1970) %>% 
  pull(mx)
wx1 <- data %>% 
  filter(year==1970) %>% 
  pull(wx)
mxwx1 <- c(mx1,wx1)

healthyex1 <- Sullivan.fun(rates=mxwx1)

# Second year
mx2 <- data %>% 
  filter(year==1990) %>% 
  pull(mx)
wx2 <- data %>% 
  filter(year==1990) %>% 
  pull(wx)
mxwx2 <- c(mx2,wx2)

healthyex2 <- Sullivan.fun(rates=mxwx2)

# Difference
(healthyex2 - healthyex1)

# We can compare this difference with the one in period life expectancy, directly from the HMD
filter(data, year==1990,age==65) %>% pull(ex) - filter(data, year==1970,age==65) %>% pull(ex)

# Decomposition -----------------------------------------------------------

# Continuous
results_cont <- horiuchi(
  func=Sullivan.fun,
  pars1 = mxwx1,
  pars2 = mxwx2,
  N=100)

# Stepwise
results_sw <- stepwise_replacement(
  func=Sullivan.fun,
  pars1 = mxwx1,
  pars2 = mxwx2)

# Rearrange the data
HE_cont <- as.data.frame(matrix(c(seq(start.age,open.age,5),results_cont),nrow=(length(results_cont)/2),ncol=3,
                                byrow=F))

HE_sw <- as.data.frame(matrix(c(seq(start.age,open.age,5),results_sw),nrow=(length(results_sw)/2),ncol=3,
                              byrow=F))

colnames(HE_sw) <- colnames(HE_cont) <- c("age","mortality","morbidity")

HE_cont <- HE_cont %>%
  pivot_longer(cols=c("mortality","morbidity"),names_to="type",values_to="contribution" )

HE_sw <- HE_sw %>% 
  pivot_longer(cols=c("mortality","morbidity"),names_to="type",values_to="contribution" )

sum(HE_cont$contribution)

sum(HE_sw$contribution) 

Sullivan.fun(rates=mxwx2)-Sullivan.fun(rates=mxwx1)

# Let's plot the results

# Continuous change decomposition
ggplot(data=HE_cont, aes(x=as.factor(age), y=contribution, fill=type)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_hline(yintercept=0, linetype= "dashed", color = "gray50", linewidth=0.5) +
  scale_x_discrete(labels=c("65-69", "70-74", "75-79", "80-84", "85+")) +
  scale_fill_viridis_d(option="H")

# Stepwise replacement decomposition
ggplot(data=HE_sw, aes(x=as.factor(age), y=contribution, fill=type)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_hline(yintercept=0, linetype= "dashed", color = "gray50", linewidth=0.5) +
  scale_x_discrete(labels=c("65-69", "70-74", "75-79", "80-84", "85+")) +
  scale_fill_viridis_d(option="H")

