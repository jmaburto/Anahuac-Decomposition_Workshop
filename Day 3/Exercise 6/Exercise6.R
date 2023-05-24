# Setup -------------------------------------------------------------------

library(here)
wd <- here()
setwd(wd)

load("Day 3/Exercise 6/AburtoBeltranSanchez.RData")

library(tidyverse)
library(DemoDecomp)

source("Day 3/Exercise 6/Functions_D3.R")

# Extract the needed information ------------------------------------------

#we need age- and cause-specific mortality rates in each period
COD1 <- data %>% 
  filter(year==2005, age>15) %>% 
  select(Cause_1:Cause_9) %>% 
  as.matrix()

COD2 <- data %>% 
  filter(year==2015, age>15) %>% 
  select(Cause_1:Cause_9) %>% 
  as.matrix()


# Linear integral method --------------------------------------------------

# Let's have a look at the lifespan variation function
edagger.frommxc

# Let's decompose the lifespan variation change
results <- horiuchi(func = edagger.frommxc, pars1 = c(COD1), pars2 = c(COD2), N = 100)

#Go back to a matrix
dim(results) <- dim(COD1)

# Check the results
#original
(original <- edagger.frommxc(COD2) - edagger.frommxc(COD1))
#with decomp
(with_decomp <- sum(results))
#error
with_decomp - original

# Rearrange the results
results <- data.frame(results)
colnames(results) <- cause_names

results <- results %>% 
  mutate(age = 16:109) %>% 
  pivot_longer(cols=1:9, names_to="cause", values_to="contribution")

# And plot them

ggplot(results, aes(x=age, y=contribution, fill=cause)) +
  ggtitle(bquote(~'Change in '~ e[15]^"\u2020" ~'2005-2015' )) +
  geom_bar(stat = "identity", position = "stack")

results %>% 
  mutate(age_group = case_when(age %in% 16:19 ~ "16-19",
                               age %in% 20:24 ~ "20-24",
                               age %in% 25:29 ~ "25-29",
                               age %in% 30:34 ~ "30-34",
                               age %in% 35:39 ~ "35-39",
                               age %in% 40:44 ~ "40-44",
                               age %in% 45:49 ~ "45-49",
                               age %in% 50:54 ~ "50-54",
                               age %in% 55:59 ~ "55-59",
                               age %in% 60:64 ~ "60-64",
                               age %in% 65:69 ~ "65-69",
                               age %in% 70:74 ~ "70-74",
                               age %in% 75:79 ~ "75-79",
                               age %in% 80:84 ~ "80-84",
                               age %in% 85:89 ~ "85-89",
                               age %in% 90:94 ~ "90-94",
                               age %in% 95:99 ~ "95-99",
                               age %in% 100:104 ~ "100-104",
                               age %in% 105:109 ~ "105-109"),
         age_group = factor(age_group, levels = c("16-19",
                                                  "20-24",
                                                  "25-29",
                                                  "30-34",
                                                  "35-39",
                                                  "40-44",
                                                  "45-49",
                                                  "50-54",
                                                  "55-59",
                                                  "60-64",
                                                  "65-69",
                                                  "70-74",
                                                  "75-79",
                                                  "80-84",
                                                  "85-89",
                                                  "90-94",
                                                  "95-99",
                                                  "100-104",
                                                  "105-109"))) %>% 
  group_by(age_group, cause) %>% 
  mutate(contribution = sum(contribution)) %>% 
ggplot(aes(x=age_group, y=contribution, fill=cause)) +
  ggtitle(bquote(~'Change in '~ e[15]^"\u2020" ~'2005-2015' )) +
  geom_bar(stat = "identity", position = "stack")


# Stepwise replacement method ---------------------------------------------

results_step <- stepwise_replacement(func = edagger.frommxc, pars1 = c(COD1), pars2 = c(COD2))

#Go back to a matrix
dim(results_step) <- dim(COD1)

# Check the results
#original
(original <- edagger.frommxc(COD2) - edagger.frommxc(COD1))
#with decomp
(with_decomp <- sum(results_step))
#error
with_decomp - original

# Rearrange and plot the results
results_step <- data.frame(results_step)
colnames(results_step) <- cause_names
results_step <- results_step %>% 
  mutate(age = 16:109) %>% 
  pivot_longer(cols=1:9, names_to="cause", values_to="contribution")

ggplot(data=results_step, aes(x=age, y=contribution, fill=cause)) +
  ggtitle(bquote(~'Change in '~ e[15]^"\u2020" ~'2005-2015' )) +
  geom_bar(stat = "identity", position = "stack")

results_step %>% 
  mutate(age_group = case_when(age %in% 16:19 ~ "16-19",
                               age %in% 20:24 ~ "20-24",
                               age %in% 25:29 ~ "25-29",
                               age %in% 30:34 ~ "30-34",
                               age %in% 35:39 ~ "35-39",
                               age %in% 40:44 ~ "40-44",
                               age %in% 45:49 ~ "45-49",
                               age %in% 50:54 ~ "50-54",
                               age %in% 55:59 ~ "55-59",
                               age %in% 60:64 ~ "60-64",
                               age %in% 65:69 ~ "65-69",
                               age %in% 70:74 ~ "70-74",
                               age %in% 75:79 ~ "75-79",
                               age %in% 80:84 ~ "80-84",
                               age %in% 85:89 ~ "85-89",
                               age %in% 90:94 ~ "90-94",
                               age %in% 95:99 ~ "95-99",
                               age %in% 100:104 ~ "100-104",
                               age %in% 105:109 ~ "105-109"),
         age_group = factor(age_group, levels = c("16-19",
                                                  "20-24",
                                                  "25-29",
                                                  "30-34",
                                                  "35-39",
                                                  "40-44",
                                                  "45-49",
                                                  "50-54",
                                                  "55-59",
                                                  "60-64",
                                                  "65-69",
                                                  "70-74",
                                                  "75-79",
                                                  "80-84",
                                                  "85-89",
                                                  "90-94",
                                                  "95-99",
                                                  "100-104",
                                                  "105-109"))) %>% 
  group_by(age_group, cause) %>% 
  mutate(contribution = sum(contribution)) %>% 
  ggplot(aes(x=age_group, y=contribution, fill=cause)) +
  ggtitle(bquote(~'Change in '~ e[15]^"\u2020" ~'2005-2015' )) +
  geom_bar(stat = "identity", position = "stack")

# Let's compare the two methods
results$contribution - results_step$contribution

