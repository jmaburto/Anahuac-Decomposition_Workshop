# Setup -------------------------------------------------------------------
library(here)
wd <- here()
setwd(wd)

load("Day 2/Exercise 4/Arriaga_COVID_USA.RData")

library(tidyverse)
library(data.table)

# Compare life expectancies  ----------------------------------------------

data %>% filter(year==2020,age==0,sex==0) %>% pull(ex) %>% unique() - 
  data %>% filter(year==2019,age==0,sex==0) %>% pull(ex) %>% unique()

# Decomposition -----------------------------------------------------------

# Age

l1 <- data %>%
  filter(year==2019,sex==0, cause=="cvd") %>% 
  pull(lx)
l2 <- data %>%
  filter(year==2020,sex==0, cause=="cvd") %>% 
  pull(lx)
d1 <- data %>%
  filter(year==2019,sex==0, cause=="cvd") %>% 
  pull(dx)
d2 <- data %>%
  filter(year==2020,sex==0, cause=="cvd") %>% 
  pull(dx)
L1 <- data %>%
  filter(year==2019,sex==0, cause=="cvd") %>% 
  pull(Lx)
L2 <- data %>%
  filter(year==2020,sex==0, cause=="cvd") %>% 
  pull(Lx)
T1 <- data %>%
  filter(year==2019,sex==0, cause=="cvd") %>% 
  pull(Tx)
T2 <- data %>%
  filter(year==2020,sex==0, cause=="cvd") %>% 
  pull(Tx)

LAG <- length(l1)

# Direct effect
DE <- (l1/l1[1])*((L2/l2)-(L1/l1))
# Indirect and interaction effects
IE <- (T2[-1]/l1[1])*((l1[-LAG]/l2[-LAG])-(l1[-1]/l2[-1]))
# one extra value for the indirect component
# since there is only direct component in the last age group
IE <- c(IE,0)

## add both to get the overall age-decomposition
ALL_age <- DE+IE
# check
# difference in life expectancies
data %>% filter(year==2020,sex==0,age==0) %>% pull(ex) %>% unique() - 
  data %>% filter(year==2019,sex==0,age==0) %>% pull(ex) %>% unique()

# sum of age-specific effects
sum(ALL_age)

ggplot() +
  ggtitle(bquote("Change in male"~"e"[0]~", USA 2019-2020")) +
  geom_bar(aes(x = unique(data$age), y = (ALL_age)), stat = "identity") +
  scale_x_continuous("Age") +
  scale_y_continuous("Contribution")

# Age and cause

mx1 <- data %>% filter(year==2019,sex==0) %>% pull(mx)
mx2 <- data %>% filter(year==2020,sex==0) %>% pull(mx)
rxi1 <- data %>% filter(year==2019,sex==0) %>% pull(prop)
rxi2 <- data %>% filter(year==2020,sex==0) %>% pull(prop)
causes <- data %>% filter(year==2020,sex==0) %>% pull(cause)

ALL_cause <- ((rxi2*mx2-rxi1*mx1)/(mx2-mx1)) %>% 
  cbind(age=0:100,
        cause = causes) %>%
  as.data.table() %>% 
  mutate(cause = factor(cause, 
                        labels=levels(causes))) %>% 
  rename(cause_multiplier = ".") %>%
  inner_join(ALL_age %>% 
               cbind(age=0:100) %>% 
               as.data.table() %>% 
               rename(C = "."), by="age") %>%
  mutate(C_cause = cause_multiplier*C)


sum(ALL_cause$C_cause)
sum(ALL_cause$C)/8
sum(ALL_age)

ALL_cause %>% 
ggplot() +
  ggtitle(bquote("Change in male"~"e"[0]~", USA 2019-2020")) +
  geom_bar(aes(x = age, y = (C_cause), fill=as.factor(cause)), stat="identity", position = "stack") +
  scale_x_continuous("Age") +
  scale_y_continuous("Contribution") +
  coord_flip()

# Correction --------------------------------------------------------------

ALL_cause_correct <- data %>% 
  filter(year==2019, sex==0) %>% 
  inner_join(data %>% 
               filter(year==2020, sex==0), by=c("age","cause"), suffix=c("_2019","_2020")) %>% 
  mutate(cause_multiplier = (prop_2020*mx_2020-prop_2019*mx_2019)/(mx_2020-mx_2019)) %>% 
  select(age, cause, cause_multiplier) %>% 
  inner_join(ALL_age %>% 
               cbind(age=0:100) %>% 
               as.data.table() %>% 
               rename(C = "."), by="age") %>%
  mutate(C_cause = cause_multiplier*C)

sum(ALL_cause_correct$C_cause)
sum(ALL_cause_correct$C)/8
sum(ALL_age)

ALL_cause_correct %>% 
  ggplot() +
  ggtitle(bquote("Change in male"~"e"[0]~", USA 2019-2020")) +
  geom_bar(aes(x = age, y = (C_cause), fill=as.factor(cause)), stat="identity", position = "stack") +
  scale_x_continuous("Age") +
  scale_y_continuous("Contribution") +
  coord_flip()


ALL_cause_correct %>% 
  mutate(age_group = case_when(age %in% 0:4 ~ "0-4",
                               age %in% 5:9 ~ "5-9",
                               age %in% 10:14 ~ "10-14",
                               age %in% 15:19 ~ "15-19",
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
                               age %in% 100 ~ "100+"),
         age_group = factor(age_group, levels = c("0-4",
                                                  "5-9",
                                                  "10-14",
                                                  "15-19",
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
                                                  "100+"))) %>% 
  group_by(age_group, cause) %>% 
  mutate(C_cause_group = sum(C_cause)) %>% 
  ggplot() +
  ggtitle(bquote("Change in male"~"e"[0]~", USA 2019-2020")) +
  geom_bar(aes(x = age_group, y = (C_cause_group), fill=as.factor(cause)), stat="identity", position = "stack") +
  scale_x_discrete("Age") +
  scale_y_continuous("Contribution") +
  coord_flip()
