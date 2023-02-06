install.packages("dplyr")

library(tidyverse)
library(haven)
minwage <- read_dta("minwage.dta")
library(vtable)
vtable(minwage)
glimpse(minwage)
minwage <- minwage %>% filter(sample == 1) %>%
  rename(treat = state) %>%
  mutate(state = case_when(treat == 0 ~ 'PA',
                           treat == 1 ~ 'NJ'),
         low_wage = 1 * (wage_st < 5))
View(minwage)
# 2 - Baseline Diff-in-Diff: starting wages
DinD_wage <- minwage %>% group_by(state) %>%
  summarize(mean_wage_st = mean(wage_st),
            mean_wage_st2 = mean(wage_st2)) %>%
  mutate(diff = mean_wage_st2 - mean_wage_st)
DinD_wage



with(DinD_wage, diff[1] - diff[2])
# 3 - Baseline Diff-in-Diff: full-time equivalent employment
DinD_emp <- minwage %>% group_by(state) %>%
  summarize(mean_fte = mean(fte),
            mean_fte2 = mean(fte2)) %>%
  mutate(diff = mean_fte2 - mean_fte)
DinD_emp
with(DinD_emp, diff[1] - diff[2])

# 4 - Reshape dataset for Diff-in-Diff regression estimation
wave1 <- minwage %>%
  select(state, treat, wage_st, fte, chain, co_owned, low_wage) %>%
  mutate(post = 0)
wave2 <- minwage %>%
  select(state, treat, wage_st2, fte2, chain, co_owned, low_wage) %>%
mutate(post = 1) %>%
  rename(wage_st = wage_st2, fte = fte2)
both_waves <- bind_rows(wave1, wave2)

##Solution to 5(a)
###Blah blah blah. . .
# 5 - Diff-in-Diff regression results
library(stargazer)
reg_wage1 <- lm(wage_st ~ treat + post + treat:post, both_waves)
reg_emp1 <- lm(fte ~ treat + post + treat:post, both_waves)
stargazer(reg_wage1, reg_emp1, type = 'text', header = FALSE, digits = 2,
          dep.var.labels = c('Starting Wage', 'Full-time Equiv. Employment'),
          omit.stat = c('f', 'ser', 'adj.rsq', 'rsq'))
