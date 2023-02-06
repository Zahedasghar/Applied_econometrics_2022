
# Add names of group members HERE
library(tidyverse)
library(wooldridge)
library(broom)
library(magrittr)
library(stargazer)
library(clubSandwich)

df <- as_tibble(injury)
df %<>% filter(ky==1)

df %>% group_by(afchnge,highearn) %>% summarize(mean.ldurat = mean(ldurat))

est.did <- lm(ldurat ~ afchnge*highearn, data=df)

#' -   `male`
#' -   `married`
#' -   Quadratic in `age`
#' -   `hosp` (1 = hospitalized)
#' -   `indust` (1 = manuf, 2 = construc, 3 = other)
#' -   `injtype` (1-8; categories for different types of injury)
#' -   `lprewage` (log of wage prior to filing a claim)
#' 
library(modelsummary)
modelsummary(list(est.did,est.did.x),estimate = "{estimate}{stars}",
             gof_omit = ".*")

