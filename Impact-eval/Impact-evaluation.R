## https://lfbeisermcgrath.github.io/rimpactevaluation/causal-inference-and-counterfactuals.html#enrolled-vs.-non-enrolled
# Loading of packages, data, and seed setting here
library(broom)
library(clusterPower)
library(estimatr)
library(fishmethods)
library(haven)
library(kableExtra)
library(MatchIt)
library(modelsummary)
library(pwr)
library(rddensity)
library(skimr)
library(texreg)
library(tidyverse)
library(gt)
library(gtsummary)
library(gtExtras)
library(glm2)
#library(haven)

df <- read_dta("Impact-eval/evaluation.dta")
vars <- read_csv("Impact-eval/variable_desc.csv")

theme_set(ggpubr::theme_pubclean())

# subset data to only "eligible" units
df_elig <- df %>%
  filter(eligible == 1)

m_ba1 <- lm_robust(health_expenditures ~ round, 
                   clusters = locality_identifier,
                   data = df %>% filter(treatment_locality==1 & enrolled ==1))

m_ba2 <- lm_robust(health_expenditures ~ round + age_hh + age_sp + educ_hh + 
                     educ_sp + female_hh + indigenous + hhsize + dirtfloor + 
                     bathroom + land + hospital_distance, 
                   clusters = locality_identifier,
                   data = df %>% filter(treatment_locality==1 & enrolled ==1))
modelsummary(list("No Controls"=m_ba1, "With Controls"=m_ba2),title="Change in Health Expenditures for Households Enrolled in Program")

#htmlreg(list(m_ba1, m_ba2), doctype = FALSE,
#        custom.model.names = c("No Controls", "With Controls"),
#        custom.coef.map = list('round' = "Post-Enrollment",
#                               '(Intercept)' = "Intercept"),
#        caption = "Change in Health Expenditures for Households Enrolled in Program",
#        caption.above = TRUE)


m_ene1 <- lm_robust(health_expenditures ~ enrolled, 
                    clusters = locality_identifier,
                    data = df %>% filter(treatment_locality==1 & round ==1))

m_ene2 <- lm_robust(health_expenditures ~ enrolled + age_hh + age_sp + educ_hh + 
                      educ_sp + female_hh + indigenous + hhsize + dirtfloor + 
                      bathroom + land + hospital_distance, 
                    clusters = locality_identifier,
                    data = df %>% filter(treatment_locality==1 & round ==1))

modelsummary(list("No Controls"=m_ene1, "With Controls"=m_ene2),
             title="Difference in Health Expenditures Between Households Enrolled and Not Enrolled in Program")


## Chapter 3 Randomized Assignment 
df_elig %>%
  filter(round == 0) %>%
  dplyr::select(treatment_locality, locality_identifier,
                age_hh, age_sp, educ_hh, educ_sp, female_hh, indigenous, 
                hhsize, dirtfloor, bathroom, land, hospital_distance) %>%
  tidyr::pivot_longer(-c("treatment_locality","locality_identifier")) %>%
  group_by(name) %>%
  do(tidy(lm_robust(value ~ treatment_locality, data = .))) %>%
  filter(term == "treatment_locality") %>%
  dplyr::select(name, estimate, std.error, p.value) 

out_round0 <- lm_robust(health_expenditures ~ treatment_locality,
                        data = df_elig %>% filter(round == 0),
                        clusters = locality_identifier)
out_round1 <- lm_robust(health_expenditures ~ treatment_locality,
                        data = df_elig %>% filter(round == 1),
                        clusters = locality_identifier)
#htmlreg(list(out_round0, out_round1), doctype = FALSE,
#        custom.coef.map = list(`treatment_locality` = "Treatment Village",
#                               `(Intercept)` = "Intercept"),
#       custom.model.names = c("Baseline", "Follow Up"),
#       caption = "Health Expenditures by Treatment Locality")

modelsummary(list(out_round0,out_round1), title="Health Expenditure by Treatment Locality")


## Re-estimate using a multivariate regression analysis that controls for the other observable 
## characteristics of the sample households. How does your impact estimate change?

out_round1_nocov <- lm_robust(health_expenditures ~ treatment_locality,
                              data = df_elig %>% filter(round == 1),
                              clusters = locality_identifier)
out_round1_wcov <- lm_robust(health_expenditures ~ treatment_locality +
                               age_hh + age_sp + educ_hh + educ_sp + 
                               female_hh + indigenous + hhsize + dirtfloor + 
                               bathroom + land + hospital_distance,
                             data = df_elig %>% filter(round == 1),
                             clusters = locality_identifier)
#htmlreg(list(out_round1_nocov, out_round1_wcov), doctype = FALSE,
#        custom.coef.map = list(`treatment_locality` = "Treatment Village",
#                               `(Intercept)` = "Intercept"),
#        custom.model.names = c("No Covariate Adjust.", "With Covariate Adjust."),
#        caption = "Evaluating HISP: Randomized Assignment with Regression Analysis")

modelsummary(list("No Covariate Adjust."=out_round1_nocov,"With Covariate Adjust."=out_round1_wcov),
             title="Evaluating HISP: Randomized Assignment with Regression Analysis")


## Ch4 Instrumental Variable

m_enroll <- lm_robust(enrolled_rp ~ promotion_locality,
                      clusters = locality_identifier,
                      data = df %>% filter(round == 1))
modelsummary(m_enroll,title="Randomized Promotion Comparison of Enrollment Rate in HISP")
#htmlreg(m_enroll, doctype = FALSE,
#        custom.coef.map = list(`promotion_locality` = "Promotion Locality",
#                               `(Intercept)` = "Intercept"),
#        caption = "Randomized Promotion Comparison of Enrollment Rate in HISP",
#        custom.model.names = "Enrollment Rate")


## Compare baseline healthcare expenditures based upon assignment to promotion.


m_base_health <- lm_robust(health_expenditures ~ promotion_locality,
                           clusters = locality_identifier,
                           data = df %>% filter(round == 0)
)

modelsummary(m_base_health,title="Randomized Promotion Comparison of Mean Household Expenditures at Baseline")
#htmlreg(m_base_health, doctype = FALSE,
#        custom.coef.map = list(`promotion_locality` = "Promotion Locality",
#                               `(Intercept)` = "Intercept"),
#        caption = "Randomized Promotion Comparison of Mean Household Expenditures at Baseline",
#        custom.model.names = "Household Expenditures at Baseline")




#################
# Estimate the difference in health expenditures by assignment to promotion, in the post-treatment period
#################
m_post_health <- lm_robust(health_expenditures ~ promotion_locality,
                           clusters = locality_identifier,
                           data = df %>% filter(round == 1)
)

modelsummary(m_post_health,title="Randomized Promotion Comparison of Mean Household Expenditures at Follow-Up")

#htmlreg(m_post_health, doctype = FALSE,
#        custom.coef.map = list(`promotion_locality` = "Promotion Locality",
#                               `(Intercept)` = "Intercept"),
#        caption = "Randomized Promotion Comparison of Mean Household Expenditures at Follow-Up",
#        custom.model.names = "Household Expenditures at Follow-Up")


###################################
#### Conduct this estimation with and without covariate adjustment. Interpret.
###################################

m_cace <- iv_robust(health_expenditures ~ enrolled_rp |
                      promotion_locality,
                    clusters = locality_identifier,
                    data = df %>% filter(round == 1))
m_cace_wcov <- iv_robust(health_expenditures ~ enrolled_rp + 
                           age_hh + age_sp + educ_hh + educ_sp + 
                           female_hh + indigenous + hhsize + dirtfloor + 
                           bathroom + land + hospital_distance | 
                           promotion_locality + 
                           age_hh + age_sp + educ_hh + educ_sp + 
                           female_hh + indigenous + hhsize + dirtfloor + 
                           bathroom + land + hospital_distance ,
                         clusters = locality_identifier,
                         data = df %>% filter(round == 1))
modelsummary(list('No Covariate Adjustment'=m_cace,'With Covariate Adjustment'=m_cace_wcov),
          title="Evaluating HISP: Randomized Promotion as an Instrumental Variable")

#htmlreg(list(m_cace, m_cace_wcov), doctype = FALSE,
#        custom.coef.map = list(`enrolled_rp` = "Enrollment",
#                               `(Intercept)` = "Intercept"),
#        custom.model.names = c("No Covariate Adjustment", "With Covariate Adjustment"),
#        caption = "Evaluating HISP: Randomized Promotion as an Instrumental Variable")



### Ch 5 Regression Discontinuity Design

# Create data subset with only treatment localities
df_treat <- df %>%
  filter(treatment_locality == 1)

ggplot(df_treat, aes(x = poverty_index)) +
  geom_vline(xintercept = 58) +
  geom_density() +
  labs(x = "Poverty Index")

test_density <- rdplotdensity(rdd = rddensity(df_treat$poverty_index, c = 58), 
                              X = df_treat$poverty_index, 
                              type = "both")
ggplot(df_treat, aes(y = enrolled, x = poverty_index)) +
  geom_vline(xintercept = 58) +
  geom_point() +
  labs(x = "Poverty Index", y = "Enrolled")

df_treat %>%
  filter(round == 1) %>%
  mutate(enrolled_lab = ifelse(enrolled == 1, "Enrolled", "Not Enrolled")) %>%
  ggplot(aes(x = poverty_index, y = health_expenditures,
             group = enrolled_lab, colour = enrolled_lab, fill = enrolled_lab)) +
  geom_point(alpha = 0.03) +
  geom_smooth(method = "lm") +
  labs(x = "Poverty Index", y = "Health Expenditures") +
  scale_colour_viridis_d("Enrollment:", end = 0.7) +
  scale_fill_viridis_d("Enrollment:", end = 0.7) +
  theme(legend.position="bottom")



df_treat <- df_treat %>%
  mutate(poverty_index_c0 = poverty_index - 58)

out_rdd <- lm_robust(health_expenditures ~ poverty_index_c0 * enrolled + 
                       age_hh + age_sp + educ_hh + educ_sp + 
                       female_hh + indigenous + hhsize + dirtfloor + 
                       bathroom + land + hospital_distance,
                     data = df_treat %>% filter(round == 1))

#htmlreg(out_rdd, doctype = FALSE,
#        custom.coef.map = list('enrolled' = "Enrollment"),
#        caption = "Evaluating HISP: Regression Discontinuity Design with Regression Analysis",
#        caption.above = TRUE)

modelsummary(out_rdd,title="Evaluating HISP: Regression Discontinuity Design with Regression Analysis")

out_rdd_cubic <- lm_robust(health_expenditures ~ enrolled * poverty_index_c0 +
                             enrolled * I(poverty_index_c0^2) + 
                             enrolled * I(poverty_index_c0^3) +
                             age_hh + age_sp + educ_hh + educ_sp + 
                             female_hh + indigenous + hhsize + dirtfloor +
                             bathroom + land + hospital_distance,
                           data = df_treat %>% filter(round == 1))


out_rdd5 <- lm_robust(health_expenditures ~ enrolled * poverty_index_c0 + 
                        age_hh + age_sp + educ_hh + educ_sp + 
                        female_hh + indigenous + hhsize + dirtfloor + 
                        bathroom + land + hospital_distance,
                      data = df_treat %>% filter(round == 1 &
                                                   abs(poverty_index_c0) <=5))

modelsummary(list("Linear"=out_rdd,"Cubic"= out_rdd_cubic,"5 Point Window"=out_rdd5), title="Evaluating HISP: Regression Discontinuity Design with Regression Analysis")


## htmlreg(list(out_rdd, out_rdd_cubic, out_rdd5), 
#doctype = FALSE,
#custom.model.names = c("Linear", "Cubic", "5 Point Window"),
#custom.coef.map = list('enrolled' = "Enrollment"),
#caption = "Evaluating HISP: Regression Discontinuity Design with Regression Analysis",
#caption.above = TRUE)


## Ch6 Difference in Differences 
out_did <- lm_robust(health_expenditures ~ round * enrolled, 
                     data = df %>% filter(treatment_locality == 1),
                     clusters = locality_identifier)

out_did_wcov <- lm_robust(health_expenditures ~ round * enrolled +
                            age_hh + age_sp + educ_hh + educ_sp + 
                            female_hh + indigenous + hhsize + dirtfloor + 
                            bathroom + land + hospital_distance, 
                          data = df %>% filter(treatment_locality == 1),
                          clusters = locality_identifier)

modelsummary(list("No Covariate Adjustment"=out_did,"With Covariance Adjustment"=out_did_wcov),
             title="Evaluating HISP: Difference-in-Differences with Regression")
#htmlreg(list(out_did, out_did_wcov), doctype = FALSE,
#        custom.coef.map = list('enrolled' = "Enrollment",
#                               'round' = "Round",
#                               'round:enrolled' = "Enrollment X Round"),
#        caption = "Evaluating HISP: Difference-in-Differences with Regression",
#        caption.above = TRUE,
#        custom.model.names = c("No Covariate Adjustment", "With Covariate Adjustment"))


## Ch7 Matching

df_w <- df %>%
  pivot_wider(names_from = round, # variable that determines new columns
              # variables that should be made "wide"
              values_from = c(health_expenditures, 
                              poverty_index, age_hh, age_sp,
                              educ_hh, educ_sp, female_hh,
                              indigenous, hhsize, dirtfloor,
                              bathroom, land, hospital_distance,
                              hospital)) %>%
  # remove the household that has missing values
  # as missing values are not allowed when using matchit
  filter(!is.na(health_expenditures_0)) 

## Propensity Score Matching
# We'll conduct propensity score estimation and matching at the same
# time, as the matchit function does this for us
psm_r <- matchit(enrolled ~ age_hh_0 + educ_hh_0,
                 data = df_w %>% dplyr::select(-hospital_0, -hospital_1), 
                 distance = "glm")
psm_ur <- matchit(enrolled ~ age_hh_0 + educ_hh_0 + age_sp_0 + educ_sp_0 +
                    female_hh_0 + indigenous_0 + hhsize_0 + dirtfloor_0 +
                    bathroom_0 + land_0 + hospital_distance_0,
                  data = df_w %>% dplyr::select(-hospital_0, -hospital_1), 
                  distance = "glm")
modelsummary(list("Limited Set"=psm_r,"Full Set"=psm_ur),
             title="Estimating the Propensity Score Based on Baseline Observed Characteristics")

#htmlreg(list(psm_r$model, psm_ur$model),
#        doctype = FALSE,
 #       custom.coef.map = list('age_hh_0' = "Age (HH) at Baseline",
  #                             'educ_hh_0' = "Education (HH) at Baseline",
   #                            'age_sp_0' = "Age (Spouse) at Baseline",
    #                           'educ_sp_0' = "Education (Spouse) at Baseline",
     #                          'female_hh_0' = "Female Head of Household (HH) at Baseline",
      #                         'indigenous_0' = "Indigenous Language Spoken at Baseline",
       #                        'hhsize_0' = "Number of Household Members at Baseline",
        #                       'dirtfloor_0' = "Dirt floor at Baseline",
         #                      'bathroom_0' = "Private Bathroom at Baseline",
          #                     'land_0' = "Hectares of Land at Baseline",
           #                    'hospital_distance_0' = "Distance From Hospital at Baseline"),
        #caption = "Estimating the Propensity Score Based on Baseline Observed Characteristics",
        #custom.model.names = c("Limited Set", "Full Set"))


df_w <- df_w %>%
  mutate(ps_ur = psm_ur$model$fitted.values)

df_w %>%
  mutate(enrolled_lab = ifelse(enrolled == 1, "Enrolled", "Not Enrolled")) %>%
  ggplot(aes(x = ps_ur,
             group = enrolled_lab, colour = enrolled_lab, fill = enrolled_lab)) +
  geom_density(alpha = I(0.2)) +
  xlab("Propensity Score") +
  scale_fill_viridis_d("Status:", end = 0.7) +
  scale_colour_viridis_d("Status:", end = 0.7) +
  theme(legend.position = "bottom")

kable(summary(psm_ur)$sum.all,
      caption = "Balance Before Matching") %>%
  kable_styling()

kable(summary(psm_ur)$sum.matched,
      caption = "Balance After Matching") %>%
  kable_styling()

match_df_r <- match.data(psm_r)
match_df_ur <- match.data(psm_ur)

out_lm_r <- lm_robust(health_expenditures_1 ~ enrolled,
                      data = match_df_r, clusters = locality_identifier,
                      weights = weights)
out_lm_ur <- lm_robust(health_expenditures_1 ~ enrolled,
                       data = match_df_ur, clusters = locality_identifier,
                       weights = weights)
modelsummary(list(out_lm_r,out_lm_ur),
             title="Evaluating HISP: Matching on Baseline Characteristics and Regression Analysis")

#htmlreg(list(out_lm_r, out_lm_ur),
#        doctype = FALSE,
#        caption = "Evaluating HISP: Matching on Baseline Characteristics and Regression Analysis",
#        custom.model.names = c("Limited Set", "Full Set"))

# limited set of covariates
df_long_match_r <- df %>%
  left_join(match_df_r %>% dplyr::select(household_identifier, weights)) %>%
  filter(!is.na(weights))

# full set of covaraites
df_long_match_ur <- df %>%
  left_join(match_df_ur %>% dplyr::select(household_identifier, weights)) %>%
  filter(!is.na(weights))

# Now estimate did 
did_reg_r <- lm_robust(health_expenditures ~ enrolled * round,
                       data = df_long_match_r, weights = weights,
                       clusters = locality_identifier)

did_reg_ur <- lm_robust(health_expenditures ~ enrolled * round,
                        data = df_long_match_ur, weights = weights,
                        clusters = locality_identifier)

modelsummary(list(did_reg_r,did_reg_ur),
             title = "Evaluating HISP: Difference-in-Differences Regression Combined With Matching")

#htmlreg(list(did_reg_r, did_reg_ur), doctype = FALSE,
#        custom.coef.map = list('enrolled' = "Enrollment",
#                               'round' = "Round",
#                               'enrolled:round' = "Enrollment X Round"),
#        caption = "Evaluating HISP: Difference-in-Differences Regression Combined With Matching",
#        custom.model.names = c("Limited Set", "Full Set"),
#        caption.above = TRUE)

# mean and standard deviation of outcome
sumstats <- df_elig %>%
  filter(round == 1 & treatment_locality == 1) %>%
  summarise(mean_health = mean(health_expenditures),
            sd_health = sd(health_expenditures),
            mean_hospital = mean(hospital),
            sd_hospital = sd(hospital))
power_calc_health <- tibble(d_health_expenditures = rep(-1:-3,2),
                            power = rep(c(0.8, 0.9), each = 3)) %>%
  mutate(n_required = map2(d_health_expenditures, power,
                           ~ {pwr.t.test(d = .x / sumstats$sd_health,
                                         sig.level = 0.05, power = .y)$n}) %>%
           unlist() %>%
           ceiling())  

power_calc_health %>%
  filter(power == 0.9) %>%
  mutate(mde = gsub("-", "$", d_health_expenditures)) %>%
  select(mde, power, n_required) %>%
  kable(align = "c", 
        col.names = c("Minimum Detectable Effect", "Power", 
                      "Sample Required per Group"),
        caption = "Evaluating HISP+: Sample Size Required to Detect Various Minimum Detectable Effects, Power = 0.9") %>%
  kable_styling(full_width = TRUE)

power_calc_health %>%
  filter(power == 0.8) %>%
  mutate(mde = gsub("-", "$", d_health_expenditures)) %>%
  select(mde, power, n_required) %>%
  kable(align = "c", 
        col.names = c("Minimum Detectable Effect", "Power", 
                      "Sample Required per Group"),
        caption = "Evaluating HISP+: Sample Size Required to Detect Various Minimum Detectable Effects, Power = 0.8") %>%
  kable_styling(full_width = TRUE)


power_calc_hospital <- tibble(d_hospital = rep(c(-.01,-.02, -.03),2),
                              power = rep(c(0.8, 0.9), each = 3)) %>% 
  mutate(n_required = map2(d_hospital, power,
                           ~ {pwr.t.test(d = .x / sumstats$sd_hospital,
                                         sig.level = 0.05, power = .y)$n}) %>%
           unlist() %>%
           ceiling ()) 


power_calc_hospital %>%
  filter(power == 0.8) %>%
  mutate(mde = gsub("-", "", d_hospital * 100)) %>%
  select(mde, power, n_required) %>%
  kable(align = "c", 
        col.names = c("Minimum Detectable Effect (%)", "Power", 
                      "Sample Required per Group"),
        caption = "Evaluating HISP+: Sample Size Required to Detect Various Minimum Desired Effects (Increase in Hospitalization Rate)") %>%
  kable_styling(full_width = TRUE)


# intraclass correlation, for cluster calculations
df_elig_t1r1 <- df_elig %>%
  filter(round == 1 & treatment_locality == 1) %>%
  select(health_expenditures, locality_identifier)

icc_est <- clus.rho(df_elig_t1r1$health_expenditures, 
                    df_elig_t1r1$locality_identifier, 
                    type = 3)$icc
icc_est


# calculating all at once
power_clstr_calc_health <- tibble(d_health_expenditures = -1:-3) %>%
  mutate(n_required = map(d_health_expenditures,
                          ~ {crtpwr.2mean(d = .x, m = 50,
                                          alpha = 0.05, power = 0.8,
                                          cv = 0, icc = icc_est,
                                          varw = sumstats$sd_health^2)}) %>%
           unlist() %>%
           ceiling()) 

power_clstr_calc_health %>%
  mutate(mde = gsub("-", "$", d_health_expenditures),
         total_clusters = 50 * 2,
         total_sample = total_clusters * n_required) %>%
  select(mde, total_clusters, n_required, total_sample) %>%
  kable(col.names = c("Minimum Detectable Effect", "Number of Clusters", 
                      "Units per Cluster", "Total Observations"),
        align = "c") %>%
  kable_styling(full_width = TRUE)

