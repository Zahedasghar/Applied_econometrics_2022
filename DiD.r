library(tidyverse)
library(ggthemes)
library(parallel)

set.seed(256)
library(gganimate)
library(broom)
library(readxl)
hope<-read_excel("HOPE.xlsx")
View(hope)

DND_reg <- lm(InCollege ~ Georgia + After + Georgia*After, data = hope)
DND_reg %>% tidy()

#' ]
#' 
#' --
#' .smallest[
#' $$\widehat{\text{Enrolled}_{it}}=0.406-0.105 \, \text{Georgia}_{i}-0.004 \, \text{After}_{t}+0.089 \, (\text{Georgia}_{i} \times \text{After}_{t})$$
#' ]
#' 
#' ---
#' 
#' # Example: Interpretting the Regression
#' 
#' $$\widehat{\text{Enrolled}_{it}}=0.406-0.105 \, \text{Georgia}_{i}-0.004 \, \text{After}_{t}+0.089 \, (\text{Georgia}_{i} \times \text{After}_{t})$$
#' 
#' --
#' 
#' - $\beta_0$: 
#' --
#' A **non-Georgian** **before** 1992 was 40.6% likely to be a college student
#' 
#' --
#' 
#' - $\beta_1$: 
#' --
#' **Georgians** **before** 1992 were 10.5% less likely to be college students than neighboring states
#' 
#' --
#' 
#' - $\beta_2$: 
#' --
#' **After** 1992, **non-Georgians** are 0.4% less likely to be college students
#' 
#' --
#' 
#' - $\beta_3$: 
#' --
#' **After** 1992, **Georgians** are 8.9% more likely to enroll in colleges than neighboring states
#' 
#' --
#' 
#' - .hi-purple[Treatment effect: HOPE increased enrollment likelihood by 8.9%]
#' 
#' ---
#' 
#' # Example: Comparing Group Means
#' 
#' $$\widehat{\text{Enrolled}_{it}}=0.406-0.105 \, \text{Georgia}_{i}-0.004 \, \text{After}_{t}+0.089 \, (\text{Georgia}_{i} \times \text{After}_{t})$$
#' 
#' - A group mean for a dummy $Y$ is $E[Y=1]$, i.e. the probability a student is enrolled:
#' 
#' - **Non-Georgian enrollment probability pre-1992**: 
#' --
#' $\beta_0=0.406$
#' 
#' --
#' - **Georgian enrollment probability pre-1992**: 
#' --
#' $\beta_0+\beta_1=0.406-0.105=0.301$
#' 
#' --
#' 
#' - **Non-Georgian enrollment probability post-1992**: 
#' --
#' $\beta_0+\beta_2=0.406-0.004=0.402$
#' 
#' --
#' 
#' - **Georgian enrollment probability post-1992**: 
#' --
#' $\beta_0+\beta_1+\beta_2+\beta_3=0.406-0.105-0.004+0.089=0.386$
#' 
#' ---
#' 
#' # Example: Comparing Group Means in R
#' 
#' .pull-left[
## ---- echo=T-------------------------------------------
# group mean for non-Georgian before 1992
hope %>%
  filter(Georgia == 0,
         After == 0) %>%
  summarize(prob = mean(InCollege))


# group mean for non-Georgian AFTER 1992
hope %>%
  filter(Georgia == 0,
         After == 1) %>%
  summarize(prob = mean(InCollege))


hope %>%
  filter(Georgia == 1,
         After == 0) %>%
  summarize(prob = mean(InCollege))

hope %>%
  filter(Georgia == 1,
         After == 1) %>%
  summarize(prob = mean(InCollege))

plot<-hope %>%
  ggplot(data=.)+
  aes(x = After,
      y = InCollege,
      color = factor(Georgia))+
  geom_smooth(method="lm", se=FALSE, size = 2)+
  #scale_y_continuous(breaks=seq(0,1,0.125), limits=c(0,0.05))+
  #geom_abline(intercept = 0.301, slope = -0.004, linetype = "dashed", color = "gray",
  #            xlim = c(0,1))+
  scale_x_continuous(breaks=seq(0,1,1), labels=c("Before", "After"))+
scale_color_manual(name = "State",
                   labels = c("Neighbors", "Georgia"), values = c("blue", "red"))+
  labs(x = "Before or After HOPE",
       y = "Probability of Being Enrolled in College")+
  theme_classic(base_family = "Fira Sans Condensed",
           base_size=16)+
  #scale_y_continuous(limits = c(0.2,0.6),
  #                   expand = c(0,0))+
  theme(legend.position = "top")
plot

plot+ geom_segment(x = 0, xend = 1, y = 0.301, yend = 0.297, linetype = "dashed", color = "gray", size = 2)


## ----dnd-gen-reg, echo=T-------------------------------
DND_fe <- lm(InCollege ~ Georgia*After + factor(StateCode) + factor(Year),
           data = hope)
DND_fe %>% tidy()

library(fixest)
DND_fe_2 <- feols(InCollege ~ Georgia*After | factor(StateCode) + factor(Year),
           data = hope)
DND_fe_2 %>% tidy()


DND_fe_controls <- lm(InCollege ~ Georgia*After + factor(StateCode) + factor(Year) + Black + LowIncome,
           data = hope)
DND_fe_controls %>% tidy()

library(fixest)
DND_fe_controls_2 <- feols(InCollege ~ Georgia*After + Black + LowIncome | factor(StateCode) + factor(Year),
           data = hope)
DND_fe_controls_2 %>% tidy()

huxtable::huxreg(DND_reg,
                 DND_fe_2,
                 DND_fe_controls_2,
                 coefs = c("Intercept" = "(Intercept)",
                 "Georgia" = "Georgia",
                 "After" = "After",
                 "Georgia x After" = "Georgia:After",
                 "Low Income" = "LowIncome",
                 "Black" = "Black"),
       statistics = c("N" = "nobs",
                      "R-Squared" = "r.squared",
                      "SER" = "sigma"),
       number_format = 4) %>%
  huxtable::add_rows(c("Fixed Effects", "None", "State & Year", "State & Year"), # add fixed effects row
         after = 13) # insert after 6th row

