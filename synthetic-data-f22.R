library(tidyverse)
library(scales)
library(truncnorm)
library(broom)

# Seeds
# https://evalf22.classes.andrewheiss.com/example/random-numbers.html#seeds
set.seed(1234)

rnorm(10)

sample(1:6, 4, replace = TRUE)
sample(1:6, 4, replace = TRUE)



# Distributions and random numbers
# https://evalf22.classes.andrewheiss.com/example/random-numbers.html
set.seed(1234)
n_people <- 3000
fake_people <- tibble(
  person_id = 1:n_people
) %>% 
  mutate(income = rnorm(n_people, mean = 200, sd = 100)) %>% 
  mutate(income = round(income, 0)) %>% 
  mutate(state = sample(c("Mississippi", "Alabama", "Georgia"),
                        n_people, 
                        replace = TRUE,
                        prob = c(0.2, 0.2, 0.6))) %>% 
  mutate(treatment = sample(c("Control", "Treatment"),
                            n_people,
                            replace = TRUE,
                            prob = c(2/3, 1/3))) %>% 
  mutate(income_uniform = runif(n_people, 500, 700)) %>% 
  mutate(income = ifelse(income < 0, 10, income),
         income_fixed = rtruncnorm(n_people, mean = 100, sd = 100, a = 0, b = 300))

runif(10, min = 5, max = 10)
sample(400:800, 10, replace = TRUE)



fake_people %>% 
  summarize(avg_income = mean(income))

ggplot(fake_people,
       aes(x = income)) +
  geom_density()

ggplot(fake_people, aes(x = state)) +
  geom_bar()



## Histograms, density plots, and bar charts


## Uniform (sample and runif)
## Normal
## Truncated normal
## Beta
## Binomial
## Poisson


# Rescaling things


# Connecting columns
## Draw a DAG
## Baseline value + effect from incoming nodes + noise

n_cookie_people <- 2001

cookie_data <- tibble(id = 1:n_cookie_people) %>% 
  # Exogneous things
  mutate(likes_blue = sample(c(TRUE, FALSE), n_cookie_people, 
                             replace = TRUE, prob = c(0.3, 0.7))) %>% 
  mutate(cookie_baseline = rtruncnorm(n_cookie_people, 
                                      mean = 3, sd = 1, a = 0),
         cookie_blue_boost = rnorm(n_cookie_people, 
                                   mean = 1.5, sd = 0.25) * likes_blue,
         cookies = cookie_baseline + cookie_blue_boost) %>% 
  mutate(net_prob_baseline = 0.5,
         like_blue_effect = -0.2,
         net_prob = baseline + effects,
         net = sample(c("Yes", "No"), n_people, prob = c(net_prob, 1 - net_prob)))  
  mutate(happiness_baseline = rnorm(n_cookie_people, 50, 15),
         happiness_blue_boost = 5 * likes_blue,
         happiness_cookie_boost = cookies * 5,
         happiness = happiness_baseline + happiness_blue_boost + 
           happiness_cookie_boost)

ggplot(cookie_data, aes(x = cookies, y = happiness)) +
  geom_point() +
  geom_smooth(method = "lm")

test_model <- lm(happiness ~ cookies + likes_blue, data = cookie_data)
tidy(test_model)

ggplot(cookie_data, aes(x = happiness_baseline)) +
  geom_density()

ggplot(cookie_data, aes(x = likes_blue)) +
  geom_bar()

ggplot(cookie_data, aes(x = likes_blue, y = cookies)) +
  geom_boxplot()

test_model <- lm(cookies ~ likes_blue, data = cookie_data)
tidy(test_model)

library(forcats)

