library(tidyverse); library(modelsummary)
gm <- causaldata::gapminder

gm <- gm %>%
  # Put GDP per capita in log format since it's very skewed
  mutate(log_GDPperCap = log(gdpPercap)) %>%
  # Perform each calculation by group
  group_by(country) %>%
  # Get within variation by subtracting out the mean
  mutate(lifeExp_within = lifeExp - mean(lifeExp),
         log_GDPperCap_within = log_GDPperCap - mean(log_GDPperCap)) %>%
  # We no longer need the grouping
  ungroup()

# Analyze the within variation
m1 <- lm(lifeExp_within ~ log_GDPperCap_within, data = gm)
msummary(m1, stars = c('*' = .1, '**' = .05, '***' = .01))