library(tidyverse); library(modelsummary)
#install.packages("causaldata")
library(causaldata)
library(vtable)

#data("Mroz")
#glimpse(Mroz)
vtable(Mroz)

df <- causaldata::Mroz %>%
  # Keep just working women
  dplyr::filter(lfp == TRUE) %>%
  # Get unlogged earnings %>%
  mutate(earn = exp(lwg))

# 1. Draw a scatterplot
ggplot(df, aes(x = inc, y = earn)) + 
  geom_point() +
  # Use a log scale for both axes
  # We'll get warnings as it drops the 0s, that's ok
  scale_x_log10() + scale_y_log10()

# 2. Get the conditional mean by college attendance
df %>%
  # wc is the college variable
  group_by(wc) %>%
  # Functions besides mean could be used here to get other conditionals
  summarize(earn = mean(earn))

# 3. Get the conditional mean by bins
df %>%
  # use cut() to cut the variable into 10 bins
  mutate(inc_cut = cut(inc, 10)) %>%
  group_by(inc_cut) %>%
  summarize(earn = mean(earn))

# 4. Draw the LOESS and linear regression curves
ggplot(df, aes(x = inc, y = earn)) +
  geom_point() + 
  # geom_smooth by default draws a LOESS; we don't want standard errors
  geom_smooth(se = FALSE) + 
  scale_x_log10() + scale_y_log10()
# Linear regression needs a 'lm' method
ggplot(df, aes(x = inc, y = earn)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) +
  scale_x_log10() + scale_y_log10()

# 5. Run a linear regression, by itself and including controls
model1 <- lm(lwg ~ log(inc), data = df)
# k5 is number of kids under 5 in the house
model2 <- lm(lwg ~ log(inc) + wc + k5, data = df)
# And make a nice table
msummary(list(model1, model2))
