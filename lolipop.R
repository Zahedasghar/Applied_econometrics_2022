library(tidyverse)


manufacturers <- mpg |> 
  count(manufacturer, sort = TRUE) |> 
  mutate(
    manufacturer = str_to_title(manufacturer),
    manufacturer = fct_reorder(manufacturer, n) 
  )
manufacturers
## Bar plot
manufacturers |> 
  ggplot(aes(y = manufacturer, x = n)) +
  geom_col(fill = 'dodgerblue4') +
  theme_minimal() +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = element_blank(), 
    y = element_blank(),
    title = 'Number of cars in the {mpg} data set'
  ) +
  theme(
    panel.grid.major.y = element_blank()
  )

## Lollipop chart
manufacturers |> 
  ggplot(aes(y = manufacturer, x = n)) +
  geom_point(col = 'dodgerblue4', size = 5) +
  geom_segment(
    aes(x = 0, xend = n, y = manufacturer, yend = manufacturer),
    linewidth = 1.5,
    col = 'dodgerblue4'
  ) +
  theme_minimal() +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = element_blank(), 
    y = element_blank(),
    title = 'Number of cars in the {mpg} data set'
  ) +
  theme(
    panel.grid.major.y = element_blank()
  )
