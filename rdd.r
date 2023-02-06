## ----setup, include=FALSE-----------------------
knitr::opts_chunk$set(fig.width = 6, fig.asp = 0.618, fig.align = "center",
                      fig.retina = 3, out.width = "75%", collapse = TRUE)
set.seed(1234)
options("digits" = 2, "width" = 150)
options(dplyr.summarise.inform = FALSE)

#' ## Load and clean data
#' 
#' First, let's download the dataset (if you haven't already), put in a folder named `data`, and load it:
#' 
#' - [{{< fa table >}} `tutoring_program.csv`](/files/data/generated_data/tutoring_program.csv)
#' 
## ----load-libraries, message=FALSE, warning=FALSE----
library(tidyverse)  # ggplot(), %>%, mutate(), and friends
library(broom)  # Convert models to data frames
library(rdrobust)  # For robust nonparametric regression discontinuity
library(rddensity)  # For nonparametric regression discontinuity density tests
library(modelsummary)  # Create side-by-side regression tables

#' 
## ----load-data-real, include=FALSE--------------
tutoring <- read_csv("tutoring_program.csv")

## ----check-fuzzy-sharp--------------------------
ggplot(tutoring, aes(x = entrance_exam, y = tutoring, color = tutoring)) +
  # Make points small and semi-transparent since there are lots of them
  geom_point(size = 0.5, alpha = 0.5,
             position = position_jitter(width = 0, height = 0.25, seed = 1234)) +
  # Add vertical line
  geom_vline(xintercept = 70) +
  # Add labels
  labs(x = "Entrance exam score", y = "Participated in tutoring program") +
  # Turn off the color legend, since it's redundant
  guides(color = "none")

## ----fuzzy-sharp-table--------------------------
tutoring %>%
  group_by(tutoring, entrance_exam <= 70) %>%
  summarize(count = n())

## ----check-running-discontinuity----------------
ggplot(tutoring, aes(x = entrance_exam, fill = tutoring)) +
  geom_histogram(binwidth = 2, color = "white", boundary = 70) +
  geom_vline(xintercept = 70) +
  labs(x = "Entrance exam score", y = "Count", fill = "In program")


## ----check-running-discontinuity-mcrary-real, echo=FALSE, warning=FALSE----
test_density <- rddensity(tutoring$entrance_exam, c = 70)
x <- capture.output(summary(test_density))
cat(x[2:18], sep = "\n")

plot_density_test <- rdplotdensity(rdd = test_density,
                                   X = tutoring$entrance_exam,
                                   type = "both")  # This adds both points and lines

## ----check-outcome-discontinuity, message=FALSE----
ggplot(tutoring, aes(x = entrance_exam, y = exit_exam, color = tutoring)) +
  geom_point(size = 0.5, alpha = 0.5) +
  # Add a line based on a linear model for the people scoring 70 or less
  geom_smooth(data = filter(tutoring, entrance_exam <= 70), method = "lm") +
  # Add a line based on a linear model for the people scoring more than 70
  geom_smooth(data = filter(tutoring, entrance_exam > 70), method = "lm") +
  geom_vline(xintercept = 70) +
  labs(x = "Entrance exam score", y = "Exit exam score", color = "Used tutoring")

## ----simple-model-------------------------------
tutoring_centered <- tutoring %>%
  mutate(entrance_centered = entrance_exam - 70)

model_simple <- lm(exit_exam ~ entrance_centered + tutoring,
                   data = tutoring_centered)
tidy(model_simple)

## ----different-bandwidth-models-----------------
model_bw_10 <- lm(exit_exam ~ entrance_centered + tutoring,
                  data = filter(tutoring_centered,
                                entrance_centered >= -10 &
                                  entrance_centered <= 10))
tidy(model_bw_10)

model_bw_5 <- lm(exit_exam ~ entrance_centered + tutoring,
                  data = filter(tutoring_centered,
                                entrance_centered >= -5 &
                                  entrance_centered <= 5))
tidy(model_bw_5)

 modelsummary(list("Full data" = model_simple,
                   "Bandwidth = 10" = model_bw_10,
                   "Bandwidth = 5" = model_bw_5))

#' 
## ----all-models-real, echo=FALSE, warning=FALSE----
gm <- modelsummary::gof_map %>%
  mutate(omit = !(raw %in% c("nobs", "r.squared", "adj.r.squared")))

modelsummary(list("Full data" = model_simple,
                  "Bandwidth = 10" = model_bw_10,
                  "Bandwidth = 5" = model_bw_5),
             gof_map = gm, stars = TRUE) %>%
  kableExtra::row_spec(5, background = "#F6D645") %>%
  kableExtra::row_spec(6, extra_css = "border-bottom: 1px solid")

## ----different-lm-bandwidths, message=FALSE-----
ggplot(tutoring, aes(x = entrance_exam, y = exit_exam, color = tutoring)) +
  geom_point(size = 0.5, alpha = 0.2) +
  # Add lines for the full model (model_simple)
  geom_smooth(data = filter(tutoring, entrance_exam <= 70),
              method = "lm", se = FALSE, linetype = "dotted", size = 1) +
  geom_smooth(data = filter(tutoring, entrance_exam > 70),
              method = "lm", se = FALSE, linetype = "dotted", size = 1) +
  # Add lines for bandwidth = 10
  geom_smooth(data = filter(tutoring, entrance_exam <= 70, entrance_exam >= 60),
              method = "lm", se = FALSE, linetype = "dashed", size = 1) +
  geom_smooth(data = filter(tutoring, entrance_exam > 70, entrance_exam <= 80),
              method = "lm", se = FALSE, linetype = "dashed", size = 1) +
  # Add lines for bandwidth = 5
  geom_smooth(data = filter(tutoring, entrance_exam <= 70, entrance_exam >= 65),
              method = "lm", se = FALSE, size = 2) +
  geom_smooth(data = filter(tutoring, entrance_exam > 70, entrance_exam <= 75),
              method = "lm", se = FALSE, size = 2) +
  geom_vline(xintercept = 70) +
  # Zoom in
  coord_cartesian(xlim = c(50, 90), ylim = c(55, 75)) +
  labs(x = "Entrance exam score", y = "Exit exam score", color = "Used tutoring")


### Nonparametric estimation
#' The `rdrobust()` function makes it really easy to measure the gap at the cutoff with nonparametric estimation. Here's the simplest version:

rdrobust(y = tutoring$exit_exam, x = tutoring$entrance_exam, c = 70) %>%
  summary()

## ----rdplot-basic-------------------------------
rdplot(y = tutoring$exit_exam, x = tutoring$entrance_exam, c = 70)

## ----rdbwselect---------------------------------
rdbwselect(y = tutoring$exit_exam, x = tutoring$entrance_exam, c = 70) %>%
  summary()

## ----rdbwselect-all-----------------------------
rdbwselect(y = tutoring$exit_exam, x = tutoring$entrance_exam, c = 70, all = TRUE) %>%
  summary()

## ----rdrobust-different-bandwidths--------------
rdrobust(y = tutoring$exit_exam, x = tutoring$entrance_exam, c = 70, h = 9.969) %>%
  summary()

rdrobust(y = tutoring$exit_exam, x = tutoring$entrance_exam, c = 70, h = 9.969 * 2) %>%
  summary()

rdrobust(y = tutoring$exit_exam, x = tutoring$entrance_exam, c = 70, h = 9.969 / 2) %>%
  summary()

## ----rdrobust-different-kernels-----------------
# Non-parametric RD with different kernels
rdrobust(y = tutoring$exit_exam, x = tutoring$entrance_exam,
         c = 70, kernel = "triangular") %>%
  summary()  # Default

rdrobust(y = tutoring$exit_exam, x = tutoring$entrance_exam,
         c = 70, kernel = "epanechnikov") %>%
  summary()

rdrobust(y = tutoring$exit_exam, x = tutoring$entrance_exam,
         c = 70, kernel = "uniform") %>%
  summary()

