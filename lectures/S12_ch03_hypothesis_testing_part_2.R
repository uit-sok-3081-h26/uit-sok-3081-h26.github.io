# SOK-3081 Econometrics Principles
# Chapter 3: Interval Estimation and Hypothesis Testing
# Part 2: Hypothesis Testing
#
# This script follows the lecture from slide 27 onward.
# It is written so that the figures and calculations can be run in class
# without switching back and forth between several files.

rm(list = ls())

options(scipen = 10)
options(digits = 4)

library(tidyverse)


# =============================================================================
# PART 1: DANIEL EXAMPLE - THE LOGIC OF HYPOTHESIS TESTING
# =============================================================================

# Four siblings draw one name each day.
# If the draw is fair, Daniel should be selected 25% of the time.

p_null <- 0.25
n_daniel <- 100

# Observed outcomes
daniel_22 <- 22 / 100
daniel_14 <- 14 / 100

# Exact binomial probabilities:
# "22 or fewer" and "14 or fewer"
pbinom(22, size = n_daniel, prob = p_null)
pbinom(14, size = n_daniel, prob = p_null)


# -----------------------------------------------------------------------------
# Visualise the null assumption directly on the proportion scale
# -----------------------------------------------------------------------------
# We deliberately keep the x-axis in proportions here.
# This is before introducing the t-statistic.

se_null_daniel <- sqrt(
  p_null * (1 - p_null) / n_daniel
)

daniel_null_distribution <- tibble(
  p = seq(0.05, 0.45, length.out = 1000)
) |>
  mutate(
    density = dt(
      (p - p_null) / se_null_daniel,
      df = n_daniel - 1
    ) / se_null_daniel
  )

fig_daniel_null <- ggplot(
  daniel_null_distribution,
  aes(x = p, y = density)
) +
  geom_line(linewidth = 1.1) +

  geom_vline(
    xintercept = p_null,
    linetype = "dashed"
  ) +

  geom_vline(
    xintercept = daniel_22,
    linewidth = 1
  ) +

  geom_vline(
    xintercept = daniel_14,
    linewidth = 1
  ) +

  annotate(
    "text",
    x = p_null,
    y = 9,
    label = "H0: p = 0.25",
    hjust = -0.1
  ) +

  annotate(
    "text",
    x = daniel_22,
    y = 6,
    label = "22 / 100",
    hjust = 1.1
  ) +

  annotate(
    "text",
    x = daniel_14,
    y = 3,
    label = "14 / 100",
    hjust = 1.1
  ) +

  labs(
    title = "What would we expect if H0 were true?",
    subtitle = "The distribution is centered on p = 0.25",
    x = "Proportion of days Daniel does the dishes",
    y = "Density"
  ) +

  scale_x_continuous(
    breaks = seq(0.10, 0.40, by = 0.05)
  ) +

  theme_minimal(base_size = 13)

fig_daniel_null


# =============================================================================
# PART 2: FOOD DATA - RETURN TO THE SUPERMARKET QUESTION
# =============================================================================

load(url(
  "http://www.principlesofeconometrics.com/poe5/data/rdata/food.rdata"
))

# Keep income in USD, as in the lecture
food$income <- food$income * 100

model_food <- lm(
  food_exp ~ income,
  data = food
)

summary(model_food)

b_2 <- coef(model_food)["income"]
se_b2 <- coef(summary(model_food))["income", "Std. Error"]
df_food <- df.residual(model_food)

c(
  estimate = b_2,
  standard_error = se_b2,
  degrees_of_freedom = df_food
)


# Economic threshold used in the supermarket example
beta_null <- 0.055

# Right-tail hypotheses:
# H0: beta_2 <= 0.055
# H1: beta_2 >  0.055


# Alternative method to calculate the p-value:
library(car)

linearHypothesis(
  model_food,
  "income = 0.055"
)


# -----------------------------------------------------------------------------
# First show the null distribution on the slope scale
# -----------------------------------------------------------------------------
# We assume H0 is true, so the distribution is centered on 0.055.

slope_null_distribution <- tibble(
  beta = seq(-0.03, 0.15, length.out = 1000)
) |>
  mutate(
    density = dt(
      (beta - beta_null) / se_b2,
      df = df_food
    ) / se_b2
  )

fig_food_null_scale <- ggplot(
  slope_null_distribution,
  aes(x = beta, y = density)
) +
  geom_line(linewidth = 1.1) +

  geom_vline(
    xintercept = beta_null,
    linetype = "dashed"
  ) +

  geom_vline(
    xintercept = b_2,
    linewidth = 1
  ) +

  annotate(
    "text",
    x = beta_null,
    y = max(slope_null_distribution$density) * 0.88,
    label = "H0: beta[2] == 0.055",
    parse = TRUE,
    hjust = 1.1
  ) +

  annotate(
    "text",
    x = b_2,
    y = max(slope_null_distribution$density) * 0.55,
    label = paste0(
      "Observed: b[2] == ",
      round(b_2, 4)
    ),
    parse = TRUE,
    hjust = -0.1
  ) +

  labs(
    title = "What would we expect if H0 were true?",
    subtitle = "Assume the true slope is 0.055",
    x = "Estimated slope",
    y = "Density"
  ) +

  theme_minimal(base_size = 13)

fig_food_null_scale


# =============================================================================
# PART 3: FROM DISTANCE TO THE t-STATISTIC
# =============================================================================

# Raw distance from the null value
raw_distance <- b_2 - beta_null
raw_distance

# Standardise the distance by the standard error
t_food <- (b_2 - beta_null) / se_b2
t_food

# Interpretation:
# The observed slope is about 2.25 standard errors above the null value.


# -----------------------------------------------------------------------------
# Plot the same distance on the t-scale
# -----------------------------------------------------------------------------

t_data <- tibble(
  t = seq(-4, 4, length.out = 2000)
) |>
  mutate(
    density = dt(t, df = df_food)
  )

fig_food_t_scale <- ggplot(
  t_data,
  aes(x = t, y = density)
) +
  geom_line(linewidth = 1.1) +

  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +

  geom_vline(
    xintercept = t_food,
    linewidth = 1
  ) +

  annotate(
    "text",
    x = 0,
    y = 0.37,
    label = "H0",
    hjust = 1.2
  ) +

  annotate(
    "text",
    x = t_food,
    y = 0.18,
    label = paste0(
      "Observed t = ",
      round(t_food, 2)
    ),
    hjust = -0.1
  ) +

  labs(
    title = "The same distance on the t-scale",
    subtitle = "Distance from H0 measured in standard errors",
    x = "t-statistic",
    y = "Density"
  ) +

  coord_cartesian(
    xlim = c(-4, 4),
    ylim = c(0, 0.42)
  ) +

  theme_minimal(base_size = 13)

fig_food_t_scale


# =============================================================================
# PART 4: CRITICAL VALUES - RIGHT-TAIL TEST
# =============================================================================

# For alpha = 0.05, we can find the critical t-value 
qt(0.05, df = 38)
qt(0.95, df = 38)

# For alpha = 0.01, we can find the critical t-value
qt(0.01, df = 38)
qt(0.99, df = 38)



# -----------------------------------------------------------------------------
# Figure: 5% upper-tail rejection region
# -----------------------------------------------------------------------------

right_tail_05 <- t_data |>
  filter(t >= critical_right_05)

fig_right_tail_05 <- ggplot(
  t_data,
  aes(x = t, y = density)
) +

  geom_area(
    data = right_tail_05,
    aes(x = t, y = density),
    alpha = 0.4
  ) +

  geom_line(linewidth = 1.1) +

  geom_vline(
    xintercept = critical_right_05,
    linetype = "dashed",
    linewidth = 0.9
  ) +

  geom_vline(
    xintercept = t_food,
    linewidth = 1
  ) +

  annotate(
    "text",
    x = critical_right_05,
    y = 0.07,
    label = paste0(
      "critical t = ",
      round(critical_right_05, 3)
    ),
    hjust = -0.1
  ) +

  annotate(
    "text",
    x = 2.8,
    y = 0.12,
    label = "Rejection area\nalpha = 0.05"
  ) +

  annotate(
    "text",
    x = t_food,
    y = 0.22,
    label = paste0(
      "Observed t = ",
      round(t_food, 2)
    ),
    hjust = -0.1
  ) +

  labs(
    title = "Upper-tail test",
    subtitle = expression(
      H[1] * ": " * beta[2] > 0.055
    ),
    x = "t",
    y = "Density"
  ) +

  coord_cartesian(
    xlim = c(-4, 4),
    ylim = c(0, 0.42)
  ) +

  theme_minimal(base_size = 14)

fig_right_tail_05


# =============================================================================
# PART 5: p-VALUE
# =============================================================================

# From t-values to probabilities:

pt(2.25, df = 38)

1-pt(2.25, df = 38)


p_food_right <- 1 - pt(
  t_food,
  df = df_food
)

p_food_right

# Interpretation:
# If the null value beta_2 = 0.055 were true,
# the probability of observing a t-statistic this large
# or larger is about 1.5%.


# -----------------------------------------------------------------------------
# Figure: right-tail p-value
# -----------------------------------------------------------------------------

p_value_region <- t_data |>
  filter(t >= t_food)

fig_p_value <- ggplot(
  t_data,
  aes(x = t, y = density)
) +

  geom_area(
    data = p_value_region,
    aes(x = t, y = density),
    alpha = 0.4
  ) +

  geom_line(linewidth = 1.1) +

  geom_vline(
    xintercept = t_food,
    linetype = "dashed",
    linewidth = 0.9
  ) +

  annotate(
    "text",
    x = t_food,
    y = 0.18,
    label = paste0(
      "Observed t = ",
      round(t_food, 2)
    ),
    hjust = -0.1
  ) +

  annotate(
    "text",
    x = 3,
    y = 0.08,
    label = paste0(
      "p = ",
      round(p_food_right, 3)
    )
  ) +

  labs(
    title = "The p-value",
    subtitle = "How unusual is our result if H0 is true?",
    x = "t",
    y = "Density"
  ) +

  coord_cartesian(
    xlim = c(-4, 4),
    ylim = c(0, 0.42)
  ) +

  theme_minimal(base_size = 14)

fig_p_value


# =============================================================================
# PART 6: ONE-SIDED VERSUS TWO-SIDED TESTS
# =============================================================================

# Same null value and same test statistic.
# Only the alternative hypothesis changes.

# Two-sided test:
# H0: beta_2 = 0.055
# H1: beta_2 != 0.055
qt(0.975, df = 38)

2*(1-pt(abs(2.25), df = 38))


critical_two_05 <- qt(
  1 - alpha_05 / 2,
  df = df_food
)

critical_two_05

# Two-sided p-value
p_food_two <- 2 * (
  1 - pt(
    abs(t_food),
    df = df_food
  )
)

p_food_two

# Decision
abs(t_food) > critical_two_05


# -----------------------------------------------------------------------------
# Figure: 5% two-tailed rejection regions
# -----------------------------------------------------------------------------

left_tail_two <- t_data |>
  filter(t <= -critical_two_05)

right_tail_two <- t_data |>
  filter(t >= critical_two_05)

fig_two_tail_05 <- ggplot(
  t_data,
  aes(x = t, y = density)
) +

  geom_area(
    data = left_tail_two,
    aes(x = t, y = density),
    alpha = 0.4
  ) +

  geom_area(
    data = right_tail_two,
    aes(x = t, y = density),
    alpha = 0.4
  ) +

  geom_line(linewidth = 1.1) +

  geom_vline(
    xintercept = c(
      -critical_two_05,
      critical_two_05
    ),
    linetype = "dashed",
    linewidth = 0.9
  ) +

  geom_vline(
    xintercept = t_food,
    linewidth = 1
  ) +

  annotate(
    "text",
    x = -critical_two_05,
    y = 0.065,
    label = paste0(
      "-",
      round(critical_two_05, 3)
    ),
    hjust = 1.1
  ) +

  annotate(
    "text",
    x = critical_two_05,
    y = 0.065,
    label = round(
      critical_two_05,
      3
    ),
    hjust = -0.1
  ) +

  annotate(
    "text",
    x = -2.8,
    y = 0.12,
    label = "alpha / 2 = 0.025"
  ) +

  annotate(
    "text",
    x = 2.8,
    y = 0.12,
    label = "alpha / 2 = 0.025"
  ) +

  annotate(
    "text",
    x = t_food,
    y = 0.22,
    label = paste0(
      "Observed t = ",
      round(t_food, 2)
    ),
    hjust = -0.1
  ) +

  labs(
    title = "Two-tailed test",
    subtitle = expression(
      H[1] * ": " * beta[2] != 0.055
    ),
    x = "t",
    y = "Density"
  ) +

  coord_cartesian(
    xlim = c(-4, 4),
    ylim = c(0, 0.42)
  ) +

  theme_minimal(base_size = 14)

fig_two_tail_05



# =============================================================================
# PART 7: SMALL CALCULATION - NEW STUDY
# =============================================================================






















# Solution

exercise_b2 <- 0.084
exercise_se <- 0.016
exercise_df <- 48
exercise_null <- 0.055
exercise_alpha <- 0.05

exercise_t <- (
  exercise_b2 - exercise_null
) / exercise_se

exercise_critical <- qt(
  1 - exercise_alpha,
  df = exercise_df
)

exercise_p <- 1 - pt(
  exercise_t,
  df = exercise_df
)

tibble(
  estimate = exercise_b2,
  standard_error = exercise_se,
  test_statistic = exercise_t,
  critical_value = exercise_critical,
  p_value = exercise_p,
  reject_null = exercise_p <= exercise_alpha
)



# =============================================================================
# PART 8: WHAT DOES summary(lm()) TEST BY DEFAULT?
# =============================================================================

coef(summary(model_food))["income", ]

# summary(lm()) reports the two-sided test:
# H0: beta_2 = 0
# H1: beta_2 != 0
#
# That is NOT the same question as the supermarket test:
# H0: beta_2 <= 0.055
# H1: beta_2 > 0.055

summary(model_food)

# =============================================================================
# PART 9: MOTEL DATA - BOOK EXERCISE 3.19
# =============================================================================

load(url(
  "http://www.principlesofeconometrics.com/poe5/data/rdata/motel.rdata"
))

names(motel)
head(motel)

model_motel <- lm(
  motel_pct ~ comp_pct,
  data = motel
)

summary(model_motel)

motel_b2 <- coef(model_motel)["comp_pct"]
motel_se_b2 <- coef(summary(model_motel))["comp_pct", "Std. Error"]
motel_df <- df.residual(model_motel)

c(
  estimate = motel_b2,
  standard_error = motel_se_b2,
  degrees_of_freedom = motel_df
)


# -----------------------------------------------------------------------------
# Exercise 3.19(c)
# H0: beta_2 <= 0
# H1: beta_2 > 0
# alpha = 0.01
# -----------------------------------------------------------------------------

motel_null <- 0
motel_alpha <- 0.01

motel_t <- (
  motel_b2 - motel_null
) / motel_se_b2

motel_critical_right <- qt(
  1 - motel_alpha,
  df = motel_df
)

motel_p_right <- 1 - pt(
  motel_t,
  df = motel_df
)

tibble(
  estimate = motel_b2,
  standard_error = motel_se_b2,
  t_statistic = motel_t,
  critical_value = motel_critical_right,
  p_value = motel_p_right,
  decision = if_else(
    motel_p_right < motel_alpha,
    "Reject H0",
    "Fail to reject H0"
  )
)


# -----------------------------------------------------------------------------
# Exercise 3.19(d)
# H0: beta_2 = 1
# H1: beta_2 != 1
# alpha = 0.01
# -----------------------------------------------------------------------------

motel_d_null <- 1
motel_d_alpha <- 0.01

motel_d_t <- (
  motel_b2 - motel_d_null
) / motel_se_b2

motel_d_critical <- qt(
  1 - motel_d_alpha / 2,
  df = motel_df
)

motel_d_p <- 2 * (
  1 - pt(
    abs(motel_d_t),
    df = motel_df
  )
)

tibble(
  t_statistic = motel_d_t,
  critical_value_lower = -motel_d_critical,
  critical_value_upper = motel_d_critical,
  p_value = motel_d_p,
  decision = if_else(
    motel_d_p < motel_d_alpha,
    "Reject H0",
    "Fail to reject H0"
  )
)


# =============================================================================
# PART 13: MOTEL DATA - BOOK EXERCISE 3.20(d)
# =============================================================================

# Did repairs reduce occupancy?
#
# H0: delta_2 >= 0
# H1: delta_2 < 0
# alpha = 0.05

model_repair <- lm(
  motel_pct ~ repair,
  data = motel
)

summary(model_repair)

repair_b2 <- coef(model_repair)["repair"]
repair_se_b2 <- coef(summary(model_repair))["repair", "Std. Error"]
repair_df <- df.residual(model_repair)

repair_null <- 0
repair_alpha <- 0.05

repair_t <- (
  repair_b2 - repair_null
) / repair_se_b2

repair_critical <- qt(
  repair_alpha,
  df = repair_df
)

repair_p <- pt(
  repair_t,
  df = repair_df
)

tibble(
  estimate = repair_b2,
  standard_error = repair_se_b2,
  t_statistic = repair_t,
  critical_value = repair_critical,
  p_value = repair_p,
  decision = if_else(
    repair_p < repair_alpha,
    "Reject H0",
    "Fail to reject H0"
  )
)


# =============================================================================
# PART 14: FIGURE EXPORTS
# =============================================================================

figure_dir <- file.path(
  "figures",
  "chapter03_part2"
)

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  file.path(
    figure_dir,
    "daniel_null_distribution.png"
  ),
  fig_daniel_null,
  width = 8,
  height = 4.5,
  dpi = 200
)

ggsave(
  file.path(
    figure_dir,
    "food_null_slope_scale.png"
  ),
  fig_food_null_scale,
  width = 8,
  height = 4.5,
  dpi = 200
)

ggsave(
  file.path(
    figure_dir,
    "food_t_scale.png"
  ),
  fig_food_t_scale,
  width = 8,
  height = 4.5,
  dpi = 200
)

ggsave(
  file.path(
    figure_dir,
    "right_tail_5_percent.png"
  ),
  fig_right_tail_05,
  width = 8,
  height = 4.5,
  dpi = 200
)

ggsave(
  file.path(
    figure_dir,
    "food_p_value.png"
  ),
  fig_p_value,
  width = 8,
  height = 4.5,
  dpi = 200
)

ggsave(
  file.path(
    figure_dir,
    "two_tail_5_percent.png"
  ),
  fig_two_tail_05,
  width = 8,
  height = 4.5,
  dpi = 200
)


# End of Part 2 ---------------------------------------------------------------
