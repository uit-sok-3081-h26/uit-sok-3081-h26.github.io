# SOK-3081 Econometrics Principles
# Chapter 3: Interval Estimation and Hypothesis Testing
# Session 11-12
#
# The script follows the lecture.  The calculations are deliberately shown
# before the convenient built-in R functions are used.

# START UP -------------------------------------------------------------------

rm(list = ls())
options(scipen = 10)
options(digits = 4)

library(tidyverse)


# PART 1: RECONNECT TO A VERY SMALL DATASET ----------------------------------
# We used these five observations in Chapter 2 and calculated the OLS results
# by hand.  We now add interval estimation to the same example.

small_data <- tibble(
  x = c(-1, 0, 1, 2, 3),
  y = c(1, 0, 3, 2, 4)
)

small_model <- lm(y ~ x, data = small_data)
summary(small_model)

# Values already obtained in the Chapter 2 exercise.
small_n <- nrow(small_data)
small_b2 <- coef(small_model)["x"]
small_se_b2 <- coef(summary(small_model))["x", "Std. Error"]
small_df <- df.residual(small_model)

c(
  estimate = small_b2,
  standard_error = small_se_b2,
  degrees_of_freedom = small_df
)

# Construct a 95 percent interval manually.
small_alpha <- 0.05
small_critical_t <- qt(1 - small_alpha / 2, df = small_df)

small_lower <- small_b2 - small_critical_t * small_se_b2
small_upper <- small_b2 + small_critical_t * small_se_b2

c(
  critical_t = small_critical_t,
  lower = small_lower,
  estimate = small_b2,
  upper = small_upper
)

# Check the same calculation with R.
confint(small_model, "x", level = 0.95)

# STOP AND INTERPRET
# 1. Why is the critical t-value so large here?
# 2. Is the interval narrow or wide?
# 3. Does the interval contain zero?
# 4. What does that tell us about the precision of this very small sample?


# PART 2: FOOD DATA - FROM A STANDARD ERROR TO AN INTERVAL -------------------
rm(list = ls())
load(url(
  "http://www.principlesofeconometrics.com/poe5/data/rdata/food.rdata"
))

# Keep the same unit used in the previous sessions: weekly income in dollars.
food$income <- food$income * 100

model_food <- lm(food_exp ~ income, data = food)
summary(model_food)

b_2 <- coef(model_food)["income"]
se_b2 <- coef(summary(model_food))["income", "Std. Error"]
df_food <- df.residual(model_food)

c(
  estimate = b_2,
  standard_error = se_b2,
  degrees_of_freedom = df_food
)

# 95 percent interval, calculated manually.
confidence_level <- 0.95
alpha <- 1 - confidence_level
critical_t <- qt(1 - alpha / 2, df = df_food)

food_ci_manual <- c(
  lower = b_2 - critical_t * se_b2,
  estimate = b_2,
  upper = b_2 + critical_t * se_b2
)

food_ci_manual

# The slope and interval are kept in the same unit throughout the lecture:
# USD change in weekly food expenditure for a USD 1 increase in weekly income.

# Check with R's built-in function.
confint(model_food, "income", level = 0.95)

confint(model_food, level = 0.95)

# STOP AND INTERPRET
# A confidence interval is a statement about a METHOD / PROCEDURE.
# It is not a probability statement about the particular interval we happened
# to obtain from this sample.


# PART 3: WHAT DOES 95 PERCENT CONFIDENCE MEAN? ------------------------------
# Repeated sampling makes the interpretation visible.

true_intercept <- 80
true_slope <- 0.10
error_sd <- 115
sample_size <- 40
repetitions <- 1000

set.seed(3081)

draw_estimate <- function() {
  
  sampled_income <- runif(
    sample_size,
    min = 500,
    max = 3500
  )
  
  simulated_data <- tibble(
    income = sampled_income,
    food_exp =
      true_intercept +
      true_slope * sampled_income +
      rnorm(
        sample_size,
        mean = 0,
        sd = error_sd
      )
  )
  
  fitted_model <- lm(
    food_exp ~ income,
    data = simulated_data
  )
  
  tibble(
    estimate = coef(fitted_model)["income"]
  )
}

simulated_estimates <- bind_rows(
  replicate(
    repetitions,
    draw_estimate(),
    simplify = FALSE
  )
)
mean(simulated_intervals$contains_true_slope)

# In a finite simulation the fraction is close to 0.946, not necessarily
# exactly 0.946.



# PART 4: CONFIDENCE LEVEL AND INTERVAL WIDTH --------------------------------

# Compare the middle 90%, 95%, and 99%
# of the simulated sampling distribution

confidence_levels <- c(0.90, 0.95, 0.99)

interval_comparison <- map_dfr(confidence_levels, function(level) {
  
  tail_probability <- (1 - level) / 2
  
  limits <- quantile(
    simulated_estimates$estimate,
    probs = c(tail_probability, 1 - tail_probability)
  )
  
  tibble(
    confidence_level = level,
    lower = limits[1],
    upper = limits[2],
    width = limits[2] - limits[1]
  )
})

interval_comparison

# THINK BEFORE RUNNING
# What should happen to the interval when the confidence level increases?


# PART 5: NEW DATA - MOTEL OCCUPANCY ----------------------------------------
# Textbook computer exercise 3.19.
# The owners of a motel compare their occupancy rate with a competitor.

load(url(
  "http://www.principlesofeconometrics.com/poe5/data/rdata/motel.rdata"
))

names(motel)
head(motel)

# First look at the data.
motel |>
  ggplot(aes(x = comp_pct, y = motel_pct)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Motel and competitor occupancy rates",
    x = "Competitor occupancy rate (%)",
    y = "Motel occupancy rate (%)"
  ) +
  theme_minimal(base_size = 13)




# PART 6: HYPOTHESIS TESTING - THE SUPERMARKET DECISION ---------------------
# The investment is profitable only if weekly food expenditure increases by
# more than USD 0.055 for each USD 1 increase in weekly household income.

null_value <- 0.055

# Economic hypotheses:
# H0: beta_2 <= 0.055   The increase is not large enough for investment.
# H1: beta_2 >  0.055   The increase is large enough for investment.
#
# The alternative points to the right: this is a right-tail test.

c(
  estimate = b_2,
  standard_error = se_b2,
  null_value = null_value,
  degrees_of_freedom = df_food
)

# Test statistic: distance from the hypothesized value, measured in SEs.
t_food <- (b_2 - null_value) / se_b2
t_food

# Critical values for two commonly used significance levels.
critical_05 <- qt(0.95, df = df_food)
critical_01 <- qt(0.99, df = df_food)

c(
  critical_value_5_percent = critical_05,
  critical_value_1_percent = critical_01
)

# Right-tail p-value.
p_food_right <- 1 - pt(t_food, df = df_food)
p_food_right

# Same data, different decision thresholds.
tibble(
  alpha = c(0.05, 0.01),
  critical_value = c(critical_05, critical_01),
  p_value = p_food_right,
  reject_null = p_food_right <= c(0.05, 0.01)
)

# STOP AND INTERPRET
# At alpha = 0.05, what is the conclusion?
# At alpha = 0.01, what is the conclusion?
# Why can the conclusion change when the data are exactly the same?


# PART 7: THREE ALTERNATIVES - ONE TEST STATISTIC ----------------------------
# We do not need three different formulas.  The test statistic is the same:
#
#              estimate - hypothesized value
#        t =  --------------------------------
#                     standard error
#
# What changes is where the rejection region / p-value is located.

# Right tail: H1: beta_2 > c
p_right <- 1 - pt(t_food, df = df_food)

# Left tail: H1: beta_2 < c
p_left <- pt(t_food, df = df_food)

# Two sided: H1: beta_2 != c
p_two_sided <- 2 * (1 - pt(abs(t_food), df = df_food))

c(
  right_tail = p_right,
  left_tail = p_left,
  two_sided = p_two_sided
)


# PART 8: WHAT DOES lm() REPORT BY DEFAULT? ----------------------------------

default_food_output <- coef(summary(model_food))["income", ]
default_food_output

# summary(lm()) reports the two-sided test
# H0: beta_2 = 0
# H1: beta_2 != 0
#
# It does NOT automatically answer the supermarket question, because the
# supermarket uses a different null value and a one-sided alternative.


# PART 9: SHORT CALCULATION ---------------------------------------------------
# A new study reports:
# b2 = 0.084, se(b2) = 0.016, df = 48.
# Test H0: beta_2 <= 0.055 against H1: beta_2 > 0.055 at alpha = 0.05.

exercise_b2 <- 0.084
exercise_se <- 0.016
exercise_df <- 48
exercise_null <- 0.055
exercise_alpha <- 0.05

exercise_t <- (exercise_b2 - exercise_null) / exercise_se
exercise_critical <- qt(1 - exercise_alpha, df = exercise_df)
exercise_p <- 1 - pt(exercise_t, df = exercise_df)

c(
  test_statistic = exercise_t,
  critical_value = exercise_critical,
  p_value = exercise_p,
  reject_null = exercise_p <= exercise_alpha
)


# PART 10: TWO-SIDED TESTS AND CONFIDENCE INTERVALS --------------------------
# For a point null H0: beta_2 = c, a two-sided 5 percent test and a 95 percent
# confidence interval produce the same reject / fail-to-reject decision.

food_ci_95 <- confint(model_food, "income", level = 0.95)

point_null <- 0.055
point_null_inside_ci <-
  point_null >= food_ci_95[1] & point_null <= food_ci_95[2]

food_two_sided_p <- 2 * (1 - pt(abs(t_food), df = df_food))

tibble(
  method = c("95% interval", "Two-sided 5% test"),
  result = c(
    paste0("[", round(food_ci_95[1], 4), ", ", round(food_ci_95[2], 4), "]"),
    paste0("p = ", round(food_two_sided_p, 4))
  ),
  decision_for_beta_2_eq_0_055 = c(
    ifelse(point_null_inside_ci, "Fail to reject", "Reject"),
    ifelse(food_two_sided_p <= 0.05, "Reject", "Fail to reject")
  )
)


# PART 11: INTERPRETATION -----------------------------------------------------
# Complete these sentences before looking at the next lecture slide.
#
# 1. Estimate:
#    "A USD 1 increase in weekly income is associated with ..."
#
# 2. Uncertainty:
#    "The 95 percent interval for the population slope is ..."
#
# 3. Hypothesis test:
#    "At alpha = 0.05 we ... because ..."
#
# 4. Scope:
#    "This regression establishes an association, but does not by itself ..."


# PART 12: p-VALUE - CALCULATE IT DIRECTLY ----------------------------------
# For the supermarket question we already calculated the observed t-statistic.
# Because H1: beta_2 > 0.055, this is a right-tail test.
#
# The p-value is the probability of obtaining this t-value or a more extreme
# value in the direction of H1, if H0 were true.

p_value_supermarket <- 1 - pt(t_food, df = df_food)

c(
  t_statistic = t_food,
  p_value = p_value_supermarket
)

# The same decision can be written in two equivalent ways:
#   reject H0 if t_food > critical_05
#   reject H0 if p_value_supermarket < 0.05


# PART 13: WORKED BOOK EXAMPLE - MOTEL EXERCISE 3.19(c) ---------------------
# We use the motel model estimated earlier:
#   motel_pct = beta_1 + beta_2 * comp_pct + e
#
# Question from Exercise 3.19(c):
# H0: beta_2 <= 0
# H1: beta_2 > 0
# alpha = 0.01

motel_null <- 0
motel_alpha <- 0.01

motel_t <- (motel_b2 - motel_null) / motel_se_b2
motel_critical_right <- qt(1 - motel_alpha, df = motel_df)
motel_p_right <- 1 - pt(motel_t, df = motel_df)

motel_worked_result <- tibble(
  estimate = motel_b2,
  standard_error = motel_se_b2,
  t_statistic = motel_t,
  critical_value = motel_critical_right,
  p_value = motel_p_right,
  decision = if_else(motel_p_right < motel_alpha, "Reject H0", "Fail to reject H0")
)

motel_worked_result

# Interpretation:
# At the 1 percent level, do the data provide evidence that beta_2 > 0?
# Translate the statistical conclusion back to the relationship between the
# competitor's occupancy rate and the motel's occupancy rate.


# PART 14: STUDENT TASK - MOTEL EXERCISE 3.19(d) ----------------------------
# Use the SAME estimated model.
#
# H0: beta_2 = 1
# H1: beta_2 != 1
# alpha = 0.01
#
# Tasks:
# 1. Calculate the t-statistic.
# 2. Find the two-sided critical value.
# 3. Calculate the two-sided p-value.
# 4. State the statistical conclusion.
# 5. Explain what beta_2 = 1 means in this context.

# Starter code:
motel_d_null <- 1
motel_d_alpha <- 0.01

# Complete these lines:
# motel_d_t <- ...
# motel_d_critical <- ...
# motel_d_p <- ...


# PART 15: STUDENT TASK - MOTEL EXERCISE 3.20(d) ----------------------------
# The motel was under repair for part of the period. Estimate:
#
#   motel_pct = delta_1 + delta_2 * repair + e
#
# The book asks whether the repairs reduced occupancy:
# H0: delta_2 >= 0
# H1: delta_2 < 0
# alpha = 0.05

model_repair <- lm(motel_pct ~ repair, data = motel)
summary(model_repair)

repair_b2 <- coef(model_repair)["repair"]
repair_se_b2 <- coef(summary(model_repair))["repair", "Std. Error"]
repair_df <- df.residual(model_repair)

# Student tasks:
# 1. Interpret the estimated coefficient on repair.
# 2. Calculate the t-statistic for H0: delta_2 = 0.
# 3. Find the left-tail critical value.
# 4. Calculate the left-tail p-value.
# 5. State the conclusion in the context of the motel's claim.

# Starter code:
repair_null <- 0
repair_alpha <- 0.05

# Complete these lines:
# repair_t <- ...
# repair_critical <- ...
# repair_p <- ...


# =============================================================================
# FIGURES USED IN THE LECTURE SLIDES
# =============================================================================
# These chunks reproduce the figures used in the PowerPoint.  They are placed
# together here so it is clear where every figure comes from.

figure_dir <- file.path("figures", "chapter03")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

uit_blue <- "#00566B"
uit_cyan <- "#20B4D6"
uit_red <- "#D71920"
uit_light <- "#DDF4F9"


# FIGURE A: FOOD DATA AND FITTED REGRESSION ----------------------------------

fig_food <- food |>
  ggplot(aes(x = income, y = food_exp)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = uit_red, linewidth = 1) +
  labs(
    x = "Weekly household income (USD)",
    y = "Weekly food expenditure (USD)"
  ) +
  theme_minimal(base_size = 13)

fig_food
ggsave(file.path(figure_dir, "food_regression.png"), fig_food,
       width = 8, height = 4.5, dpi = 200)


# FIGURE B: SAMPLING DISTRIBUTIONS, N = 40 AND N = 200 ----------------------

draw_one_slope <- function(n) {
  x <- runif(n, min = 500, max = 3500)
  y <- true_intercept + true_slope * x + rnorm(n, 0, error_sd)
  coef(lm(y ~ x))["x"]
}

set.seed(3081)

sampling_slopes <- bind_rows(
  tibble(
    estimate = replicate(1000, draw_one_slope(40)),
    sample_size = "N = 40"
  ),
  tibble(
    estimate = replicate(1000, draw_one_slope(200)),
    sample_size = "N = 200"
  )
)

fig_sampling <- sampling_slopes |>
  ggplot(aes(x = estimate, color = sample_size)) +
  geom_density(linewidth = 1.2) +
  geom_vline(xintercept = true_slope, linetype = "dashed", color = uit_red) +
  labs(
    x = "Estimated slope",
    y = "Density",
    color = NULL
  ) +
  theme_minimal(base_size = 13)

fig_sampling
ggsave(file.path(figure_dir, "sampling_distribution_40_200.png"), fig_sampling,
       width = 8, height = 4.5, dpi = 200)


# FIGURE C: NORMAL AND t DISTRIBUTIONS ---------------------------------------

x_grid <- seq(-4, 4, length.out = 1000)

distribution_data <- bind_rows(
  tibble(x = x_grid, density = dnorm(x_grid), distribution = "Normal"),
  tibble(x = x_grid, density = dt(x_grid, df = 5), distribution = "t(5)"),
  tibble(x = x_grid, density = dt(x_grid, df = 38), distribution = "t(38)")
)

fig_t_compare <- distribution_data |>
  ggplot(aes(x = x, y = density, color = distribution)) +
  geom_line(linewidth = 1.1) +
  labs(x = "Standardized value", y = "Density", color = NULL) +
  theme_minimal(base_size = 13)

fig_t_compare
ggsave(file.path(figure_dir, "normal_t_comparison.png"), fig_t_compare,
       width = 8, height = 4.5, dpi = 200)


# FIGURE D: SAME ESTIMATE, DIFFERENT PRECISION -------------------------------

precision_data <- bind_rows(
  tibble(
    value = seq(-0.15, 0.35, length.out = 800),
    se = "SE = 0.02"
  ),
  tibble(
    value = seq(-0.15, 0.35, length.out = 800),
    se = "SE = 0.08"
  )
) |>
  mutate(
    density = if_else(
      se == "SE = 0.02",
      dnorm(value, mean = 0.10, sd = 0.02),
      dnorm(value, mean = 0.10, sd = 0.08)
    )
  )

fig_precision <- precision_data |>
  ggplot(aes(x = value, y = density, color = se)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 0.10, linetype = "dashed", color = uit_red) +
  labs(x = "Possible slope estimates", y = "Density", color = NULL) +
  theme_minimal(base_size = 13)

fig_precision
ggsave(file.path(figure_dir, "same_estimate_different_precision.png"), fig_precision,
       width = 8, height = 4.5, dpi = 200)


# FIGURE E: 95 PERCENT INTERVALS FROM REPEATED SAMPLES -----------------------

intervals_to_plot <- simulated_intervals |>
  slice(1:25)

fig_repeated_ci <- intervals_to_plot |>
  ggplot(aes(
    y = sample,
    x = estimate,
    xmin = lower,
    xmax = upper,
    color = contains_true_slope
  )) +
  geom_errorbarh(height = 0) +
  geom_point(size = 2) +
  geom_vline(xintercept = true_slope, linetype = "dashed", color = uit_red) +
  scale_color_manual(values = c(`TRUE` = uit_blue, `FALSE` = uit_red)) +
  labs(x = "Slope", y = "Sample", color = "Contains β2") +
  theme_minimal(base_size = 13)

fig_repeated_ci
ggsave(file.path(figure_dir, "repeated_confidence_intervals.png"), fig_repeated_ci,
       width = 8, height = 5, dpi = 200)


# FIGURE F: 90, 95, AND 99 PERCENT INTERVALS --------------------------------

fig_interval_width <- interval_comparison |>
  mutate(label = paste0(round(confidence_level * 100), "%")) |>
  ggplot(aes(y = reorder(label, confidence_level), x = b_2,
             xmin = lower, xmax = upper)) +
  geom_errorbarh(height = 0, linewidth = 2, color = uit_cyan) +
  geom_point(size = 3, color = uit_blue) +
  labs(x = "Possible values of beta_2", y = "Confidence level") +
  theme_minimal(base_size = 13)

fig_interval_width
ggsave(file.path(figure_dir, "confidence_level_width.png"), fig_interval_width,
       width = 8, height = 4.5, dpi = 200)


# FIGURE G: THE THREE ALTERNATIVES -------------------------------------------

plot_tail <- function(alternative = c("left", "right", "two"),
                      alpha = 0.05, df = 38) {
  alternative <- match.arg(alternative)
  x <- seq(-4, 4, length.out = 1000)
  d <- dt(x, df = df)
  plot_data <- tibble(x = x, density = d)

  if (alternative == "left") {
    cutoff <- qt(alpha, df)
    plot_data <- plot_data |> mutate(reject = x <= cutoff)
    title_text <- "H1: beta_2 < c"
  }

  if (alternative == "right") {
    cutoff <- qt(1 - alpha, df)
    plot_data <- plot_data |> mutate(reject = x >= cutoff)
    title_text <- "H1: beta_2 > c"
  }

  if (alternative == "two") {
    cutoff <- qt(1 - alpha / 2, df)
    plot_data <- plot_data |> mutate(reject = abs(x) >= cutoff)
    title_text <- "H1: beta_2 != c"
  }

  ggplot(plot_data, aes(x = x, y = density)) +
    geom_line(color = uit_blue, linewidth = 1.1) +
    geom_area(data = filter(plot_data, reject), fill = uit_red, alpha = 0.75) +
    labs(title = title_text, x = "t", y = NULL) +
    theme_minimal(base_size = 13) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
}

fig_left <- plot_tail("left")
fig_right <- plot_tail("right")
fig_two <- plot_tail("two")

fig_left
fig_right
fig_two

ggsave(file.path(figure_dir, "left_tail.png"), fig_left,
       width = 6, height = 4, dpi = 200)
ggsave(file.path(figure_dir, "right_tail.png"), fig_right,
       width = 6, height = 4, dpi = 200)
ggsave(file.path(figure_dir, "two_tail.png"), fig_two,
       width = 6, height = 4, dpi = 200)


# FIGURE H: RIGHT-TAIL p-VALUE FOR THE SUPERMARKET TEST ----------------------

p_grid <- tibble(
  t = seq(-4, 4, length.out = 1000)
) |>
  mutate(
    density = dt(t, df = df_food),
    p_region = t >= t_food
  )

fig_p_value <- ggplot(p_grid, aes(x = t, y = density)) +
  geom_line(color = uit_blue, linewidth = 1.1) +
  geom_area(data = filter(p_grid, p_region), fill = uit_red, alpha = 0.75) +
  geom_vline(xintercept = t_food, linetype = "dashed", color = uit_red) +
  labs(x = "t-statistic", y = NULL) +
  theme_minimal(base_size = 13) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

fig_p_value
ggsave(file.path(figure_dir, "p_value_supermarket.png"), fig_p_value,
       width = 8, height = 4.5, dpi = 200)


# FIGURE I: SESSION 10 SMALL-DATA RECAP --------------------------------------
# Used on the introductory recap slide. The figure shows the five observations,
# the fitted OLS line, and the point of sample means.

fig_session10 <- small_data |>
  ggplot(aes(x = x, y = y)) +
  geom_vline(xintercept = 0, color = "grey65", linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "grey65", linewidth = 0.5) +
  geom_point(size = 3, color = uit_blue) +
  geom_abline(intercept = 1.2, slope = 0.8,
              color = uit_red, linewidth = 1) +
  annotate(
    "point",
    x = mean(small_data$x),
    y = mean(small_data$y),
    shape = 4,
    size = 4,
    stroke = 1.2,
    color = uit_cyan
  ) +
  annotate(
    "text",
    x = 2.45,
    y = 3.1,
    label = "y-hat = 1.2 + 0.8x",
    color = uit_red,
    size = 4
  ) +
  labs(x = "x", y = "y") +
  coord_cartesian(xlim = c(-1.5, 3.5), ylim = c(-0.5, 4.5)) +
  theme_minimal(base_size = 13) +
  theme(panel.grid = element_blank())

fig_session10
ggsave(file.path(figure_dir, "session10_small_data.png"), fig_session10,
       width = 7, height = 4.4, dpi = 200)

# End of script --------------------------------------------------------------
