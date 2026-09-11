# SOK-3081 Econometrics Principles
# Chapter 2: The Simple Linear Regression Model
# Textbook: Principles of Econometrics, 5e, Chapter 2
#
# Purpose of this script:
# Move from a visual relationship in the data to the OLS regression line,
# then use the fitted model for residual analysis, prediction and elasticity.


# Start up -----------------------------------------------------------------------------

# Remove all objects from the current R environment so that the script starts cleanly.
rm(list = ls())

# Avoid scientific notation for ordinary-sized numbers.
options(scipen = 10)

# Display more digits in printed numerical output.
# This is useful when checking OLS identities that should be very close to zero.
options(digits = 10)

# Allow Norwegian characters in the R session.
# This may depend on the operating system and can be omitted if it causes problems.
Sys.setlocale(locale = "no_NO")

# tidyverse provides data manipulation tools (dplyr, tibble) and ggplot2.
library(tidyverse)

# mosaic provides makeFun(), which we use later for prediction.
library(mosaic)

#' Opening the data definition URL in the default web browser
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/food.def")

#' Loading the food dataset from the provided URL
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/food.rdata"))

# In the original dataset, income is measured in hundreds of dollars.
# Convert it to dollars once here. From this point onward, income = weekly dollars.
# This changes the numerical size of the slope coefficient, but not fitted values.
food <- food |>
  mutate(income = income * 100)


# -----------------------------------------------------------------------------
# LAB 1: What do the data show before estimation?
# QUESTION: Is higher income associated with higher food expenditure, and is a
# straight line a reasonable first summary?
# PREDICTION STOP: Ask students for the expected sign and strength before plotting.
# -----------------------------------------------------------------------------

# Inspect the variables and basic summary statistics.
# In particular, note the observed range and mean of income and food expenditure.
summary(food)

# Scatter plot of weekly food expenditure against weekly household income.
# The dashed lines mark the sample means (x_bar, y_bar).
# The red line is our deliberately chosen, non-OLS "simple model":
#     food expenditure = 80 + 0.12 * income
# It gives us a line to compare with the OLS line later.
food |>
  ggplot(aes(x = income, y = food_exp)) +
  geom_point(size = 2.6, colour = "#2878B5", alpha = 0.85) +
  labs(
    title = "Weekly food expenditure and household income",
    subtitle = "Three-person households",
    x = "Weekly household income (dollars)",
    y = "Weekly food expenditure (dollars)"
  ) +
  theme_minimal(base_size = 13) +
  geom_hline(
    yintercept = mean(food$food_exp),
    linetype = "dashed",
    color = "grey35"
  ) +
  geom_vline(
    xintercept = mean(food$income),
    linetype = "dashed",
    color = "grey35"
  ) +
  coord_cartesian(
    xlim = c(0, 4000),
    ylim = c(0, 600)
  ) +
  geom_abline(
    intercept = 80,
    slope = 0.12,
    color = "red",
    linewidth = 1
  )

# Check the observed income range.
# This is useful later when discussing interpolation versus extrapolation.
range(food$income)


# -----------------------------------------------------------------------------
# LAB 2: Evaluate our simple model
# -----------------------------------------------------------------------------

# Calculate fitted values from our hand-chosen line:
#     y_hat = 80 + 0.12 * income
#
# Then calculate its Sum of Squared Errors (SSE).
# A lower SSE means the line fits the observed data better according to the
# least-squares criterion.
food |>
  mutate(our_y_hat = 80 + 0.12 * income) |>
  summarise(
    sse_our = sum((food_exp - our_y_hat)^2),
    mean_fitted_value = mean(our_y_hat)
  )


# -----------------------------------------------------------------------------
# LAB 3: Estimate the OLS line manually
# -----------------------------------------------------------------------------

# OLS chooses b_1 and b_2 to minimize the sum of squared residuals.
# We calculate the coefficients directly from the OLS formulas before using lm().

# Demean x and y: subtract the sample mean from every observation.
# These deviations from the mean are the building blocks of the OLS slope.
food <- food |>
  mutate(
    income_demeaned = income - mean(income),
    food_exp_demeaned = food_exp - mean(food_exp)
  )

# Estimate the slope coefficient b_2.
#
#               sum[(x_i - x_bar)(y_i - y_bar)]
#     b_2 =     ---------------------------------
#                    sum[(x_i - x_bar)^2]
#
# The numerator measures how x and y vary together.
# The denominator measures the variation in x.
b_2 <- food |>
  summarise(
    b_2 = sum(income_demeaned * food_exp_demeaned) /
      sum(income_demeaned^2)
  ) |>
  pull(b_2)

# Estimate the intercept b_1.
# Because the OLS line with an intercept passes through (x_bar, y_bar),
# we can obtain the intercept from:
#     b_1 = y_bar - b_2 * x_bar
b_1 <- food |>
  summarise(
    b_1 = mean(food_exp) - b_2 * mean(income)
  ) |>
  pull(b_1)

# Print the manually calculated OLS coefficients.
b_1
b_2

# Calculate the fitted value for every household:
#     y_hat_i = b_1 + b_2 * x_i
# y_hat_i is our estimate of the conditional mean E(y_i | x_i).
food <- food |>
  mutate(
    y_hat = b_1 + b_2 * income
  )

# Calculate the OLS residual for every household:
#     e_hat_i = y_i - y_hat_i
# A positive residual means observed food expenditure is above the fitted line.
# A negative residual means it is below the fitted line.
food <- food |>
  mutate(
    e_hat = food_exp - y_hat
  )

# Check the fit of the OLS model.
# SSE is the quantity OLS minimizes.
# The mean residual should be numerically extremely close to zero when an
# intercept is included in the model.
food |>
  summarise(
    sse = sum(e_hat^2),
    mean_residual = mean(e_hat)
  )


# -----------------------------------------------------------------------------
# LAB 4: Estimate the same model using lm()
# -----------------------------------------------------------------------------

# lm() estimates the OLS regression automatically.
# The formula food_exp ~ income means:
#     dependent variable = food_exp
#     explanatory variable = income
model_food <- lm(food_exp ~ income, data = food)
model_food

# Full regression output: coefficients, standard errors, t-statistics, R-squared, etc.
summary(model_food)

# Extract only the estimated intercept and slope.
# These should be the same as b_1 and b_2 calculated manually above.
coef(model_food)


# -----------------------------------------------------------------------------
# LAB 5: Plot the OLS line and inspect residuals
# -----------------------------------------------------------------------------

# Plot the observed data and the estimated OLS regression line.
# The dashed lines again show the sample means.
# Notice that the OLS line passes through (x_bar, y_bar).
food |>
  ggplot(aes(x = income, y = food_exp)) +
  geom_point(size = 2.6, colour = "#2878B5", alpha = 0.85) +
  labs(
    title = "Weekly food expenditure and household income",
    subtitle = "Three-person households",
    x = "Weekly household income (dollars)",
    y = "Weekly food expenditure (dollars)"
  ) +
  theme_minimal(base_size = 13) +
  geom_hline(
    yintercept = mean(food$food_exp),
    linetype = "dashed",
    color = "grey35"
  ) +
  geom_vline(
    xintercept = mean(food$income),
    linetype = "dashed",
    color = "grey35"
  ) +
  coord_cartesian(
    xlim = c(0, 4000),
    ylim = c(0, 600)
  ) +
  geom_abline(
    intercept = b_1,
    slope = b_2,
    color = "red",
    linewidth = 1
  )

# Plot the distribution of the residuals.
# This is a first descriptive look at whether residuals are centered around zero
# and whether the distribution appears strongly asymmetric or unusual.
# A histogram alone is NOT a formal test of the regression assumptions.
food |>
  ggplot(aes(x = e_hat)) +
  geom_histogram(bins = 10) +
  labs(
    title = "Distribution of OLS residuals",
    x = "Residual",
    y = "Number of observations"
  ) +
  theme_minimal(base_size = 13)


# -----------------------------------------------------------------------------
# LAB 6: Algebraic properties of the OLS residuals
# -----------------------------------------------------------------------------

# With an intercept in the regression, OLS mechanically gives us several
# important sample properties:
#   1. the residuals sum to zero,
#   2. the mean residual is zero,
#   3. x and the residuals have zero sample covariance.
#
# These are properties of the fitted sample. They do NOT prove the population
# exogeneity assumption E(e_i | x) = 0.

food |>
  summarise(
    sum_residuals = sum(e_hat),
    mean_residual = mean(e_hat),
    sum_x_residual = sum(income * e_hat),
    covariance_x_residual = cov(income, e_hat),
    correlation_x_residual = cor(income, e_hat)
  )

# Why is the covariance zero?
# Sample covariance is based on deviations from the means:
#     cov(x, e_hat) = 1/(N-1) * sum[(x_i - x_bar)(e_hat_i - e_hat_bar)]
# Since OLS with an intercept gives e_hat_bar = 0, and the OLS first-order
# condition gives sum(x_i * e_hat_i) = 0, the sample covariance is zero.
# Small non-zero printed values may occur because computers use finite precision.


# -----------------------------------------------------------------------------
# LAB 7: Prediction
# -----------------------------------------------------------------------------

# makeFun() converts the fitted lm object into a convenient prediction function.
# We can then supply a value of income directly.
f <- makeFun(model_food)

# Predicted mean weekly food expenditure for income = $3,000.
# This value is inside the observed range of x, so this is interpolation.
f(income = 3000)

# Prediction at the sample mean of income.
# Because the OLS line passes through the sample means, this fitted value should
# equal the sample mean of food expenditure.
f(mean(food$income))

# Confidence interval for the conditional mean E(y | x) at mean income.
# This is uncertainty about the mean outcome, not about a single new household.
f(mean(food$income), interval = "confidence")


# -----------------------------------------------------------------------------
# LAB 8: Avoid extrapolation
# -----------------------------------------------------------------------------

# Predictions are most credible within the observed range of income.
# Outside that range, the fitted line will still return a numerical prediction,
# but we are assuming that the same linear relationship continues into a region
# for which we have no observations.

# The two green vertical lines mark the minimum and maximum observed income.
food |>
  ggplot(aes(x = income, y = food_exp)) +
  geom_point(size = 2.6, colour = "#2878B5", alpha = 0.85) +
  labs(
    title = "Prediction and the observed income range",
    subtitle = "Predictions outside the green lines are extrapolations",
    x = "Weekly household income (dollars)",
    y = "Weekly food expenditure (dollars)"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(
    xintercept = range(food$income),
    linetype = "dashed",
    color = "green"
  ) +
  coord_cartesian(
    xlim = c(0, 4000),
    ylim = c(0, 600)
  ) +
  geom_abline(
    intercept = b_1,
    slope = b_2,
    color = "red",
    linewidth = 1
  )

# Print the exact observed income range for reference.
range(food$income)


# -----------------------------------------------------------------------------
# LAB 9: Elasticity at the sample mean
# -----------------------------------------------------------------------------

# In a linear level-level model, the slope b_2 is a constant marginal effect,
# but elasticity is NOT constant.
#
# Elasticity at a particular point is:
#     elasticity = b_2 * x / y_hat
#
# A common reporting choice is to evaluate elasticity at the sample means.
# Since the OLS line passes through (x_bar, y_bar), y_hat at x_bar equals y_bar.

elasticity_at_mean <-
  b_2 * mean(food$income) /
  (b_1 + b_2 * mean(food$income))

elasticity_at_mean


# -----------------------------------------------------------------------------
# LAB 10: Elasticity across the observed income range
# -----------------------------------------------------------------------------

# First check the range over which we actually observed income.
range(food$income)

# Create 100 evenly spaced income values between the sample minimum and maximum.
# We deliberately restrict the calculation to the observed range to avoid
# interpreting the linear model far outside the data.
elasticity_data <- tibble(
  income = seq(
    from = min(food$income),
    to = max(food$income),
    length.out = 100
  )
) |>
  mutate(
    # Predicted food expenditure at each income level.
    y_hat = b_1 + b_2 * income,
    
    # Point elasticity of predicted food expenditure with respect to income.
    elasticity = b_2 * income / y_hat
  )

elasticity_data

# Plot how the elasticity implied by the linear model varies with income.
# The horizontal line at 1 separates necessities (elasticity < 1) from
# expenditure categories that increase proportionally or more than proportionally.
# The vertical dashed line marks the sample mean income.
elasticity_data |>
  ggplot(aes(x = income, y = elasticity)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(
    xintercept = mean(food$income),
    linetype = "dashed",
    color = "grey35"
  ) +
  labs(
    title = "Income elasticity of food expenditure",
    subtitle = "Elasticity implied by the linear model within the observed income range",
    x = "Weekly household income (dollars)",
    y = "Elasticity"
  ) +
  theme_minimal(base_size = 13)

# Interpretation:
# In a linear model with a positive intercept and positive slope, elasticity can
# increase with income even while food expenditure becomes a smaller share of
# income. This is a consequence of the chosen linear functional form.
# Do not extend this pattern far beyond the observed income range.

#' -------------------------------
#' Other economic models, 2.3.2
#' Estimating an "iron" model where the response is the iron ore price and the predictor is the exchange rate
rm(list=ls())
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/iron.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/iron.rdata"))

ironmodel <- lm(iron ~ xrate, data = iron)
summary(ironmodel)

#' Calculating the elasticity of the model
coef(ironmodel)[2]*mean(iron$xrate)/mean(iron$iron)

#' -------------------------------
#' Example 2.6
rm(list=ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/br.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/br.rdata"))

head(br)

