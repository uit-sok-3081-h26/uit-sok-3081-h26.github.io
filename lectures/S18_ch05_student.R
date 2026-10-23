# SOK-3081 | Chapter 5 seminar
# Student worksheet
# Work through one exercise at a time. The script contains data and headings,
# but not worked solutions.


# Start up: Packages and data --------------------------------------------------

rm(list = ls())
options(scipen = 10)
options(digits = 4)

library(tidyverse)

# Exercise 5.21 uses deltaMethod() from the car package.
# Run install.packages("car") once if the package is not installed.
library(car)

# Download the textbook datasets directly from the internet.
# load() places the data frames london5 and toody5 in the workspace.
load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/london5.rdata"))
load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/toody5.rdata"))

# Inspect the downloaded data.
head(london5)
names(london5)

head(toody5)
names(toody5)

# The seven observations for Exercise 5.1 are printed in the exercise.
exercise_51 <- tibble(
  i = 1:7,
  y = c(1, 1, 4, 0, 1, -2, 2),
  x1 = rep(1, 7),
  x2 = c(0, 1, 2, -2, 1, -2, 0),
  x3 = c(1, -2, 2, 1, -2, -1, 1)
)

exercise_51


# 5.1 Estimate a multiple regression by hand --------------------------------

## 5.1a Deviations from the sample means

# Write your code below.


## 5.1b Cross-products and sums of squares

# Write your code below.


## 5.1c Least-squares estimates b1, b2 and b3

# Write your code below.


## 5.1d Least-squares residuals

# Write your code below.


## 5.1e Estimate the error variance

# Write your code below.


## 5.1f Correlation between x2 and x3

# Write your code below.


## 5.1g Standard error of b2

# Write your code below.


## 5.1h SSE, SST, SSR and R-squared

# Write your code below.


# 5.2 Confidence interval and hypothesis test -------------------------------

## 5.2a A 95% confidence interval for beta_2

# Continue with the estimates from Exercise 5.1.
# Write your code below.


## 5.2b Test H0: beta_2 = 1.25 against H1: beta_2 != 1.25

# Write your code below.


# 5.3 Complete and interpret regression output ------------------------------

# The following values reproduce the incomplete output supplied in the task.
output_53 <- tibble(
  variable = c("Intercept", "log(TOTEXP)", "NK", "AGE"),
  coefficient = c(1.4515, 2.7648, NA, -0.1503),
  standard_error = c(2.2019, NA, 0.3695, 0.0235),
  t_statistic = c(NA, 5.7103, -3.9376, -6.4019),
  p_value = c(0.5099, 0.0000, 0.0001, 0.0000)
)

N_53 <- 1200
K_53 <- 4
SSE_53 <- 46221.62
mean_walc_53 <- 6.19434
sd_walc_53 <- 6.39547

output_53


## 5.3a Complete the missing regression output

# Write your code below.


## 5.3b Interpret b2, b3 and b4

# Write your interpretation as comments below.


## 5.3c A 95% confidence interval for beta_4

# Write your code below.


## 5.3d Which coefficients differ significantly from zero at 5%?

# Write your code and conclusion below.


## 5.3e Test whether one additional child reduces the share by 2 points

# Write your code below.


# 5.4 Alcohol expenditure, children and age ---------------------------------

# Coefficients and covariance matrix supplied in the task.
coefficients_54 <- c(
  intercept = 8.149,
  log_totexp = 2.884,
  nk = -1.217,
  age = -0.5699,
  age_squared = 0.005515
)

covariance_54 <- matrix(
  c(
    0.1462, -0.01774, 0.0002347,
    -0.01774, 0.03204, -0.0004138,
    0.0002347, -0.0004138, 0.000005438
  ),
  nrow = 3,
  byrow = TRUE,
  dimnames = list(
    c("nk", "age", "age_squared"),
    c("nk", "age", "age_squared")
  )
)

N_54 <- 1200
K_54 <- 5

coefficients_54
covariance_54


## 5.4a Effect of one additional child

# Write your code below.


## 5.4b Marginal effect of age at 25, 50 and 75

# Write your code below.


## 5.4c Age at which mean alcohol budget share is minimized

# Write your code below.


## 5.4d Summarize the findings

# Write your interpretation as comments below.


## 5.4e Intervals when the conditional errors are normal

# Write your answer as comments below.


## 5.4f Intervals when the conditional errors are not normal

# Write your answer as comments below.


# 5.18 Food, clothing and household size ------------------------------------

# Dataset: london5, downloaded in Part 0.
# wfood and wcloth are budget shares; totexp is total expenditure;
# nk is the number of children.


## 5.18a Expected signs

# Write your answer as comments below.


## 5.18b One-child households

# Estimate the food-share and clothing-share equations.
# Write your code below.


## 5.18c Lower-tail test for the food coefficient at 1%

# Write your code below.


## 5.18d Lower-tail test for the clothing coefficient at 1%

# Write your code below.


## 5.18e Two-child households and 95% confidence intervals

# Write your code below.


## 5.18f Interaction models using all observations

# Write your code below.


## 5.18g Compare the marginal log-expenditure effects

# Write your code and conclusion below.


# 5.21 Rainfall and technological change ------------------------------------

# Dataset: toody5, downloaded in Part 0.
# y is wheat yield in tonnes per hectare; rain is growing-season rainfall in
# decimeters; trend starts at 0 in 1950 and increases by 0.1 each year.


## 5.21a Estimate the yield model

# Model: y ~ trend + rain + rain^2 + trend:rain
# Write your code below.


## 5.21b Significance at 5% and 10%

# Write your code and conclusion below.


## 5.21c Interpret the coefficient signs

# Write your interpretation as comments below.


## 5.21d Marginal rainfall effects in 1959 and 1995

# Use rain = 2.98 and trend = 0.9 for 1959.
# Use rain = 4.797 and trend = 4.5 for 1995.
# Write your code below.


## 5.21e Rainfall that maximizes expected yield

# Find point estimates and 95% confidence intervals for 1959 and 1995.
# Write your code below.
