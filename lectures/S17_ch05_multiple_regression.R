# SOK-3081 | Chapter 5: Multiple Regression
# Run one section at a time. Read the comments before running the code.
# Slide footers refer to the numbered parts below. Run START UP and earlier
# parts first; then continue at the part named on the lecture slide.
# Parts 1-5 follow Session 1; Parts 6-10 follow Session 2.
# Slides marked "Review" return to an earlier result; do not restart the script.
# The aim is to interpret the results, not to memorise R commands.
# Data source: Hill, Griffiths and Lim, Principles of Econometrics, 5th ed.

# START UP -------------------------------------------------------------------

# Remove old objects so that we start with a clean workspace.
rm(list = ls())
# Show ordinary numbers rather than scientific notation where possible.
options(scipen = 10)
# Show about four significant digits in ordinary R output.
options(digits = 4)

library(tidyverse)
library(mosaic)

# Part 1: Data and the economic question -------------------------------------
# Lecture: Andy's decision; the data; the separate price and advertising plots.
# PREDICT: Should lower prices and higher advertising increase sales revenue?
# Revenue is not profit: production costs are not observed in this dataset.

load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/andy.rdata"))
# load() puts the data frame andy into the workspace.
# Do not write andy <- load(...): load() returns an object name, not the data.

head(andy)       # Look at the first six observations.
# sales: monthly revenue in THOUSANDS of dollars.
# price: the price index in dollars.
# advert: advertising expenditure in THOUSANDS of dollars.

# Each graph below contains just one scatterplot and one fitted line.
# ggplot(): choose the data. aes(): choose the horizontal and vertical axes.
# geom_point(): draw the observations.
# geom_smooth(method = "lm"): fit the least-squares straight line.
# se = FALSE: leave out the shaded confidence band to keep the graph simple.
# + adds another layer or a label to the same graph.

price_plot <- ggplot(andy, aes(x = price, y = sales)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(x = "Price (dollars)", y = "Sales (thousand dollars)")
print(price_plot)

# Price OLS
model_price <- lm(sales ~ price, data = andy)
summary(model_price)

advert_plot <- ggplot(andy, aes(x = advert, y = sales)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(x = "Advertising (thousand dollars)", y = "Sales (thousand dollars)")
print(advert_plot)

# Advertising OLS
model_advert <- lm(sales ~ advert, data = andy)
summary(model_advert)

# THINK: These are SIMPLE regressions. Neither line holds the other variable
# constant. They describe raw relationships, not the multiple-regression slopes.

# Part 2: Multiple regression: include price AND advertising ------------------
# Lecture: from one variable to two; OLS; the estimated coefficients.
# If we do one at a time, we might get a misleading picture. For example, if higher advertising is associated with lower prices, then the simple regression of sales on price will include some of the advertising effect. Multiple regression allows us to hold one variable constant while estimating the effect of another.




# The formula sales ~ price + advert says:
# explain sales using price and advertising. R includes an intercept.
# lm() chooses the coefficients that minimise the sum of squared residuals.

model <- lm(sales ~ price + advert, data = andy)
summary(model)

# In the output, Estimate gives the coefficient and Std. Error its uncertainty.
# The price coefficient holds advertising constant, and vice versa.
# coef() gives just the estimated coefficients.
b <- coef(model)
b

# Save the three numbers. [[1]] takes the first number, [[2]] the second, etc.
b1 <- b[[1]]   # Intercept.
b2 <- b[[2]]   # Price coefficient.
b3 <- b[[3]]   # Advertising coefficient.

# INTERPRET: With advertising fixed, a 10-cent price cut is a change of -0.10.
price_cut_gain <- -0.10 * b2
price_cut_gain * 1000     # Convert the predicted revenue gain to dollars.

# Advertising is in thousands: another 1,000 dollars is a change of 1.
advert_gain <- 1 * b3
advert_gain * 1000
# A revenue gain is not the same as a profit gain.

# Part 3: Predict sales at a particular price and advertising level ------------
# Lecture: A prediction by hand.
# 1,200 dollars of advertising must be entered as 1.20.

f_model <- makeFun(model)
predicted_sales <- f_model(price=5.5, advert=1.2)[[1]]
predicted_sales * 1000

# The same calculation by hand:
b1 + b2 * 5.50 + b3 * 1.20
# THINK: Is it sensible to use this equation at a price of zero?
# Our linear approximation describes the range covered by the observed data.

# Part 4: Residuals and the sum of squared errors ------------------------------
# Lecture: residuals; SSE; error variance; explained and unexplained variation.
# fitted() gives the model's predicted sales for each observed city.
# residuals() gives observed sales minus predicted sales: e_hat = y - y_hat.
fitted_sales <- fitted(model)
e_hat <- residuals(model)

# Look at the first city. [1] selects the first observation.
andy$sales[1]
fitted_sales[1]
e_hat[1]

# MATH: SSE = sum(e_hat_i^2).
# ^2 squares each residual. sum() adds all the squared residuals.
SSE <- sum(e_hat^2)
SSE
# Squaring prevents positive and negative errors from cancelling.

# N is the number of observations. K includes the intercept.
N <- nrow(andy)
K <- length(coef(model))
residual_df <- N - K

# MATH: estimated error variance = SSE / (N - K).
sigma2_hat <- SSE / (N - K)
sigma2_hat

# The total variation measures squared distances from the average sales value.
# MATH: R-squared = 1 - SSE / SST.
SST <- sum((andy$sales - mean(andy$sales))^2)
R_squared <- 1 - SSE / SST
R_squared
# About 44.8% of the sample variation in sales is explained by this model.
# This does not establish causality.

# Part 5: Holding price constant: the FWL idea --------------------------------
# Lecture: controlling for price; the three regressions; the partial relationship.
# First remove the part of sales that is linearly related to price.
sales_price_model <- lm(sales ~ price, data = andy)
sales_net_price <- residuals(sales_price_model)

# Then remove the part of advertising that is linearly related to price.
advert_price_model <- lm(advert ~ price, data = andy)
summary(advert_price_model)

advert_net_price <- residuals(advert_price_model)

# Regress the remaining sales variation on the remaining advertising variation.
# -1 removes the intercept: the two residual variables already have mean zero.
fwl_data <- tibble(sales_net_price, advert_net_price)
fwl_model <- lm(sales_net_price ~ advert_net_price - 1, data = fwl_data)
coef(fwl_model)
b3
# The two advertising coefficients are the same: this is the FWL theorem.

fwl_plot <- ggplot(fwl_data, aes(x = advert_net_price, y = sales_net_price)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x - 1, se = FALSE) +
  labs(x = "Advertising after removing price (thousand dollars)",
       y = "Sales after removing price (thousand dollars)")
print(fwl_plot)
# Use the full model for inference. The residual regression's default standard
# error uses different degrees of freedom, even though its slope is identical.

# Part 6: Precision, covariance and confidence intervals ----------------------
# Lecture: regressor correlation; covariance matrix; standard errors; intervals.
# First inspect the correlation between the two explanatory variables.
cor(andy$price, andy$advert)

# Then inspect the covariance matrix of the estimated coefficients.
# Diagonal entries are variances; off-diagonal entries are covariances.
V <- vcov(model)
V
sqrt(diag(V))
# Read a standard error from the model summary.
# ["advert", "Std. Error"] selects the advertising row and standard-error column.
advert_se <- coef(summary(model))["advert", "Std. Error"]
advert_se

ci95 <- confint(model, level = 0.95)
ci95
# Each row gives a lower and an upper endpoint for one coefficient.
# The advertising interval includes 1: a revenue gain of one dollar per
# advertising dollar is compatible with the data at this confidence level.

# Calculate the advertising interval using the formula from the lecture.
tc95 <- qt(0.975, df = residual_df)
b3 - tc95 * advert_se
b3 + tc95 * advert_se

# Part 7: Two questions, two hypothesis tests ---------------------------------
# Lecture: the two economic claims; test results; test calculations in R.
# QUESTION A: Is advertising related to sales? H0: beta_3 = 0; H1: beta_3 != 0.
# MATH: t = (estimate - hypothesised value) / standard error.
advert_zero_t <- (b3 - 0) / advert_se
advert_zero_p <- 2 * pt(-abs(advert_zero_t), df = residual_df)
advert_zero_p
# pt() gives a probability from the t-distribution.
# Here we count both tails, so we multiply the one-tail probability by 2.
# p < 0.05: reject the zero-effect null at the 5% level.

# QUESTION B: Does extra revenue cover advertising expenditure?
# H0: beta_3 <= 1; H1: beta_3 > 1. Evaluate the test at the boundary value 1.
advert_cover_t <- (b3 - 1) / advert_se
advert_cover_p <- pt(advert_cover_t, df = residual_df, lower.tail = FALSE)
advert_cover_p
# lower.tail = FALSE gives the probability ABOVE our t-value.
# p > 0.05: insufficient evidence for beta_3 > 1 at the 5% level.
# This does not prove that advertising fails. Production costs also matter.

# Part 8: Change price and advertising together -------------------------------
# Lecture: combined changes; the covariance term; the interval for the change.
# Cut price by 40 cents; increase advertising by 800 dollars.
# MATH: change in expected sales = -0.40 * beta_2 + 0.80 * beta_3.
policy_estimate <- -0.40 * b2 + 0.80 * b3
policy_estimate * 1000

# OPTIONAL CALCULATION: uncertainty about this combined change.
# vcov() contains coefficient variances on the diagonal and covariances off it.
# Select the relevant numbers by their row and column names.
V <- vcov(model)
var_price <- V["price", "price"]
var_advert <- V["advert", "advert"]
cov_price_advert <- V["price", "advert"]

# Write out the variance formula, just as in the hand calculation.
policy_variance <- (-0.40)^2 * var_price + 0.80^2 * var_advert +
  2 * (-0.40) * 0.80 * cov_price_advert
policy_se <- sqrt(policy_variance)

# A 90% interval leaves 5% in each tail; its upper critical value is qt(0.95).
tc90 <- qt(0.95, df = residual_df)
policy_lower <- policy_estimate - tc90 * policy_se
policy_upper <- policy_estimate + tc90 * policy_se
policy_lower * 1000
policy_upper * 1000
# Interpret this as uncertainty about a MEAN revenue change, not about profit.

# Part 9: Diminishing returns to advertising ----------------------------------
# Lecture: the quadratic model; advertising curve; the marginal effect.
# I(advert^2) tells R to include advertising squared as another regressor.
quadratic_model <- lm(sales ~ price + advert + I(advert^2), data = andy)
summary(quadratic_model)
bq <- coef(quadratic_model)
bq3 <- bq[[3]]   # Coefficient on advertising.
bq4 <- bq[[4]]   # Coefficient on advertising squared.

# OPTIONAL GRAPH: show this curve while holding price at 5.50 dollars.
# Cities have different prices. First subtract their estimated price contribution
# relative to 5.50. This adjustment uses the multiple-regression price coefficient.
# It is a model-based adjustment, not newly observed sales.
andy$sales_at_fixed_price <- andy$sales - bq[[2]] * (andy$price - 5.50)

# y ~ x + I(x^2) asks geom_smooth() for a quadratic least-squares curve.
# A straight line would use formula = y ~ x instead.
advertising_plot <- ggplot(andy, aes(x = advert, y = sales_at_fixed_price)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  labs(x = "Advertising (thousand dollars)",
       y = "Sales adjusted to price = 5.50 (thousand dollars)")
print(advertising_plot)
# The points are adjusted observations. The curve is the fitted conditional mean.

# MATH: marginal effect = beta_3 + 2 * beta_4 * advertising.
# At 500 dollars of advertising:
bq3 + 2 * bq4 * 0.5
# At 2,000 dollars of advertising:
bq3 + 2 * bq4 * 2
# The effect is smaller when advertising is already high.

# Part 10: An interaction: a short hand calculation ----------------------------
# Lecture: When one effect depends on another variable.
# Illustrative coefficients, NOT estimates from a wage dataset:
# E(wage | educ, exper) = 5 + educ + 0.20*exper + 0.05*educ*exper.
# The effect of one more education year is 1 + 0.05*experience.
1 + 0.05 * 0       # At zero years of experience.
1 + 0.05 * 20      # At twenty years of experience.

# PRESENTATION SUPPORT --------------------------------------------------------
# These small objects fill tables and labels in the Quarto slides.
# Students can stop above; this is formatting rather than a new analysis.

coefficient_table <- tibble(
  Term = c("Intercept", "Price", "Advertising"),
  Estimate = as.numeric(b), SE = coef(summary(model))[, "Std. Error"]
)
residual_example <- tibble(
  Observed = andy$sales[1], Fitted = fitted_sales[1], Residual = e_hat[1]
)
fwl_coefficient <- coef(fwl_model)[[1]]
fwl_naive_se <- coef(summary(fwl_model))[1, "Std. Error"]
test_comparison <- tibble(
  Claim = c("Advertising is related to sales", "Revenue covers advertising cost"),
  t = c(advert_zero_t, advert_cover_t), p = c(advert_zero_p, advert_cover_p)
)
policy_change <- c(estimate = policy_estimate, se = policy_se,
                   lower = policy_lower, upper = policy_upper)
marginal_effects <- tibble(
  Advertising = c(0.5, 1, 2, 3),
  Marginal_effect = bq3 + 2 * bq4 * Advertising
)
# Reference calculation: ignores additional production costs.
candidate_advert <- (1 - bq3) / (2 * bq4)
