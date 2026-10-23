
# Start up and enter the seven textbook observations ------------------
rm(list=ls())

library(tidyverse)

# These seven observations are printed in Exercise 5.1, Table 5.5.
# We enter them directly rather than load a separate dataset.

# Exercise 5.1

ex_data <-tibble(
  i = 1:7,
  y = c(1,1,4,0,1,-2,2),
  x_1 = c(1,1,1,1,1,1,1),
  x_2 = c(0,1,2,-2,1,-2,0),
  x_3 = c(1,-2,2,1,-2,-1,1)
)

ex_data

## 5.1.a

y_mean <- mean(ex_data$y)
x_1_mean <- mean(ex_data$x_1)
x_2_mean <- mean(ex_data$x_2)
x_3_mean <- mean(ex_data$x_3)



ex_data <- ex_data |>
  mutate(y_dm = y - y_mean,
         x_1_dm = x_1 - x_1_mean,
         x_2_dm = x_2 - x_2_mean,
         x_3_dm = x_3 - x_3_mean)

ex_data

# dm means deviation from the sample mean.
# The constant x_1 becomes zero when centered. We use x_2 and x_3 below.


## 5.1.b: Products and sums ---------------------------------------------------

# First multiply the deviations for each observation.
ex_data <- ex_data |>
  mutate(y_x2 = y_dm * x_2_dm,
         x2_squared = x_2_dm^2,
         y_x3 = y_dm * x_3_dm,
         x2_x3 = x_2_dm * x_3_dm,
         x3_squared = x_3_dm^2)

ex_data

# Then add the seven values in each column.
S_y2 <- sum(ex_data$y_x2)
S_22 <- sum(ex_data$x2_squared)
S_y3 <- sum(ex_data$y_x3)
S_23 <- sum(ex_data$x2_x3)
S_33 <- sum(ex_data$x3_squared)

S_y2  # 14
S_22  # 14
S_y3  # 9
S_23  # 0
S_33  # 16


## 5.1.c: Coefficient estimates ---------------------------------------------

# Both slope formulas in Appendix 5A have the same denominator.
denominator <- S_22 * S_33 - S_23^2

b2 <- (S_y2 * S_33 - S_y3 * S_23) / denominator
b3 <- (S_y3 * S_22 - S_y2 * S_23) / denominator
b1 <- y_mean - b2 * x_2_mean - b3 * x_3_mean

b1  # 1
b2  # 1
b3  # 0.5625

# Fitted equation: y_hat = 1 + 1*x_2 + 0.5625*x_3.


## 5.1.d: Fitted values and residuals ----------------------------------------

ex_data <- ex_data |>
  mutate(y_hat = b1 + b2 * x_2 + b3 * x_3,
         e_hat = y - y_hat)

ex_data |>
  select(i, y, y_hat, e_hat)

# A residual is observed y minus fitted y.
# Positive residual: the observation is above the fitted plane.
sum(ex_data$e_hat)  # Zero when the model includes an intercept.


## 5.1.e: Estimated error variance ------------------------------------------

ex_data <- ex_data |>
  mutate(e_hat_squared = e_hat^2)

SSE <- sum(ex_data$e_hat_squared)
N <- nrow(ex_data)
K <- 3  # Intercept and two slopes.
df <- N - K

sigma_squared_hat <- SSE / df

SSE                # 0.9375
df                 # 4
sigma_squared_hat  # 0.234375

# Divide by N - K, not N: we estimated three coefficients.


## 5.1.f: Correlation between the regressors ---------------------------------

r_23 <- S_23 / sqrt(S_22 * S_33)
r_23  # 0

# Check with R's correlation function.
cor(ex_data$x_2, ex_data$x_3)

# Zero sample correlation means no linear association in these observations.
# It does not establish statistical independence.


## 5.1.g: Standard error of b2 -----------------------------------------------

var_b2 <- sigma_squared_hat / ((1 - r_23^2) * S_22)
se_b2 <- sqrt(var_b2)

var_b2  # About 0.01674
se_b2   # About 0.1294

# The standard error describes uncertainty in the estimated slope.


## 5.1.h: Explained and unexplained variation --------------------------------

ex_data <- ex_data |>
  mutate(y_dm_squared = y_dm^2,
         explained_squared = (y_hat - y_mean)^2)

SST <- sum(ex_data$y_dm_squared)
SSR <- sum(ex_data$explained_squared)

SSE  # Unexplained sum of squares: 0.9375
SST  # Total sum of squares: 20
SSR  # Explained sum of squares: 19.0625

SSR + SSE  # Equals SST for OLS with an intercept.

R_squared <- 1 - SSE / SST
R_squared
SSR / SST  # The other formula gives the same answer.

# The model explains about 95.3% of sample variation in y around its mean.


# Exercise 5.2 ---------------------------------------------------------------
# Continue using b2, se_b2 and df from Exercise 5.1.
# The t calculations assume the usual model assumptions, including normal errors.

## 5.2.a: A 95% confidence interval for beta_2 --------------------------------

# qt() gives the critical value from the t distribution.
# A two-sided 95% interval leaves 2.5% in each tail.
t_critical <- qt(0.975, df = df)
t_critical  # About 2.7764, with four degrees of freedom.

lower <- b2 - t_critical * se_b2
upper <- b2 + t_critical * se_b2

lower  # About 0.6408
upper  # About 1.3592


## 5.2.b: Test whether beta_2 equals 1.25 -------------------------------------

# H0: beta_2 = 1.25. H1: beta_2 is not equal to 1.25.
# We use a 5% significance level and a two-sided test.
null_value <- 1.25
t_stat <- (b2 - null_value) / se_b2
t_stat  # About -1.9322

# abs() gives the absolute value.
# Reject H0 if the absolute t statistic exceeds the critical value.
abs(t_stat)
t_critical
abs(t_stat) > t_critical  # FALSE: do not reject at 5%.

# pt() gives a lower-tail probability. Double the tail for a two-sided test.
p_value <- 2 * pt(-abs(t_stat), df = df)
p_value  # About 0.1255, above 0.05.

# There is insufficient evidence that beta_2 differs from 1.25.
# This does not prove equality. Notice that 1.25 is also inside the 95% interval.


# Optional check: let lm() do the regression ----------------------------------

model <- lm(y ~ x_2 + x_3, data = ex_data)
summary(model)
confint(model, "x_2", level = 0.95)

# R includes an intercept automatically, so we do not add x_1 to the formula.
# summary(model) tests coefficients against ZERO, not against 1.25.


# Exercise 5.3: Read the supplied regression output ---------------------------
# WALC = beta_1 + beta_2*log(TOTEXP) + beta_3*NK + beta_4*AGE + e.
# WALC is the PERCENTAGE of the household budget spent on alcohol.
# NK is the number of children; AGE is the household head's age.
# This exercise uses the rounded output in Table 5.6, not a new regression.

## 5.3.a: Fill in the missing numbers ----------------------------------------

N_3 <- 1200
K_3 <- 4
df_3 <- N_3 - K_3

# (i) The intercept's t statistic: estimate divided by standard error.
b1_3 <- 1.4515
se_b1_3 <- 2.2019
t_b1_3 <- b1_3 / se_b1_3
t_b1_3  # About 0.6592

# (ii) Rearrange t = estimate / SE to find the missing standard error.
b2_3 <- 2.7648
t_b2_3 <- 5.7103
se_b2_3 <- b2_3 / t_b2_3
se_b2_3  # About 0.4842

# (iii) Rearrange the same formula to find the missing coefficient.
se_b3_3 <- 0.3695
t_b3_3 <- -3.9376
b3_3 <- t_b3_3 * se_b3_3
b3_3  # About -1.4549

# (iv) Recover SST from the sample standard deviation of WALC.
SSE_3 <- 46221.62
sd_y_3 <- 6.39547
SST_3 <- (N_3 - 1) * sd_y_3^2
R_squared_3 <- 1 - SSE_3 / SST_3
R_squared_3  # About 0.0575: 5.75% of the sample variation is explained.

# (v) The regression standard error is the square root of error variance.
sigma_squared_3 <- SSE_3 / df_3
sigma_hat_3 <- sqrt(sigma_squared_3)
sigma_hat_3  # About 6.2167 percentage points.


## 5.3.b: Interpret the coefficients -----------------------------------------

# Hold the other explanatory variables constant in each interpretation.
# log(TOTEXP): a 1% expenditure increase raises mean WALC by approximately
# b2 / 100 percentage points. It is not a b2 percent increase in WALC.
b2_3 / 100

# For comparison, the exact change for a 1% expenditure increase is:
b2_3 * log(1.01)

# One additional child: about 1.4549 percentage points lower mean WALC.
b3_3

# One additional year of age: about 0.1503 percentage points lower mean WALC.
b4_3 <- -0.1503
b4_3


## 5.3.c: A 95% confidence interval for the age coefficient -------------------

se_b4_3 <- 0.0235
critical_3 <- qt(0.975, df = df_3)
lower_age_3 <- b4_3 - critical_3 * se_b4_3
upper_age_3 <- b4_3 + critical_3 * se_b4_3

lower_age_3  # About -0.1964
upper_age_3  # About -0.1042

# The interval is entirely negative: higher age is associated with a lower
# mean alcohol budget share, controlling for expenditure and children.


## 5.3.d: Which coefficients differ from zero at 5%? --------------------------

# Use the t statistics supplied in the table, including its rounded age value.
output_3 <- tibble(
  variable = c("Intercept", "log(TOTEXP)", "NK", "AGE"),
  estimate = c(b1_3, b2_3, b3_3, b4_3),
  standard_error = c(se_b1_3, se_b2_3, se_b3_3, se_b4_3),
  t_statistic = c(t_b1_3, t_b2_3, t_b3_3, -6.4019)
)

output_3 <- output_3 |>
  mutate(p_value = 2 * pt(-abs(t_statistic), df = df_3),
         significant_5_percent = p_value < 0.05)

output_3

# All three slopes differ from zero at 5%; the intercept does not.
# A printed p-value of 0.0000 means very small, not literally zero.
# Rounded inputs can produce small differences from the book's output.


## 5.3.e: Does one extra child reduce the share by 2 percentage points? -------

# H0: beta_3 = -2. H1: beta_3 is not equal to -2.
# The null value is NEGATIVE because the question says "reduces".
t_child_3 <- (b3_3 - (-2)) / se_b3_3
p_child_3 <- 2 * pt(-abs(t_child_3), df = df_3)

t_child_3  # About 1.4751
p_child_3  # About 0.1404

# Do not reject at 5%. There is insufficient evidence of a difference from -2.
# This is compatible with rejecting a zero coefficient: the null values differ.


# Exercise 5.4: A quadratic age effect ---------------------------------------
# WALC = beta_1 + beta_2*log(TOTEXP) + beta_3*NK + beta_4*AGE
#        + beta_5*AGE^2 + e.
# Use the supplied estimates and covariance entries from Table 5.7.
# We do not need to load household data to solve this exercise.

b3_4 <- -1.217
b4_4 <- -0.5699
b5_4 <- 0.005515

var_b3_4 <- 0.1462
var_b4_4 <- 0.03204
var_b5_4 <- 0.000005438
cov_b4_b5_4 <- -0.0004138

df_4 <- 1200 - 5
critical_4 <- qt(0.975, df = df_4)


## 5.4.a: One additional child ----------------------------------------------

se_child_4 <- sqrt(var_b3_4)
lower_child_4 <- b3_4 - critical_4 * se_child_4
upper_child_4 <- b3_4 + critical_4 * se_child_4

b3_4           # -1.217 percentage points.
lower_child_4  # About -1.9672
upper_child_4  # About -0.4668

# More children are associated with a lower conditional mean alcohol share.


## 5.4.b: Marginal age effects at 25, 50 and 75 -------------------------------

# The derivative with respect to age is beta_4 + 2*AGE*beta_5.
# A vector lets R repeat the same calculation at all three ages.
age_4 <- c(25, 50, 75)
age_effect_4 <- b4_4 + 2 * age_4 * b5_4

# Variance of a sum includes the covariance term.
# Do not take the square root of the covariance by itself.
var_age_effect_4 <- var_b4_4 + (2 * age_4)^2 * var_b5_4 +
  2 * (2 * age_4) * cov_b4_b5_4
se_age_effect_4 <- sqrt(var_age_effect_4)

age_results_4 <- tibble(
  age = age_4,
  effect = age_effect_4,
  standard_error = se_age_effect_4,
  lower = age_effect_4 - critical_4 * se_age_effect_4,
  upper = age_effect_4 + critical_4 * se_age_effect_4
)

age_results_4

# Effects are about -0.2942, -0.0184 and 0.2574 percentage points per year.
# Only the interval at age 25 excludes zero.
# These are derivatives, not the exact changes for a full additional year.


## 5.4.c: The age at which mean alcohol share is minimized -------------------

# Set beta_4 + 2*age*beta_5 equal to zero and solve for age.
minimum_age_4 <- -b4_4 / (2 * b5_4)
minimum_age_4  # About 51.6682 years.

# Positive beta_5 means the curve bends upward: this is a minimum.
# A ratio is nonlinear. Use the delta method for an approximate SE.
# g4 and g5 are its derivatives with respect to beta_4 and beta_5.
g4_4 <- -1 / (2 * b5_4)
g5_4 <- b4_4 / (2 * b5_4^2)

var_minimum_4 <- g4_4^2 * var_b4_4 + g5_4^2 * var_b5_4 +
  2 * g4_4 * g5_4 * cov_b4_b5_4
se_minimum_4 <- sqrt(var_minimum_4)

se_minimum_4  # About 6.1410 years.
lower_minimum_4 <- minimum_age_4 - critical_4 * se_minimum_4
upper_minimum_4 <- minimum_age_4 + critical_4 * se_minimum_4

lower_minimum_4  # About 39.62 years.
upper_minimum_4  # About 63.72 years.


## 5.4.d: Summarize ----------------------------------------------------------

# More children: lower conditional mean alcohol budget share.
# At 25: the share decreases with age. The fitted minimum is around 52.
# At 50 and 75: the age effects are not significantly different from zero.
# The estimated minimum is uncertain: its interval spans about 40 to 64 years.
# These conditional associations do not automatically have a causal meaning.


## 5.4.e: Normally distributed errors ----------------------------------------

# Under MR1-MR6, (a) and (b) use exact finite-sample t intervals.
# They concern coefficients or linear combinations of coefficients.
# Part (c) concerns a nonlinear ratio: its delta-method interval is approximate.
# Using a t critical value does not make that nonlinear interval exact.


## 5.4.f: Errors that are not normally distributed ----------------------------

# None of the intervals is guaranteed to be exact in finite samples.
# With the other assumptions and suitable large-sample conditions, all three
# can be justified approximately. The ratio also requires beta_5 != 0.


# Exercise 5.5: Does a larger sample always identify the slope? ----------------
# Here we calculate theoretical variances, not estimates from a dataset.
# Both models have an intercept and one regressor.
# Under MR1-MR6: var(b2 | x) = sigma^2 / sum((x - mean(x))^2).

## 5.5.a: A linear time trend, x_t = t ---------------------------------------

# Compare increasing sample sizes. Set sigma^2 = 1 for illustration.
# For another error variance, multiply all slope variances by that value.
sample_size_5 <- c(10, 50, 100, 500, 1000)
sigma_squared_5 <- 1

# Use the supplied identities for the sum of t and the sum of t squared.
sum_t_5 <- sample_size_5 * (sample_size_5 + 1) / 2
sum_t_squared_5 <- sample_size_5 * (sample_size_5 + 1) *
  (2 * sample_size_5 + 1) / 6

Sxx_trend_5 <- sum_t_squared_5 - sum_t_5^2 / sample_size_5
var_trend_5 <- sigma_squared_5 / Sxx_trend_5

tibble(sample_size = sample_size_5, Sxx = Sxx_trend_5,
       slope_variance = var_trend_5)

# Algebra gives Sxx = T*(T^2 - 1)/12.
# Therefore var(b2 | x) = 12*sigma^2 / (T*(T^2 - 1)).
# The variance tends to ZERO as T increases.
# OLS is unbiased under the assumptions, so the slope is consistent here.


## 5.5.b: A shrinking regressor, x_t = (0.5)^t -------------------------------

# The geometric-series formulas give these two sums.
sum_x_5 <- 1 - 0.5^sample_size_5
sum_x_squared_5 <- (1 - 0.25^sample_size_5) / 3

Sxx_shrinking_5 <- sum_x_squared_5 - sum_x_5^2 / sample_size_5
var_shrinking_5 <- sigma_squared_5 / Sxx_shrinking_5

tibble(sample_size = sample_size_5, Sxx = Sxx_shrinking_5,
       slope_variance = var_shrinking_5)

# As T increases, Sxx tends to 1/3, so the variance tends to 3*sigma^2.
3 * sigma_squared_5  # The limit is 3 when sigma^2 = 1, not zero.

# Under MR6, the slope estimator is normal. Its variance does not vanish,
# so its distribution does not concentrate at the true slope: not consistent.


## 5.5.c: Intuition ----------------------------------------------------------

# With x_t = t, new observations extend the range of x.
# Increasing independent variation in x makes the slope more precise.
# With x_t = (0.5)^t, new x values are increasingly close to zero.
# They mainly help estimate the intercept and add little slope information.
# More observations alone are not enough: variation in the regressor matters.


# Exercise 5.6: Test linear combinations -------------------------------------
# Use the estimates and estimated covariance matrix supplied in the question.
# Diagonal entries are variances; off-diagonal entries are covariances.

b1_6 <- 2
b2_6 <- 3
b3_6 <- -1

var_b1_6 <- 3
var_b2_6 <- 4
var_b3_6 <- 3
cov_b1_b2_6 <- -2
cov_b1_b3_6 <- 1
cov_b2_b3_6 <- 0

df_6 <- 63 - 3
critical_6 <- qt(0.975, df = df_6)
critical_6  # About 2.0003 for a two-sided 5% test.


## 5.6.a: H0: beta_2 = 0 -----------------------------------------------------

se_b2_6 <- sqrt(var_b2_6)
t_a_6 <- (b2_6 - 0) / se_b2_6
p_a_6 <- 2 * pt(-abs(t_a_6), df = df_6)

se_b2_6  # 2
t_a_6    # 1.5
p_a_6    # About 0.1389: do not reject at 5%.


## 5.6.b: H0: beta_1 + 2*beta_2 = 5 -----------------------------------------

estimate_b_6 <- b1_6 + 2 * b2_6

# var(a*b1 + c*b2) = a^2*var(b1) + c^2*var(b2) + 2*a*c*cov(b1,b2).
# Here the weights are a = 1 and c = 2.
variance_b_6 <- var_b1_6 + 2^2 * var_b2_6 +
  2 * 1 * 2 * cov_b1_b2_6
se_b_6 <- sqrt(variance_b_6)
t_b_6 <- (estimate_b_6 - 5) / se_b_6
p_b_6 <- 2 * pt(-abs(t_b_6), df = df_6)

estimate_b_6  # 8
variance_b_6  # 11
t_b_6         # About 0.9045
p_b_6         # About 0.3693: do not reject at 5%.


## 5.6.c: H0: beta_1 - beta_2 + beta_3 = 4 -----------------------------------

estimate_c_6 <- b1_6 - b2_6 + b3_6

# The weights are 1, -1 and 1. Include all three covariance terms.
variance_c_6 <- 1^2 * var_b1_6 + (-1)^2 * var_b2_6 + 1^2 * var_b3_6 +
  2 * 1 * (-1) * cov_b1_b2_6 +
  2 * 1 * 1 * cov_b1_b3_6 +
  2 * (-1) * 1 * cov_b2_b3_6

se_c_6 <- sqrt(variance_c_6)
t_c_6 <- (estimate_c_6 - 4) / se_c_6
p_c_6 <- 2 * pt(-abs(t_c_6), df = df_6)

estimate_c_6  # -2
variance_c_6  # 16
t_c_6         # -1.5
p_c_6         # About 0.1389: do not reject at 5%.

# A negative covariance can contribute POSITIVELY when one weight is negative.
# Take the square root of the TOTAL variance to obtain a standard error.
# Do not add standard errors or take square roots of individual covariances.
# None of the three tests rejects, but this does not prove the nulls true.


# Exercise 5.7: Standard errors from summary information ----------------------
# Model: y = beta_1 + beta_2*x_2 + beta_3*x_3 + e.
# The numbers below are supplied in the question; no dataset is required.

N_7 <- 203
K_7 <- 3
df_7 <- N_7 - K_7
SSE_7 <- 6800
S_22_7 <- 1780.7
S_33_7 <- 3453.3
r_23_7 <- 0.7087
b2_7 <- 0.7176
b3_7 <- 1.0516


## 5.7.a: Standard errors of the slopes --------------------------------------

sigma_squared_7 <- SSE_7 / df_7
var_b2_7 <- sigma_squared_7 / ((1 - r_23_7^2) * S_22_7)
var_b3_7 <- sigma_squared_7 / ((1 - r_23_7^2) * S_33_7)
se_b2_7 <- sqrt(var_b2_7)
se_b3_7 <- sqrt(var_b3_7)

sigma_squared_7  # 34
se_b2_7          # About 0.19586
se_b3_7          # About 0.14064

# Holding the other quantities fixed, stronger correlation between the
# regressors increases these standard errors: less separate variation remains.


## 5.7.b: Is beta_2 different from zero? -------------------------------------

# H0: beta_2 = 0. H1: beta_2 != 0. Significance level: 5%.
t_b_7 <- b2_7 / se_b2_7
critical_b_7 <- qt(0.975, df = df_7)
p_b_7 <- 2 * pt(-abs(t_b_7), df = df_7)

t_b_7            # About 3.6639
critical_b_7     # About 1.9719
p_b_7            # About 0.00032
abs(t_b_7) > critical_b_7  # TRUE: reject at 5%.


## 5.7.c: Is beta_3 greater than 0.9? ----------------------------------------

# H0: beta_3 <= 0.9. H1: beta_3 > 0.9. Significance level: 10%.
# Calculate the statistic at the null boundary, beta_3 = 0.9.
t_c_7 <- (b3_7 - 0.9) / se_b3_7
critical_c_7 <- qt(0.90, df = df_7)
p_c_7 <- pt(t_c_7, df = df_7, lower.tail = FALSE)

t_c_7         # About 1.0779
critical_c_7  # About 1.2858 for an upper-tail 10% test.
p_c_7         # About 0.1412: do not reject at 10%.

# The estimate exceeds 0.9, but the evidence is insufficient to conclude
# that the population coefficient exceeds 0.9.


## 5.7.d: Are the two slopes equal? ------------------------------------------

# H0: beta_2 - beta_3 = 0. H1: beta_2 - beta_3 != 0.
# Significance level: 1%.
cov_b2_b3_7 <- -0.019521
difference_7 <- b2_7 - b3_7
var_difference_7 <- var_b2_7 + var_b3_7 - 2 * cov_b2_b3_7
se_difference_7 <- sqrt(var_difference_7)

t_d_7 <- difference_7 / se_difference_7
critical_d_7 <- qt(0.995, df = df_7)
p_d_7 <- 2 * pt(-abs(t_d_7), df = df_7)

difference_7     # -0.334
se_difference_7  # About 0.31174
t_d_7            # About -1.0714
critical_d_7     # About 2.6006
p_d_7            # About 0.2853: do not reject at 1%.

# Subtracting coefficients does NOT mean subtracting their variances.
# Here the negative covariance increases the variance of the difference.


# Exercise 5.8: Olympic medals in 1996 ---------------------------------------
# Use the supplied regression for 79 countries that won at least one medal.
# MEDALS = beta_1 + beta_2*POPM + beta_3*GDPB + e.
# POPM: population in millions. GDPB: GDP in billions of 1995 dollars.
# Assume MR1-MR6 for the tests and confidence interval.

b1_8 <- 5.917
b2_8 <- 0.01813
b3_8 <- 0.01026
se_b2_8 <- 0.00819
se_b3_8 <- 0.00136
R_squared_8 <- 0.4879
df_8 <- 79 - 3


## 5.8.a: Interpret the slopes ----------------------------------------------

# Holding GDP fixed, one million more people is associated with
# 0.01813 additional expected medals.
# For an increase of 100 million people:
100 * b2_8  # 1.813 expected medals.

# Holding population fixed, one billion dollars more GDP is associated with
# 0.01026 additional expected medals. For an extra 100 billion dollars:
100 * b3_8  # 1.026 expected medals.

# The expected count can be fractional even though actual medals are integers.
# These results concern medal-winning countries, not automatically all countries.


## 5.8.b: Interpret R-squared ------------------------------------------------

100 * R_squared_8        # 48.79% explained.
100 * (1 - R_squared_8)  # 51.21% unexplained.

# Population and GDP explain 48.79% of sample variation in medal counts
# around their sample mean. This is not a percentage of correct predictions.


## 5.8.c: A positive GDP coefficient -----------------------------------------

# H0: beta_3 = 0. H1: beta_3 > 0. Use an UPPER-tail test.
t_gdp_8 <- b3_8 / se_b3_8
p_gdp_8 <- pt(t_gdp_8, df = df_8, lower.tail = FALSE)
critical_1_percent_8 <- qt(0.99, df = df_8)
critical_5_percent_8 <- qt(0.95, df = df_8)

t_gdp_8               # About 7.5441
p_gdp_8               # About 4.09e-11: reject at both 1% and 5%.
critical_1_percent_8  # About 2.3764
critical_5_percent_8  # About 1.6652


## 5.8.d: A positive population coefficient ----------------------------------

# H0: beta_2 = 0. H1: beta_2 > 0. Again use the upper tail.
t_population_8 <- b2_8 / se_b2_8
p_population_8 <- pt(t_population_8, df = df_8, lower.tail = FALSE)

t_population_8  # About 2.2137
p_population_8  # About 0.01493

t_population_8 > critical_1_percent_8  # FALSE: do not reject at 1%.
t_population_8 > critical_5_percent_8  # TRUE: reject at 5%.

# The data do not change. Only the threshold for rejecting the null changes.


## 5.8.e: Test four possible population coefficients --------------------------

# Each H0: beta_2 = null value. Each H1: beta_2 != null value.
# A vector repeats the same two-sided calculation for all four null values.
null_values_8 <- c(0.01, 0.02, 0.03, 0.04)
t_values_8 <- (b2_8 - null_values_8) / se_b2_8
p_values_8 <- 2 * pt(-abs(t_values_8), df = df_8)

tests_8 <- tibble(
  null_value = null_values_8,
  t_statistic = t_values_8,
  p_value = p_values_8,
  reject_at_5_percent = p_values_8 < 0.05
)

tests_8

# Do not reject 0.01, 0.02 or 0.03. Reject 0.04.
# This does not mean beta_2 equals three different values.
# Several values can be compatible with an imprecise estimate.


## 5.8.f: A 95% confidence interval for beta_2 --------------------------------

critical_two_sided_8 <- qt(0.975, df = df_8)
lower_population_8 <- b2_8 - critical_two_sided_8 * se_b2_8
upper_population_8 <- b2_8 + critical_two_sided_8 * se_b2_8

lower_population_8  # About 0.001818
upper_population_8  # About 0.034442

# Units: expected medals per additional million people, holding GDP fixed.
# The nonrejected values from (e) are inside this interval; 0.04 is outside.
# This connects a two-sided 5% test with a 95% confidence interval.


# Exercise 5.9: Olympic medals in a level-log model ---------------------------
# The supplied regression uses 63 medal-winning countries, excluding the UK.
# MEDALS = beta_1 + beta_2*log(POPM) + beta_3*log(GDPB) + e.
# POPM is millions of people; GDPB is billions of 1995 dollars.
# log() in R is the natural logarithm.

b1_9 <- -13.153
b2_9 <- 2.764
b3_9 <- 4.270
se_b2_9 <- 2.070
se_b3_9 <- 1.718
df_9 <- 63 - 3


## 5.9.a: Interpret the slopes ----------------------------------------------

# Holding GDP fixed, a 1% population increase corresponds to approximately:
b2_9 / 100  # 0.02764 additional expected medals.

# Holding population fixed, a 1% GDP increase corresponds to approximately:
b3_9 / 100  # 0.04270 additional expected medals.

# These are changes in medal COUNTS, not percentage changes in medals.
# For an exact finite change, multiply by log(new value / old value).
b3_9 * log(1.10)  # About 0.40697 more expected medals for 10% higher GDP.


## 5.9.b: Interpret R-squared ------------------------------------------------

R_squared_9 <- 0.275
100 * R_squared_9  # 27.5% of sample variation around mean medals is explained.

# We cannot compare functional forms cleanly with Exercise 5.8:
# the Olympic year and sample also differ.


## 5.9.c: Is the GDP coefficient positive? -----------------------------------

# H0: beta_3 = 0. H1: beta_3 > 0. Upper-tail test.
t_gdp_9 <- b3_9 / se_b3_9
p_gdp_9 <- pt(t_gdp_9, df = df_9, lower.tail = FALSE)
critical_10_percent_9 <- qt(0.90, df = df_9)
critical_5_percent_9 <- qt(0.95, df = df_9)

t_gdp_9  # About 2.4854
p_gdp_9  # About 0.00787: reject at both 10% and 5%.


## 5.9.d: Is the population coefficient positive? ----------------------------

# H0: beta_2 = 0. H1: beta_2 > 0.
t_population_9 <- b2_9 / se_b2_9
p_population_9 <- pt(t_population_9, df = df_9, lower.tail = FALSE)

t_population_9  # About 1.3353
p_population_9  # About 0.09342
t_population_9 > critical_10_percent_9  # TRUE: reject at 10%.
t_population_9 > critical_5_percent_9   # FALSE: do not reject at 5%.


## 5.9.e: Expected medals for the United Kingdom -----------------------------

# Insert population and GDP in the stated units BEFORE taking logs.
population_uk_9 <- 58
gdp_uk_9 <- 1010
expected_medals_9 <- b1_9 + b2_9 * log(population_uk_9) +
  b3_9 * log(gdp_uk_9)

# The question supplies the SE of the estimated conditional mean.
se_mean_9 <- 4.22196
critical_9 <- qt(0.975, df = df_9)
lower_mean_9 <- expected_medals_9 - critical_9 * se_mean_9
upper_mean_9 <- expected_medals_9 + critical_9 * se_mean_9

expected_medals_9  # About 27.60867
lower_mean_9       # About 19.16349
upper_mean_9       # About 36.05384

# This is a confidence interval for expected medals.
# A prediction interval for an actual medal count also includes a new error.


## 5.9.f: Compare with 20 medals ---------------------------------------------

# Follow the question's test: H0: expected medals = 20; H1: not equal to 20.
t_uk_9 <- (expected_medals_9 - 20) / se_mean_9
p_uk_9 <- 2 * pt(-abs(t_uk_9), df = df_9)

t_uk_9  # About 1.8022
p_uk_9  # About 0.07654: do not reject at 5%, but reject at 10%.

# The estimate is about eight medals above the observed count.
# Failure to reject does not establish that the model predicts well.
# The actual 20 medals are an outcome, not a known conditional mean.
# To assess uncertainty in that outcome, we would need a prediction interval.


## 5.9.g: Write the standard-error expression --------------------------------

# Let a = log(58), c = log(1010). The estimated mean is b1 + a*b2 + c*b3.
# Its variance is:
# var(b1) + a^2*var(b2) + c^2*var(b3)
# + 2*a*cov(b1,b2) + 2*c*cov(b1,b3) + 2*a*c*cov(b2,b3).
# The standard error is the square root of this TOTAL variance.
# The question asks only for the expression, not a numerical calculation.
# Individual coefficient SEs are not enough: we also need their covariances.


# Exercise 5.10: Wheat yield, rainfall and a time trend -----------------------
# Use the supplied output for 1950-1996: 47 observations.
# YIELD is tonnes per hectare; RAIN is inches during the growing season.
# t = 1 in 1950 and t = 47 in 1996.
# The question assumes MR1-MR5. Without MR6, t inference is approximate.

b1_10 <- 0.1717
b2_10 <- 0.01117
b3_10 <- 0.05238
se_b2_10 <- 0.00262
se_b3_10 <- 0.01367
df_linear_10 <- 47 - 3


## 5.10.a: Interpret the linear model ----------------------------------------

# Holding rainfall fixed, one more year is associated with 0.01117 more
# tonnes per hectare in mean yield. The trend aims to capture technology,
# but could also reflect other gradual changes omitted from the model.
# Holding the year fixed, an extra inch of rain adds 0.05238 tonnes per hectare.


## 5.10.b: Does the annual trend effect exceed 0.01? --------------------------

# H0: beta_2 <= 0.01. H1: beta_2 > 0.01. Significance level: 5%.
t_trend_linear_10 <- (b2_10 - 0.01) / se_b2_10
p_trend_linear_10 <- pt(t_trend_linear_10, df = df_linear_10,
                        lower.tail = FALSE)

t_trend_linear_10  # About 0.4466
p_trend_linear_10  # About 0.3287: do not reject at 5%.


## 5.10.c: Does the rainfall effect differ from 0.03? -------------------------

# H0: beta_3 = 0.03. H1: beta_3 != 0.03. Two-sided 5% test.
t_rain_linear_10 <- (b3_10 - 0.03) / se_b3_10
p_rain_linear_10 <- 2 * pt(-abs(t_rain_linear_10), df = df_linear_10)

t_rain_linear_10  # About 1.6372
p_rain_linear_10  # About 0.1087: do not reject at 5%.


## 5.10.d: Add squared rainfall ---------------------------------------------

# Keep separate names for the supplied QUADRATIC model estimates.
q1_10 <- -0.6759
q2_10 <- 0.011671
q3_10 <- 0.2229
q4_10 <- -0.008155
se_q2_10 <- 0.00250
se_q3_10 <- 0.0734
se_q4_10 <- 0.003453
cov_q3_q4_10 <- -0.0002493
df_quadratic_10 <- 47 - 4

# Rainfall's marginal effect is now beta_3 + 2*RAIN*beta_4.
# The negative squared term allows diminishing benefits and damage from
# excessive rain. This is the expected sign for a concave yield curve.


## 5.10.e: Repeat the trend test with the quadratic model --------------------

t_trend_quadratic_10 <- (q2_10 - 0.01) / se_q2_10
p_trend_quadratic_10 <- pt(t_trend_quadratic_10, df = df_quadratic_10,
                           lower.tail = FALSE)

t_trend_quadratic_10  # About 0.6684
p_trend_quadratic_10  # About 0.2537: still do not reject at 5%.


## 5.10.f: Test the marginal rainfall effect at mean rainfall -----------------

mean_rain_10 <- 10.059
rain_effect_10 <- q3_10 + 2 * mean_rain_10 * q4_10
var_rain_effect_10 <- se_q3_10^2 + (2 * mean_rain_10)^2 * se_q4_10^2 +
  2 * (2 * mean_rain_10) * cov_q3_q4_10
se_rain_effect_10 <- sqrt(var_rain_effect_10)

# H0: the marginal effect at mean rainfall = 0.03. H1: not equal to 0.03.
t_rain_quadratic_10 <- (rain_effect_10 - 0.03) / se_rain_effect_10
p_rain_quadratic_10 <- 2 * pt(-abs(t_rain_quadratic_10),
                              df = df_quadratic_10)

rain_effect_10        # About 0.05884
se_rain_effect_10     # About 0.01351
t_rain_quadratic_10   # About 2.1349
p_rain_quadratic_10   # About 0.03851: reject at 5%.

# Do not test q3 alone: it is the rainfall slope at ZERO rainfall.
# Keep the covariance term and avoid rounding before the final result.


## 5.10.g: Forecast 1997 with observed rainfall -------------------------------

trend_1997_10 <- 48
rain_1997_10 <- 9.48
forecast_observed_10 <- q1_10 + q2_10 * trend_1997_10 +
  q3_10 * rain_1997_10 + q4_10 * rain_1997_10^2

forecast_observed_10  # About 1.26451 tonnes per hectare.


## 5.10.h: Forecast before observing rainfall --------------------------------

# Assume 1997 rainfall has the historical mean and variance.
sd_rain_10 <- 2.624
forecast_linear_10 <- b1_10 + b2_10 * trend_1997_10 +
  b3_10 * mean_rain_10

# E(RAIN^2) = E(RAIN)^2 + var(RAIN), not just E(RAIN)^2.
expected_rain_squared_10 <- mean_rain_10^2 + sd_rain_10^2
forecast_quadratic_10 <- q1_10 + q2_10 * trend_1997_10 +
  q3_10 * mean_rain_10 + q4_10 * expected_rain_squared_10

forecast_linear_10     # About 1.23475
forecast_quadratic_10  # About 1.24516

# For comparison, just inserting mean rainfall gives a different quantity:
yield_at_mean_rain_10 <- q1_10 + q2_10 * trend_1997_10 +
  q3_10 * mean_rain_10 + q4_10 * mean_rain_10^2
yield_at_mean_rain_10  # About 1.30131

# The curve is concave. Averaging over uncertain rainfall gives lower yield
# than evaluating the curve only at mean rainfall.


# Exercise 5.11: Education, experience and wages ------------------------------
# WAGE = beta_1 + beta_2*EDUC + beta_3*EXPER + beta_4*EXPER^2 + e.
# Use the supplied estimates from 600 observations. EDUC and EXPER are years.

b1_11 <- -16.308
b2_11 <- 2.329
b3_11 <- 0.5240
b4_11 <- -0.007582
se_b2_11 <- 0.163
se_b3_11 <- 0.1263
se_b4_11 <- 0.002532
cov_b3_b4_11 <- -0.00030526
df_11 <- 600 - 4
critical_11 <- qt(0.975, df = df_11)


## 5.11.a: The marginal effect of experience ---------------------------------

# Differentiate with respect to EXPER, holding EDUC constant:
# marginal experience effect = beta_3 + 2*beta_4*EXPER.
# The squared term matters: beta_3 alone is the effect at zero experience.


## 5.11.b: Expected signs ----------------------------------------------------

# beta_2 > 0: more education is expected to raise mean wages.
# beta_3 > 0: initial experience is expected to raise mean wages.
# beta_4 < 0: the experience gain diminishes and can eventually turn negative.
# All three estimates have these signs.


## 5.11.c: When does mean wage start to decline? ------------------------------

# Set beta_3 + 2*beta_4*EXPER equal to zero and solve.
peak_experience_11 <- -b3_11 / (2 * b4_11)
peak_experience_11  # About 34.5555 years.

# With negative beta_4, this is a maximum. The fitted slope is negative beyond it.


## 5.11.d.i: A 95% interval for the education effect --------------------------

lower_education_11 <- b2_11 - critical_11 * se_b2_11
upper_education_11 <- b2_11 + critical_11 * se_b2_11

lower_education_11  # About 2.00888
upper_education_11  # About 2.64912

# Units: wage units per extra year of education, holding experience fixed.


## 5.11.d.ii-iii: Experience effects at 4 and 25 years -------------------------

experience_11 <- c(4, 25)
experience_effect_11 <- b3_11 + 2 * experience_11 * b4_11
var_experience_effect_11 <- se_b3_11^2 +
  (2 * experience_11)^2 * se_b4_11^2 +
  2 * (2 * experience_11) * cov_b3_b4_11
se_experience_effect_11 <- sqrt(var_experience_effect_11)

experience_results_11 <- tibble(
  experience = experience_11,
  effect = experience_effect_11,
  standard_error = se_experience_effect_11,
  lower = experience_effect_11 - critical_11 * se_experience_effect_11,
  upper = experience_effect_11 + critical_11 * se_experience_effect_11
)

experience_results_11

# At 4 years: effect about 0.46334, with interval [0.25294, 0.67375].
# At 25 years: effect about 0.14490, with interval [0.07003, 0.21977].
# Both intervals are positive; the estimated gain is smaller at 25 years.


## 5.11.d.iv: A 95% interval for experience at the wage maximum ----------------

# As in 5.4.c, a ratio needs a delta-method approximation.
# Derivatives of -beta_3/(2*beta_4), evaluated at the estimates:
g3_11 <- -1 / (2 * b4_11)
g4_11 <- b3_11 / (2 * b4_11^2)

var_peak_11 <- g3_11^2 * se_b3_11^2 + g4_11^2 * se_b4_11^2 +
  2 * g3_11 * g4_11 * cov_b3_b4_11
se_peak_11 <- sqrt(var_peak_11)
lower_peak_11 <- peak_experience_11 - critical_11 * se_peak_11
upper_peak_11 <- peak_experience_11 + critical_11 * se_peak_11

se_peak_11     # About 4.3640 years.
lower_peak_11  # About 25.98484 years.
upper_peak_11  # About 43.12622 years.

# This nonlinear interval is approximate even with normally distributed errors.
# The peak concerns the fitted mean wage curve, not every worker's career.


# Exercise 5.12: Control for house age using residuals ------------------------
# PRICE is in thousands of dollars; SQFT is in hundreds of square feet.
# First regress PRICE on AGE and save residuals v_hat.
# Then regress SQFT on AGE and save residuals u_hat.
# Both preliminary regressions include an intercept.
# The question supplies their sums of squares and cross-products.

N_12 <- 850
K_12 <- 3
df_12 <- N_12 - K_12
sum_v_squared_12 <- 10377817
sum_u_squared_12 <- 75773.4
sum_uv_12 <- 688318


## 5.12.a: Estimate the size slope after controlling for age ------------------

# Regress the price residuals on the size residuals without an intercept.
# The FWL theorem gives the same size slope as PRICE ~ SQFT + AGE.
b2_12 <- sum_uv_12 / sum_u_squared_12
b2_12  # About 9.08390 thousand dollars per additional 100 square feet.

# u_hat is the part of size not explained by age.
# v_hat is the part of price not explained by age.
# Their relationship isolates the size slope after controlling for age.
b2_12 * 1000 / 100  # About 90.84 dollars per additional square foot.


## 5.12.b: Show the residual sum-of-squares identity --------------------------

# e_hat = v_hat - b2*u_hat. Squaring and summing gives:
# SSE = sum(v_hat^2) - 2*b2*sum(u_hat*v_hat) + b2^2*sum(u_hat^2).
# From (a), b2*sum(u_hat^2) = sum(u_hat*v_hat).
# Therefore the final two terms simplify to -b2*sum(u_hat*v_hat).

SSE_expanded_12 <- sum_v_squared_12 - 2 * b2_12 * sum_uv_12 +
  b2_12^2 * sum_u_squared_12
SSE_12 <- sum_v_squared_12 - b2_12 * sum_uv_12

SSE_expanded_12
SSE_12  # Same result: about 4125205.0132.


## 5.12.c: Estimate the error variance ---------------------------------------

# Count all three parameters in the FULL model: intercept, size and age.
sigma_squared_12 <- SSE_12 / df_12
sigma_squared_12  # About 4870.3719.


## 5.12.d: Standard error of the size slope ----------------------------------

# Use the size variation remaining after removing age.
se_b2_12 <- sqrt(sigma_squared_12 / sum_u_squared_12)
se_b2_12  # About 0.253526.


## 5.12.e: Is the size slope below 9.5? ---------------------------------------

# H0: beta_2 >= 9.5. H1: beta_2 < 9.5. Calculate at the null boundary.
t_size_12 <- (b2_12 - 9.5) / se_b2_12
p_size_12 <- pt(t_size_12, df = df_12)

t_size_12  # About -1.64125.
p_size_12  # About 0.05056: just above 0.05.

# Do not reject at 5%; reject at 10%.
# Do not round the p-value to 0.05 before deciding.


# Exercise 5.13: Okun's law --------------------------------------------------
# U is the unemployment rate; G is quarterly economic growth in percent.
# DU = U_t - U_(t-1) = -gamma*(G - G_N).
# G_N is growth consistent with unchanged unemployment.
# These calculations use the supplied regression and covariance matrix.


## 5.13.a: Rewrite the equation ---------------------------------------------

# Expanding gives DU = gamma*G_N - gamma*G.
# In DU = beta_1 + beta_2*G + e:
# beta_1 = gamma*G_N and beta_2 = -gamma.


## 5.13.b: Estimate gamma and normal growth ----------------------------------

b1_13 <- 0.1989
b2_13 <- -0.2713
sigma_hat_13 <- 0.2749
var_b1_13 <- 0.0007212
var_b2_13 <- 0.0006113
cov_b1_b2_13 <- -0.0004277

# 1970 Q1 through 2014 Q4: 45 years, with four quarters per year.
N_13 <- 45 * 4
df_13 <- N_13 - 2
critical_13 <- qt(0.975, df = df_13)

gamma_13 <- -b2_13
normal_growth_13 <- -b1_13 / b2_13

gamma_13          # 0.2713.
normal_growth_13  # About 0.73314 percent per quarter.

# Growth one percentage point above normal is associated with a 0.2713
# percentage-point fall in unemployment over the quarter.


## 5.13.c: Standard errors and tests against zero ----------------------------

se_b1_13 <- sqrt(var_b1_13)
se_b2_13 <- sqrt(var_b2_13)
se_gamma_13 <- se_b2_13  # Changing sign does not change the variance.

# For normal growth, -beta_1/beta_2, use the delta method.
g1_13 <- -1 / b2_13
g2_13 <- b1_13 / b2_13^2
var_normal_growth_13 <- g1_13^2 * var_b1_13 + g2_13^2 * var_b2_13 +
  2 * g1_13 * g2_13 * cov_b1_b2_13
se_normal_growth_13 <- sqrt(var_normal_growth_13)

results_13 <- tibble(
  quantity = c("beta_1", "beta_2", "gamma", "normal growth"),
  estimate = c(b1_13, b2_13, gamma_13, normal_growth_13),
  standard_error = c(se_b1_13, se_b2_13, se_gamma_13, se_normal_growth_13)
)

results_13 <- results_13 |>
  mutate(t_statistic = estimate / standard_error,
         p_value = 2 * pt(-abs(t_statistic), df = df_13),
         significant_5_percent = p_value < 0.05)

results_13  # All four differ from zero at 5%.

# The normal-growth ratio uses an approximation, even with normal errors.
# Exact t inference for the linear quantities additionally requires MR6.


## 5.13.d: Is normal growth 0.8% per quarter? ---------------------------------

# H0: G_N = 0.8. H1: G_N != 0.8. Two-sided 5% test.
t_growth_13 <- (normal_growth_13 - 0.8) / se_normal_growth_13
p_growth_13 <- 2 * pt(-abs(t_growth_13), df = df_13)

t_growth_13  # About -0.88237.
p_growth_13  # About 0.3788: do not reject at 5%.

# Optional: the same null can be written as beta_1 + 0.8*beta_2 = 0.
# This avoids taking a ratio and tests a linear combination instead.
growth_restriction_13 <- b1_13 + 0.8 * b2_13
se_restriction_13 <- sqrt(var_b1_13 + 0.8^2 * var_b2_13 +
  2 * 0.8 * cov_b1_b2_13)
t_restriction_13 <- growth_restriction_13 / se_restriction_13
p_restriction_13 <- 2 * pt(-abs(t_restriction_13), df = df_13)

p_restriction_13  # About 0.3818: same decision.

# The small difference comes from the nonlinear approximation in the first test.


## 5.13.e: A 95% confidence interval for gamma --------------------------------

lower_gamma_13 <- gamma_13 - critical_13 * se_gamma_13
upper_gamma_13 <- gamma_13 + critical_13 * se_gamma_13

lower_gamma_13  # About 0.22251.
upper_gamma_13  # About 0.32009.


## 5.13.f: A 95% interval for expected unemployment ---------------------------

previous_unemployment_13 <- 5.7991
growth_2015_13 <- 0.062  # Enter as given, without multiplying/dividing by 100.

predicted_change_13 <- b1_13 + b2_13 * growth_2015_13
predicted_unemployment_13 <- previous_unemployment_13 + predicted_change_13

# Treat previous unemployment and the stated growth input as fixed.
var_mean_13 <- var_b1_13 + growth_2015_13^2 * var_b2_13 +
  2 * growth_2015_13 * cov_b1_b2_13
se_mean_13 <- sqrt(var_mean_13)
lower_mean_13 <- predicted_unemployment_13 - critical_13 * se_mean_13
upper_mean_13 <- predicted_unemployment_13 + critical_13 * se_mean_13

predicted_unemployment_13  # About 5.98118%.
lower_mean_13              # About 5.93008%.
upper_mean_13              # About 6.03228%.

# This interval describes uncertainty about the conditional MEAN.


## 5.13.g: A 95% prediction interval for the actual quarter --------------------

# A new quarter also has its own regression error.
# Add its variance, assuming it is uncorrelated with the estimation errors.
se_prediction_13 <- sqrt(var_mean_13 + sigma_hat_13^2)
lower_prediction_13 <- predicted_unemployment_13 -
  critical_13 * se_prediction_13
upper_prediction_13 <- predicted_unemployment_13 +
  critical_13 * se_prediction_13

lower_prediction_13  # About 5.43630%.
upper_prediction_13  # About 6.52606%.

# Same center, wider interval: it includes uncertainty from a new quarterly shock.
# Serial correlation would require a different forecasting calculation.


# Exercise 5.14: Which normality assumption matters? --------------------------
# Model: y_i = beta_1 + beta_2*x_i + e_i, with independently sampled pairs.
# This is mainly a theory exercise. We use comments for the explanations,
# and a few numerical calculations for the population moments.
# Assume nondegenerate regressors and the required moments exist.


## 5.14.a: Log-normal x, conditionally normal errors --------------------------

# x is log-normal, but e | x is normal with mean zero and variance sigma_e^2.

# (i) Unbiased? YES, for both coefficients.
# Conditional mean-zero errors give E(b_j | x) = beta_j.
# Averaging over random x also gives E(b_j) = beta_j.

# (ii) Consistent? YES, under the stated sampling and moment conditions.
# The regressor has positive variance and finite moments, and cov(x,e) = 0.

# (iii) Conditionally normal? YES.
# With x fixed, the estimates are linear combinations of independent normal errors.
# In particular: b2 | x ~ N(beta_2, sigma_e^2 / Sxx).

# (iv) Unconditionally normal? NOT IN GENERAL.
# Different x samples give different coefficient covariance matrices.
# Mixing those conditional normals generally does not give a single normal.

# (v) Exact t tests in finite samples? YES, under these assumptions.
# Conditional on any full-rank x sample, the statistic has a t_(N-2) distribution.
# That distribution is the same for every x sample, so it also holds marginally.
# A normal histogram of x is NOT required for exact regression t inference.

# (vi) Asymptotic variance of the slope when x = exp(w), w ~ N(0,1).
mean_x_14 <- exp(0.5)
var_x_14 <- (exp(1) - 1) * mean_x_14^2

mean_x_14  # About 1.64872.
var_x_14   # exp(2) - exp(1), about 4.67077.
1 / var_x_14  # About 0.21410.

# Therefore Avar(b2) = sigma_e^2 / (N * var(x))
#                    = sigma_e^2 / (N * (exp(2) - exp(1))).
# This is about 0.21410*sigma_e^2/N.
# The limiting variance of sqrt(N)*(b2 - beta_2) omits the factor 1/N.


## 5.14.b: Normal x, conditionally log-normal errors --------------------------

# Now x ~ N(0,1). The conditional log-normal error distribution has the
# SAME parameters mu_v and sigma_v^2 at every x value.
# Its mean is mu_e = exp(mu_v + sigma_v^2/2), which is positive.
# Its variance is sigma_e^2 = (exp(sigma_v^2) - 1)*mu_e^2.

# (i) Add and subtract mu_e:
# y_i = (beta_1 + mu_e) + beta_2*x_i + (e_i - mu_e).
# Define beta_1_star = beta_1 + mu_e and e_star = e - mu_e.
# The slope parameter stays unchanged.

# (ii) E(e_star | x) = E(e | x) - mu_e = 0.
# var(e_star | x) = var(e | x) = sigma_e^2.
# Subtracting a constant changes the mean, not the variance.
# It does NOT turn the log-normal error into a normal error.

# (iii) Is b2 unbiased? YES.
# The centered model has conditional mean-zero errors.
# The fitted intercept estimates beta_1_star, not the original beta_1.

# (iv) Is b2 consistent? YES, under the stated sampling and moment conditions.
# Consistency does not require normal errors.

# (v) Is b2 conditionally normal? NOT IN GENERAL in a finite sample.
# Its estimation error is a weighted sum of centered log-normal variables.

# (vi) Is b2 unconditionally normal? NOT IN GENERAL in a finite sample.
# A normal regressor does not make non-normal errors normal.
# Large-sample approximate normality is a different result.

# (vii) Exact finite-sample t tests? NO, not generally.
# With the other assumptions and a sufficiently large sample, use approximate
# inference. A normal distribution for x does not replace normality of errors.

# (viii) Asymptotic slope variance:
# Avar(b2) = sigma_e^2 / (N * var(x)) = sigma_e^2/N, since var(x) = 1.
# The limiting variance of sqrt(N)*(b2 - beta_2) is sigma_e^2.

# Main lesson: conditional error normality gives exact small-sample inference.
# Regressor normality is neither required for it nor sufficient to provide it.


# Exercise 5.15: Zero covariance is not conditional mean zero -----------------
# Model: y = beta_1 + beta_2*x + e, with independently sampled pairs.
# x ~ N(0,1), and E(e | x) = c*(x^2 - 1).
# This is mainly a theory exercise; no dataset is required.


## 5.15.a: Show that E(e) = 0 ------------------------------------------------

# Use iterated expectations: E(e) = E[E(e | x)].
# Therefore E(e) = c*(E(x^2) - 1) = c*(1 - 1) = 0.
# This uses E(x^2) = 1 for a standard normal variable.
# If c != 0, E(e | x) still varies with x and is not identically zero.


## 5.15.b: Show that cov(e,x) = 0 --------------------------------------------

# Since E(x) = 0, the supplied identity gives:
# cov(e,x) = E[x*E(e | x)] = c*E[x*(x^2 - 1)]
#          = c*(E(x^3) - E(x)) = c*(0 - 0) = 0.
# The conditional relationship is quadratic and symmetric around zero.
# Zero LINEAR covariance does not rule out a nonlinear relationship.


## 5.15.c.i: Is the slope unbiased? ------------------------------------------

# Conditional on x: NOT IN GENERAL when c != 0.
# Conditional bias = c*sum((x - mean(x))*(x^2 - 1)) / sum((x - mean(x))^2).
# For illustration only, consider the fixed x values 0, 1 and 2, with c = 1.
x_example_15 <- c(0, 1, 2)
c_example_15 <- 1
conditional_bias_15 <- c_example_15 *
  sum((x_example_15 - mean(x_example_15)) * (x_example_15^2 - 1)) /
  sum((x_example_15 - mean(x_example_15))^2)

conditional_bias_15  # 2: a nonzero conditional bias for this fixed sample.

# Unconditionally: YES in this special symmetric example, if the expectation
# exists. A normal x sample has the same distribution as its negative.
# Changing every x to -x reverses the conditional bias but leaves its
# denominator unchanged. These opposite biases cancel when averaging over x.
# Thus E(b2) = beta_2, although E(b2 | x) need not equal beta_2.
# Zero covariance alone does NOT guarantee unbiasedness in general.


## 5.15.c.ii: Is the slope consistent? ---------------------------------------

# YES, under the usual sampling and moment conditions.
# b2 - beta_2 = [sum((x - mean(x))*e)/N] / [sum((x - mean(x))^2)/N].
# The numerator converges to cov(x,e) = 0.
# The denominator converges to var(x) = 1.
# Therefore b2 converges to beta_2.

# However, E(y | x) = beta_1 + beta_2*x + c*(x^2 - 1) is nonlinear.
# Consistency of this slope does not make the conditional mean a straight line.


# Exercise 5.16: Tuna prices and substitution --------------------------------
# CANS is weekly sales of brand A in THOUSANDS of cans.
# RPRCE_B = 100*PRICE_B/PRICE_A, and similarly for C.
# Use the supplied regression: log(CANS) = beta_1 + beta_2*RPRCE_B
#                                        + beta_3*RPRCE_C + e.

b1_16 <- -2.724
b2_16 <- 0.0146
b3_16 <- 0.02649
se_b2_16 <- 0.00548
se_b3_16 <- 0.00544
cov_b2_b3_16 <- -0.0000143
sigma_hat_16 <- 0.5663
df_16 <- 52 - 3
critical_16 <- qt(0.95, df = df_16)  # Two-sided 10% test.


## 5.16.a: Interpret the coefficients ----------------------------------------

# Holding the other relative price fixed, a ONE-POINT increase in RPRCE_B
# raises expected log sales by beta_2. The approximate percentage effect is:
100 * b2_16  # 1.46%.
100 * b3_16  # 2.649% for a one-point increase in RPRCE_C.

# Exact percentage changes on the exponentiated fitted-log scale:
100 * (exp(b2_16) - 1)
100 * (exp(b3_16) - 1)

# Both signs should be positive if B and C are substitutes for A.
# A point in a relative-price index is not the same as a 1% price increase.
# These percentage interpretations also apply to mean sales if the
# retransformation factor E(exp(e) | X) is constant across X.
# MR1-MR5 alone describe mean LOG sales, not that additional assumption.


## 5.16.b: Is the brand B effect different from 2.5%? --------------------------

# Use the usual small-change approximation: H0: beta_2 = 0.025.
# H1: beta_2 != 0.025. Significance level: 10%.
t_b_16 <- (b2_16 - 0.025) / se_b2_16
p_b_16 <- 2 * pt(-abs(t_b_16), df = df_16)

t_b_16  # About -1.89781.
p_b_16  # About 0.06362: reject at 10%.


## 5.16.c: Is the brand C effect different from 2.5%? --------------------------

t_c_16 <- (b3_16 - 0.025) / se_b3_16
p_c_16 <- 2 * pt(-abs(t_c_16), df = df_16)

t_c_16  # About 0.27390.
p_c_16  # About 0.78532: do not reject at 10%.

# For an exact finite 2.5% effect, replace 0.025 with log(1.025).
# The two decisions remain unchanged.
# With MR1-MR5 alone, these conventional t tests are approximate.
# Exact finite-sample t inference additionally requires normal errors.


## 5.16.d: Are the two effects equal? ----------------------------------------

# H0: beta_2 - beta_3 = 0. H1: beta_2 - beta_3 != 0.
difference_16 <- b2_16 - b3_16
se_difference_16 <- sqrt(se_b2_16^2 + se_b3_16^2 - 2 * cov_b2_b3_16)
t_difference_16 <- difference_16 / se_difference_16
p_difference_16 <- 2 * pt(-abs(t_difference_16), df = df_16)

se_difference_16  # About 0.009393.
t_difference_16   # About -1.26587.
p_difference_16   # About 0.21155: do not reject at 10%.

# There is no contradiction with (b)-(c). A significant result for one
# coefficient and a nonsignificant result for the other does not establish
# a significant difference between them. Test their difference directly.


## 5.16.e: Which competitor is the closer substitute? -------------------------

# The point estimates suggest C because b3 > b2.
# But (d) does not establish a difference at 10%, so the ranking is uncertain.


## 5.16.f: Corrected predictions of mean sales --------------------------------

# Use the three pairs of relative prices specified in the question.
sales_scenarios_16 <- tibble(
  scenario = c("i", "ii", "iii"),
  relative_price_B = c(125, 111.11, 100),
  relative_price_C = c(100, 88.89, 80)
)

# Under normal, homoskedastic log errors, add sigma_hat^2/2 BEFORE exp().
# exp(fitted_log) alone omits the correction for mean sales.
sales_scenarios_16 <- sales_scenarios_16 |>
  mutate(fitted_log = b1_16 + b2_16 * relative_price_B +
           b3_16 * relative_price_C,
         mean_sales_thousands = exp(fitted_log + sigma_hat_16^2 / 2))

sales_scenarios_16
# Mean sales are about 6.75544, 4.10929 and 2.76088 thousand cans.
# This particular correction needs more than MR1-MR5: it assumes normal
# log errors with constant variance, as in the requested corrected predictor.


## 5.16.g: Choose among three prices for A ------------------------------------

# With B priced at $1 and C at $0.80, these prices for A correspond to (f).
# Keep the rounded ratios specified in (f) for the revenue comparison.
sales_scenarios_16 <- sales_scenarios_16 |>
  mutate(price_A = c(0.80, 0.90, 1.00),
         mean_sales_cans = 1000 * mean_sales_thousands,
         revenue_dollars = price_A * mean_sales_cans)

sales_scenarios_16 |>
  select(scenario, price_A, mean_sales_cans, revenue_dollars)

# Predicted revenues are about $5404.35, $3698.36 and $2760.88.
# Choose $0.80 among the three stated prices to maximize predicted revenue.
# This is not necessarily the best price among every possible price.
# Profit maximization would also require production costs.


# Exercise 5.17: Check Exercise 5.1 using lm() --------------------------------
# Use ex_data, entered at the start of this script from the seven textbook rows.
# This exercise asks for software checks of 5.1(c), (e), (f), (g) and (h).


## 5.17: Check 5.1.c, the three coefficients ----------------------------------

model_17 <- lm(y ~ x_2 + x_3, data = ex_data)
coef(model_17)
summary(model_17)

# R includes the intercept automatically: do not also add the constant x_1.
# Coefficients: intercept = 1, x_2 = 1, x_3 = 0.5625.


## 5.17: Check 5.1.e, the estimated error variance -----------------------------

# summary() reports the residual STANDARD DEVIATION as sigma. Square it.
sigma_squared_17 <- summary(model_17)$sigma^2
sigma_squared_17  # 0.234375.


## 5.17: Check 5.1.f, the regressor correlation -------------------------------

cor(ex_data$x_2, ex_data$x_3)  # 0.


## 5.17: Check 5.1.g, the standard error of b2 ---------------------------------

# Select row x_2 and column Std. Error from the coefficient table.
se_b2_17 <- coef(summary(model_17))["x_2", "Std. Error"]
se_b2_17  # About 0.129387.


## 5.17: Check 5.1.h, sums of squares and R-squared ----------------------------

SSE_17 <- sum(residuals(model_17)^2)
SST_17 <- sum((ex_data$y - mean(ex_data$y))^2)
SSR_17 <- SST_17 - SSE_17
R_squared_17 <- summary(model_17)$r.squared

SSE_17        # 0.9375.
SST_17        # 20.
SSR_17        # 19.0625.
R_squared_17  # 0.953125.

# Compare the software results with the hand-formula results from 5.1.
tibble(
  quantity = c("SSE", "SST", "SSR", "R-squared", "Error variance", "SE of b2"),
  hand_calculation = c(SSE, SST, SSR, R_squared, sigma_squared_hat, se_b2),
  software = c(SSE_17, SST_17, SSR_17, R_squared_17, sigma_squared_17, se_b2_17)
)


# Exercise 5.18: Food and clothing budget shares ------------------------------

# Part 0: Download the textbook data for Exercises 5.18 and 5.19.
load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/london5.rdata"))
head(london5)
count(london5, nk)

# load() creates london5. Variable names are lowercase.
# Shares are fractions: 0.30 means 30% of the budget, not 0.30%.
# There are 336 one-child and 514 two-child households.


## 5.18.a: Expected signs ----------------------------------------------------

# Food is expected to be a necessity: its share falls as total expenditure rises.
# Therefore we expect a negative coefficient on log(totexp).
# Clothing might be a luxury, with a positive log-expenditure coefficient.


## 5.18.b: Estimate the models for one-child households -----------------------

one_child_18 <- london5 |>
  filter(nk == 1)

food1_18 <- lm(wfood ~ log(totexp), data = one_child_18)
cloth1_18 <- lm(wcloth ~ log(totexp), data = one_child_18)

summary(food1_18)
summary(cloth1_18)

# The food slope is about -0.16359; the clothing slope is about 0.06493.
# Both differ from zero at 1% in two-sided tests.
# Food behaves as a necessity; clothing behaves as a luxury here.
# Clothing's positive slope could reflect more discretionary/quality purchases,
# but this regression does not establish that explanation.
# A 1% expenditure increase changes food share by about -0.16359 percentage
# points and clothing share by +0.06493 percentage points.
# A falling food SHARE does not necessarily mean falling food spending.


## 5.18.c: Test whether food is a necessity -----------------------------------

# H0: beta_2 >= 0. H1: beta_2 < 0. Lower-tail test at 1%.
t_food1_18 <- coef(summary(food1_18))["log(totexp)", "t value"]
p_food1_18 <- pt(t_food1_18, df = df.residual(food1_18))

t_food1_18  # About -13.6701.
p_food1_18  # About 2.14e-34: reject at 1%.

# The necessity claim is in the alternative: require evidence of a negative slope.


## 5.18.d: The same lower-tail test for clothing ------------------------------

# H0: alpha_2 >= 0. H1: alpha_2 < 0.
t_cloth1_18 <- coef(summary(cloth1_18))["log(totexp)", "t value"]
p_cloth1_18 <- pt(t_cloth1_18, df = df.residual(cloth1_18))

t_cloth1_18  # About +5.4817.
p_cloth1_18  # Almost 1: do not reject at 1%.

# The estimate is positive, opposite to this negative-slope alternative.
# Do not just halve a two-sided p-value without checking the direction.


## 5.18.e: Two-child models and confidence intervals --------------------------

two_children_18 <- london5 |>
  filter(nk == 2)

food2_18 <- lm(wfood ~ log(totexp), data = two_children_18)
cloth2_18 <- lm(wcloth ~ log(totexp), data = two_children_18)

coef(food2_18)   # Food slope about -0.12913.
coef(cloth2_18)  # Clothing slope about 0.08098.

confint(food1_18, "log(totexp)")
confint(food2_18, "log(totexp)")
confint(cloth1_18, "log(totexp)")
confint(cloth2_18, "log(totexp)")

# The food slope looks less negative for two-child households.
# The clothing difference looks smaller relative to its uncertainty.
# Overlapping individual intervals do not test equality of two slopes.


## 5.18.f: Test slope differences with interaction models ---------------------

# * includes log(totexp), nk, and log(totexp):nk.
food_all_18 <- lm(wfood ~ log(totexp) * nk, data = london5)
cloth_all_18 <- lm(wcloth ~ log(totexp) * nk, data = london5)

summary(food_all_18)
summary(cloth_all_18)

# Since nk only takes values 1 and 2, the interaction coefficient is the
# difference in slopes between the two groups. Test it against zero.
p_food_interaction_18 <- coef(summary(food_all_18))[
  "log(totexp):nk", "Pr(>|t|)"]
p_cloth_interaction_18 <- coef(summary(cloth_all_18))[
  "log(totexp):nk", "Pr(>|t|)"]

p_food_interaction_18   # About 0.03515: reject equal food slopes at 5%.
p_cloth_interaction_18  # About 0.32351: do not reject equal clothing slopes.

# Read the book's "95% significance" as 95% confidence, or 5% significance.
# These are conventional homoskedastic regression tests.


## 5.18.g: Recover the group slopes from the interaction model ----------------

food_coefficients_18 <- coef(food_all_18)
cloth_coefficients_18 <- coef(cloth_all_18)

food_slopes_18 <- food_coefficients_18["log(totexp)"] +
  food_coefficients_18["log(totexp):nk"] * c(1, 2)
cloth_slopes_18 <- cloth_coefficients_18["log(totexp)"] +
  cloth_coefficients_18["log(totexp):nk"] * c(1, 2)

tibble(
  children = c(1, 2),
  separate_food_slope = c(coef(food1_18)[2], coef(food2_18)[2]),
  interaction_food_slope = food_slopes_18,
  separate_clothing_slope = c(coef(cloth1_18)[2], coef(cloth2_18)[2]),
  interaction_clothing_slope = cloth_slopes_18
)

# Point estimates match: the interaction model allows two separate lines.
# Standard errors need not match, because the combined model pools error variance.


# Exercise 5.19: Food shares and expenditure elasticity -----------------------
# Continue with london5 downloaded directly above.
# WFOOD = beta_1 + beta_2*log(TOTEXP) + e.


## 5.19.a: Why does a negative slope indicate a necessity? ---------------------

# Let w = beta_1 + beta_2*log(TOTEXP) be the positive mean food share.
# Food expenditure elasticity is epsilon = 1 + beta_2/w.
# With w > 0, epsilon < 1 exactly when beta_2 < 0.
# Food is a necessity if food spending increases less than proportionately
# with total expenditure. The argument requires a positive food share.


## 5.19.b: Estimate using all households --------------------------------------

food_19 <- lm(wfood ~ log(totexp), data = london5)
summary(food_19)

b1_19 <- coef(food_19)[1]
b2_19 <- coef(food_19)[2]

# Intercept about 0.993839; slope about -0.140509.
# The negative slope is strongly significant. R-squared is about 0.25437.


## 5.19.c: Mean shares and 95% confidence intervals ----------------------------

budgets_19 <- tibble(totexp = c(50, 90, 170))
predict(food_19, newdata = budgets_19, interval = "confidence")

# Fitted mean shares: about 0.44417, 0.36158 and 0.27221.
# Multiply by 100 to express them as percentages of the household budget.
# These intervals concern mean shares, not individual household outcomes.


## 5.19.d: Elasticities and approximate confidence intervals -------------------

expenditure_19 <- budgets_19$totexp
mean_share_19 <- b1_19 + b2_19 * log(expenditure_19)
elasticity_19 <- 1 + b2_19 / mean_share_19

# The elasticity is nonlinear. Use the delta method, as in 5.4 and 5.11.
# Derivatives of 1 + beta_2/(beta_1 + beta_2*log(TOTEXP)):
g1_19 <- -b2_19 / mean_share_19^2
g2_19 <- b1_19 / mean_share_19^2

# vcov() returns the estimated coefficient covariance matrix.
V_19 <- vcov(food_19)
var_elasticity_19 <- g1_19^2 * V_19[1, 1] + g2_19^2 * V_19[2, 2] +
  2 * g1_19 * g2_19 * V_19[1, 2]
se_elasticity_19 <- sqrt(var_elasticity_19)

# Use the normal critical value for these large-sample approximate intervals.
critical_19 <- qnorm(0.975)
elasticity_results_19 <- tibble(
  expenditure = expenditure_19,
  mean_share = mean_share_19,
  elasticity = elasticity_19,
  standard_error = se_elasticity_19,
  lower = elasticity_19 - critical_19 * se_elasticity_19,
  upper = elasticity_19 + critical_19 * se_elasticity_19
)

elasticity_results_19

# Elasticities: about 0.68366, 0.61140 and 0.48383.
# At expenditure 90, 1% more total expenditure corresponds to about 0.6114%
# more FOOD SPENDING. This is not the percentage change in the food share.


## 5.19.e: Interpretation and precision ---------------------------------------

# Both mean food share and its expenditure elasticity decline with expenditure.
# All three elasticity intervals lie between zero and one: food spending rises
# less than proportionately, so its budget share falls.
# The mean-share interval is narrowest at 90, wider at the two outer values.
# The elasticity SE increases from about 0.0154 to 0.0411 across these budgets.
# Dividing by a smaller share makes the elasticity more sensitive to uncertainty.


# Exercise 5.20: A slope based on two group means ------------------------------
# Sort observations by x, then split into equal halves.
# Alternative slope = (mean(y in upper half) - mean(y in lower half)) /
#                     (mean(x in upper half) - mean(x in lower half)).
# This exercise generates artificial regressors; it has no dataset to download.


## 5.20.a: Unbiasedness -------------------------------------------------------

# Conditional on x, each group's mean error has expectation zero.
# The expected difference in mean y is beta_2 times the difference in mean x.
# Dividing by the fixed x gap gives E(alternative slope | x) = beta_2.
# The estimator is also marginally unbiased when the expectation exists.


## 5.20.b: Conditional variance -----------------------------------------------

# Each half has N/2 independent errors, so each mean error has variance
# sigma^2/(N/2) = 2*sigma^2/N. Their difference has variance 4*sigma^2/N.
# Let gap = mean(x in upper half) - mean(x in lower half).
# Conditional slope variance = 4*sigma^2/(N*gap^2).


## 5.20.c: Unconditional variance ---------------------------------------------

# Average the conditional variance over the random x samples:
# var(alternative slope) = (4*sigma^2/N)*E(1/gap^2), when finite.
# There is no extra variance-of-the-conditional-mean term because that mean
# is always beta_2.


## 5.20.d: Consistency --------------------------------------------------------

# A sufficient condition is that N*gap^2 grows without bound in probability.
# For example, a gap converging to a nonzero constant makes the conditional
# variance tend to zero. Under the assumptions, this gives consistency.


## 5.20.e: Compare variances for four sample sizes -----------------------------

# runif() generates uniform random numbers; sort() puts them in increasing order.
# set.seed() makes the generated examples reproducible.
# A for loop repeats the SAME calculation for each sample size.
set.seed(520)
sample_sizes_20 <- c(100, 500, 1000, 5000)
sigma_squared_20 <- 1000
single_samples_20 <- tibble()

for (N_20 in sample_sizes_20) {
  x_20 <- sort(runif(N_20, min = 0, max = 10))
  lower_half_20 <- x_20[1:(N_20 / 2)]
  upper_half_20 <- x_20[(N_20 / 2 + 1):N_20]
  gap_20 <- mean(upper_half_20) - mean(lower_half_20)
  sx_squared_20 <- mean((x_20 - mean(x_20))^2)

  single_samples_20 <- bind_rows(single_samples_20, tibble(
    N = N_20,
    OLS_variance = sigma_squared_20 / (N_20 * sx_squared_20),
    split_variance = 4 * sigma_squared_20 / (N_20 * gap_20^2),
    inverse_variance = 1 / sx_squared_20,
    inverse_gap = 4 / gap_20^2
  ))
}

single_samples_20

# mean((x - mean(x))^2) uses divisor N, as the question requests.
# var(x) would instead use divisor N - 1.
# The last two columns are single realizations, not estimates of expectations
# based on repeated samples. To estimate expectations, repeat and average:

set.seed(5201)
repetitions_20 <- 1000
average_results_20 <- tibble()

for (N_20 in sample_sizes_20) {
  inverse_variances_20 <- numeric(repetitions_20)
  inverse_gaps_20 <- numeric(repetitions_20)

  for (r_20 in 1:repetitions_20) {
    x_20 <- sort(runif(N_20, min = 0, max = 10))
    lower_half_20 <- x_20[1:(N_20 / 2)]
    upper_half_20 <- x_20[(N_20 / 2 + 1):N_20]
    gap_20 <- mean(upper_half_20) - mean(lower_half_20)
    sx_squared_20 <- mean((x_20 - mean(x_20))^2)
    inverse_variances_20[r_20] <- 1 / sx_squared_20
    inverse_gaps_20[r_20] <- 4 / gap_20^2
  }

  average_results_20 <- bind_rows(average_results_20, tibble(
    N = N_20,
    mean_inverse_variance = mean(inverse_variances_20),
    mean_inverse_gap = mean(inverse_gaps_20),
    mean_OLS_variance = sigma_squared_20 / N_20 * mean(inverse_variances_20),
    mean_split_variance = sigma_squared_20 / N_20 * mean(inverse_gaps_20)
  ))
}

average_results_20

# numeric() creates space to store the repetitions.
# [r_20] stores one result at position r_20. mean() averages all repetitions.
# These averages estimate the requested expectations; simulation is approximate.


## 5.20.f: Efficiency and sample size -----------------------------------------

# Both variances decrease with N, but the sorted split estimator has higher
# variance. OLS uses all x variation instead of reducing it to two group means.
# Large-sample benchmarks for sigma^2 = 1000:
tibble(N = sample_sizes_20,
       approximate_OLS_variance = 120 / sample_sizes_20,
       approximate_split_variance = 160 / sample_sizes_20)

# The split estimator's asymptotic variance is about one-third larger.
# The falling variances support consistency in this sorted uniform example.


## 5.20.g: Explain the limits 0.12 and 0.16 ------------------------------------

uniform_variance_20 <- (10 - 0)^2 / 12
limiting_gap_20 <- 7.5 - 2.5

1 / uniform_variance_20   # 0.12.
4 / limiting_gap_20^2     # 0.16.

# The lower and upper sorted halves have limiting means 2.5 and 7.5.
# The sample inverse quantities approach these constants in probability.
# Their expectations approach the same limits under the required inverse-moment
# conditions. Expectations themselves are deterministic, not random samples.


## 5.20.h: What if we do not sort? --------------------------------------------

# Repeat the single-sample comparison, omitting sort().
set.seed(5208)
unsorted_results_20 <- tibble()

for (N_20 in sample_sizes_20) {
  x_20 <- runif(N_20, min = 0, max = 10)
  lower_half_20 <- x_20[1:(N_20 / 2)]
  upper_half_20 <- x_20[(N_20 / 2 + 1):N_20]
  gap_20 <- mean(upper_half_20) - mean(lower_half_20)

  unsorted_results_20 <- bind_rows(unsorted_results_20, tibble(
    N = N_20,
    gap = gap_20,
    split_variance = 4 * sigma_squared_20 / (N_20 * gap_20^2)
  ))
}

unsorted_results_20

# Both random halves have means approaching 5, so their gap approaches zero.
# Typically the gap shrinks at rate 1/sqrt(N), as does the mean-error difference.
# Their ratio does not concentrate at the true slope: NOT consistent.
# Near-zero random gaps make the estimator unstable; a few simulated values
# do not establish a finite unconditional variance, which can be infinite here.


# Exercise 5.21: Rainfall, technology and wheat yield --------------------------

# Part 0: Download data and load the package used for coefficient combinations.
# Install car once if needed: install.packages("car")
library(car)
load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/toody5.rdata"))
head(toody5)

# Yield y: tonnes per hectare. Rainfall rain: decimeters.
# Use trend, which starts at 0 in 1950 and increases by 0.1 per year.
# Thus 1959 has trend 0.9 and 1995 has trend 4.5.


## 5.21.a: Estimate the model -------------------------------------------------

model_21 <- lm(y ~ trend + rain + I(rain^2) + trend:rain, data = toody5)
coef(summary(model_21))

# I(rain^2) adds squared rainfall; trend:rain adds the interaction.
# There are 48 observations and 43 residual degrees of freedom.


## 5.21.b: Which coefficients differ from zero? -------------------------------

p_values_21 <- coef(summary(model_21))[, "Pr(>|t|)"]
tibble(coefficient = names(p_values_21),
       p_value = as.numeric(p_values_21),
       significant_5_percent = p_values_21 < 0.05,
       significant_10_percent = p_values_21 < 0.10)

# At 5%, all except the interaction differ from zero.
# At 10%, all five do. The interaction p-value is about 0.05350.


## 5.21.c: Interpret the signs -----------------------------------------------

# Positive rain and negative rain^2 allow benefits from rain followed by
# diminishing benefits and eventually harm from excessive rainfall.
# The negative interaction means the rainfall slope falls as trend increases.
# This is consistent with developing drought-resistant wheat varieties.
# The technology effect is beta_2 + beta_5*RAIN, not beta_2 alone.
# The negative intercept refers to zero rainfall at trend zero, outside the
# observed rainfall range; it need not have a useful physical meaning.


## 5.21.d: Marginal rainfall effects in 1959 and 1995 --------------------------

# Rainfall slope = beta_3 + 2*beta_4*RAIN + beta_5*TREND.
# deltaMethod() uses the coefficient covariance matrix to calculate the SE.
# Give coefficients simple names in their MODEL ORDER:
# b1 = intercept; b2 = trend; b3 = rain; b4 = rain^2; b5 = interaction.
# No need to write all the covariance terms by hand each time.
rain1959_21 <- deltaMethod(model_21, "b3 + 2*b4*2.98 + b5*0.9",
  parameterNames = c("b1", "b2", "b3", "b4", "b5"))
rain1995_21 <- deltaMethod(model_21, "b3 + 2*b4*4.797 + b5*4.5",
  parameterNames = c("b1", "b2", "b3", "b4", "b5"))

# Read Estimate and SE, then form intervals with the chapter's t critical value.
critical_21 <- qt(0.975, df = df.residual(model_21))
rain_effects_21 <- tibble(
  year = c(1959, 1995),
  effect = c(rain1959_21[1, "Estimate"], rain1995_21[1, "Estimate"]),
  standard_error = c(rain1959_21[1, "SE"], rain1995_21[1, "SE"])
)

rain_effects_21 <- rain_effects_21 |>
  mutate(lower = effect - critical_21 * standard_error,
         upper = effect + critical_21 * standard_error)

rain_effects_21

# 1959: effect about 0.48843, interval [0.19485, 0.78202].
# 1995: effect about -0.41661, interval [-0.72843, -0.10480].
# Units: tonnes per hectare per additional decimeter of rain.
# Both rainfall AND technology differ between the two cases.
# Comparing them does not isolate technology alone.


## 5.21.e: Rainfall that maximizes expected yield -----------------------------

# Set the rainfall slope to zero: RAIN* = -(beta_3 + beta_5*TREND)/(2*beta_4).
# The negative squared coefficient makes the fitted turning point a maximum.
optimum1959_21 <- deltaMethod(model_21, "-(b3 + b5*0.9)/(2*b4)",
  parameterNames = c("b1", "b2", "b3", "b4", "b5"))
optimum1995_21 <- deltaMethod(model_21, "-(b3 + b5*4.5)/(2*b4)",
  parameterNames = c("b1", "b2", "b3", "b4", "b5"))

optimum_results_21 <- tibble(
  year = c(1959, 1995),
  rainfall = c(optimum1959_21[1, "Estimate"], optimum1995_21[1, "Estimate"]),
  standard_error = c(optimum1959_21[1, "SE"], optimum1995_21[1, "SE"])
)

optimum_results_21 <- optimum_results_21 |>
  mutate(lower = rainfall - critical_21 * standard_error,
         upper = rainfall + critical_21 * standard_error)

optimum_results_21

# Estimated optima: about 4.53473 dm in 1959 and 3.47088 dm in 1995.
# Approximate intervals: [4.05904, 5.01042] and [2.67921, 4.26255].
# The fitted optimum is lower in 1995 and is estimated less precisely.
# Actual 1959 rain was below its optimum; 1995 rain was above its optimum.
# This explains the different marginal-effect signs in (d).
# These nonlinear ratio intervals are approximate, even with normal errors.
# Overlapping intervals are not a formal test of equality between the optima.


# Exercise 5.22: Rainfall change versus technological change ------------------
# Continue with toody5, downloaded above. Keep the same yield specification.


## 5.22.a: Estimate and report the yield model --------------------------------

model_22 <- lm(y ~ trend + rain + I(rain^2) + trend:rain, data = toody5)
coef(summary(model_22))


## 5.22.b: Does extra rain increase expected yield in 1974? --------------------

# H0: marginal rainfall effect <= 0. H1: effect > 0.
# For 1974, trend = 2.4 and rain = 4.576.
rain_effect_22 <- deltaMethod(model_22, "b3 + 2*b4*4.576 + b5*2.4",
  parameterNames = c("b1", "b2", "b3", "b4", "b5"))
estimate_rain_22 <- rain_effect_22[1, "Estimate"]
se_rain_22 <- rain_effect_22[1, "SE"]
t_rain_22 <- estimate_rain_22 / se_rain_22
p_rain_22 <- pt(t_rain_22, df = df.residual(model_22), lower.tail = FALSE)

estimate_rain_22  # About -0.15222.
t_rain_22         # About -2.17933.
p_rain_22         # About 0.98258: do not reject at 5%.

# Use the UPPER tail because the alternative is positive, even if t is negative.


## 5.22.c: Technological improvement at median rainfall -----------------------

# 1960 has trend = 1; 1995 has trend = 4.5. The change is 3.5 trend units.
# Hold rainfall at the stated median, 3.8355 dm.
technology_22 <- deltaMethod(model_22, "3.5*(b2 + 3.8355*b5)",
  parameterNames = c("b1", "b2", "b3", "b4", "b5"))
gain_22 <- technology_22[1, "Estimate"]
se_gain_22 <- technology_22[1, "SE"]
critical_22 <- qt(0.975, df = df.residual(model_22))
lower_gain_22 <- gain_22 - critical_22 * se_gain_22
upper_gain_22 <- gain_22 + critical_22 * se_gain_22

gain_22        # About 1.12707 tonnes per hectare.
lower_gain_22  # About 0.88983.
upper_gain_22  # About 1.36432.

# This is an interval for a conditional mean change, not two actual harvests.


## 5.22.d: Is mean rainfall declining? ----------------------------------------

rain_model_22 <- lm(rain ~ trend, data = toody5)
summary(rain_model_22)

# H0: rainfall trend >= 0. H1: rainfall trend < 0. Lower-tail test.
t_trend_22 <- coef(summary(rain_model_22))["trend", "t value"]
p_trend_22 <- pt(t_trend_22, df = df.residual(rain_model_22))

t_trend_22  # About -1.70897.
p_trend_22  # About 0.04710: reject no decline at 5%.

# One trend unit is TEN calendar years. Convert the trend slope to dm per year:
coef(rain_model_22)["trend"] * 0.1  # About -0.01522.


## 5.22.e: Predict mean rainfall in 1960 and 1995 ------------------------------

rain_means_22 <- predict(rain_model_22,
  newdata = tibble(trend = c(1, 4.5)))
rain_means_22  # About 4.24069 and 3.70790 dm.


## 5.22.f: Change rainfall, holding technology at its 1960 level ---------------

climate_cases_22 <- tibble(trend = c(1, 1), rain = as.numeric(rain_means_22))
climate_yields_22 <- predict(model_22, newdata = climate_cases_22)
climate_change_22 <- climate_yields_22[2] - climate_yields_22[1]

climate_change_22  # About -0.08886 tonnes per hectare.

# This rainfall-only comparison does not establish a causal climate-change effect.


## 5.22.g: Change technology, holding rainfall at its 1960 mean -----------------

technology_cases_22 <- tibble(trend = c(1, 4.5),
                             rain = as.numeric(rain_means_22[1]))
technology_yields_22 <- predict(model_22, newdata = technology_cases_22)
technology_change_22 <- technology_yields_22[2] - technology_yields_22[1]

technology_change_22  # About 0.99541 tonnes per hectare.


## 5.22.h: Compare the changes ------------------------------------------------

tibble(
  comparison = c("Technology at median rain", "Rain at 1960 technology",
                 "Technology at 1960 rain"),
  yield_change = c(gain_22, climate_change_22, technology_change_22)
)

# Technology gains are larger than the rainfall-only loss in these comparisons.
# The two technology gains use different fixed rainfall levels.
# With a negative interaction, the model gives a larger technology gain
# at the lower, median rainfall level.

# When BOTH rainfall and technology change, include their interaction change.
combined_cases_22 <- tibble(trend = c(1, 4.5), rain = as.numeric(rain_means_22))
combined_yields_22 <- predict(model_22, newdata = combined_cases_22)
combined_change_22 <- combined_yields_22[2] - combined_yields_22[1]

combined_change_22                       # About 1.07968.
climate_change_22 + technology_change_22  # About 0.90655.

# The additional 0.17312 is b5 * change in rain * change in trend.
# All cases use estimated MEAN rainfall. Interpreting changes as averages over
# random rainfall additionally requires unchanged rainfall-variance contributions.


# Exercise 5.23: Quantity, quality and unit price ------------------------------

# Part 0: Download the 56 historical observations.
load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/cocaine.rdata"))
head(cocaine)

# price: dollars per gram; quant: grams per sale; qual: percentage purity.
# trend = 1 in 1984, increasing by one per year to 8 in 1991.


## 5.23.a: Expected signs ----------------------------------------------------

# Quantity: negative if larger sales receive a lower price per gram.
# Quality: positive if greater purity commands a premium.
# Trend: uncertain; depends on how supply, demand and other conditions change.


## 5.23.b: Estimate and interpret the regression ------------------------------

model_23 <- lm(price ~ quant + qual + trend, data = cocaine)
coef(summary(model_23))

# Quantity slope about -0.05997: one additional gram per sale is associated
# with a $0.05997 lower price PER GRAM, holding quality and time fixed.
# Quality slope about 0.11621: one percentage point more purity is associated
# with a $0.11621 higher price per gram, holding quantity and time fixed.
# The signs match the proposed quantity discount and quality premium.


## 5.23.c: Explained price variation -----------------------------------------

R_squared_23 <- summary(model_23)$r.squared
R_squared_23            # About 0.50965.
100 * R_squared_23      # About 50.97% of sample variation in unit price.

# This concerns variation around sample mean price, not correct predictions.


## 5.23.d: Test for a quantity discount ---------------------------------------

# H0: beta_2 >= 0. H1: beta_2 < 0.
t_quantity_23 <- coef(summary(model_23))["quant", "t value"]
p_quantity_23 <- pt(t_quantity_23, df = df.residual(model_23))

t_quantity_23  # About -5.89194.
p_quantity_23  # About 1.43e-7: reject at 5% and 1%.

# Strong evidence of a discount pattern, but not proof that reducing the
# number of transactions to avoid detection is its causal explanation.


## 5.23.e: Test for a positive quality premium -------------------------------

# H0: beta_3 = 0. H1: beta_3 > 0.
t_quality_23 <- coef(summary(model_23))["qual", "t value"]
p_quality_23 <- pt(t_quality_23, df = df.residual(model_23), lower.tail = FALSE)

t_quality_23  # About 0.57169.
p_quality_23  # About 0.28500: do not reject at 5% or 10%.

# A positive point estimate alone does not establish a positive population effect.


## 5.23.f: Average annual price change ---------------------------------------

coef(model_23)["trend"]
confint(model_23, "trend")

# Holding quantity and quality fixed, price per gram falls by about $2.35458
# per year. The two-sided p-value is about 0.09536: significant at 10%, not 5%.
# Increased supply, distribution efficiency or weaker demand could explain it.
# The trend coefficient cannot distinguish those explanations.
