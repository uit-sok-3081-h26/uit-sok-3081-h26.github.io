# SOK-3081 Econometrics Principles
# Chapter 2: The Simple Linear Regression Model
# Textbook: Principles of Econometrics, 5e, Chapter 2
#
# Purpose of this script:
# Move from a visual relationship in the data to the OLS regression line,
# then use the fitted model for residual analysis, prediction and elasticity.


##### Start up -----------------------------------------------------------------------------

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

# Opening the data definition URL in the default web browser
#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/food.def")

# Loading the food dataset from the provided URL
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/food.rdata"))

# In the original dataset, income is measured in hundreds of dollars.
# Convert it to dollars once here. From this point onward, income = weekly dollars.
# This changes the numerical size of the slope coefficient, but not fitted values.
food <- food |>
  mutate(income = income * 100)

##### LAB 0: calculating variables from last time  -----------------------------------------
mean(food$income)
model_food <- lm(food_exp ~ income, data = food)
model_food

# Full regression output: coefficients, standard errors, t-statistics, R-squared, etc.
summary(model_food)

# Extract only the estimated intercept and slope.
# These should be the same as b_1 and b_2 calculated manually above.
coef(model_food)

# writing out the coefficients
b_1 <- coef(model_food)[1]
b_2 <- coef(model_food)[2]

# Displaying the names of the components of the fit object
names(model_food)

# Easier way to fine all the fittet values or predicted values if there had not been en "error"
model_food$fitted.values
fitted(model_food)

# write it to the datasett
food$fitted <- fitted(model_food)


# Easier way to fine the residuals or the difference between what we would expect and what we actually recoreded
model_food$residuals
resid(model_food)
# write it to the datasett
food$residuals <- resid(model_food)

# A simple plot of the OLS
plotModel(model_food) 

# making a predictio function
prediction_function <- makeFun(model_food)

##### LAB 1: Elasticity at the sample mean  -----------------------------------------
# In a linear level-level model, the slope b_2 is a constant marginal effect,
# but elasticity is NOT constant.
#
# Elasticity at a particular point is:
#     elasticity = b_2 * x / y_hat
#
# A common reporting choice is to evaluate elasticity at the sample means.
# Since the OLS line passes through (x_bar, y_bar), y_hat at x_bar equals y_bar.

# finding the mean
mean(food$income)
mean(food$food_exp)

b_2 * mean(food$income) /
  (mean(food$food_exp))


##### LAB 2: Elasticity across the observed income range  -----------------------------------------

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



##### LAB 3A: Sampling variability -----------------------------------------

# We now create a population model where the true parameters are known.
# This allows us to study what happens to the OLS estimate across repeated samples.

beta_1 <- 80
beta_2 <- 0.10
sigma  <- 50

# Step 1: Keep the observed income values fixed.
x <- food$income

# Number of observations.
N <- length(x)

# Make the simulation reproducible.
set.seed(120)

# Step 2: Generate one random error for each observation.
e <- rnorm(N, mean = 0, sd = sigma)

# Step 3: Generate y from the population model.
y <- beta_1 + beta_2 * x + e

# Put the simulated sample into a data frame.
sample_1 <- tibble(
  income = x,
  food_exp = y
)

# Step 4: Estimate the regression.
model_1 <- lm(food_exp ~ income, data = sample_1)
model_1

coef(lm(food_exp ~ income, data = sample_1))[2]

# step 5: Repeat?



##### LAB 3B: Repeated sampling --------------------------------------------------

# Number of simulated samples.
B <- 1000

# Create an empty vector where we will store the estimated slopes.
b_2_sim <- numeric(B)

# Repeat the sampling experiment B times.
for (s in 1:B) {
  
  # Generate new random errors.
  e <- rnorm(N, mean = 0, sd = sigma)
  
  # Generate a new y-variable from the same population model.
  y <- beta_1 + beta_2 * x + e
  
  # Estimate the regression and save the slope.
  b_2_sim[s] <- coef(lm(y ~ x))[2]
}


# Make it into a table 
simulation_results <- tibble(
  simulation = 1:B,
  b_2 = b_2_sim
)

head(simulation_results)

mean(simulation_results$b_2)
sd(simulation_results$b_2)

min(simulation_results$b_2)
max(simulation_results$b_2)



##### LAB 3C:  -----------------------------------------

simulation_results |>
  ggplot(aes(x = b_2)) +
  geom_histogram(
    bins = 30,
    color = "white"
  ) +
  geom_vline(
    xintercept = beta_2,
    linetype = "dashed",
    color = "red",
    linewidth = 1
  ) +
  labs(
    title = "Sampling distribution of the OLS slope estimator",
    subtitle = "1,000 samples from the same population model",
    x = expression(b[2]),
    y = "Number of simulations"
  ) +
  theme_minimal(base_size = 13)


##### LAB 3D:  -----------------------------------------

# Number of samples we want to visualize.
B_plot <- 20

# Create empty vectors for the estimated intercepts and slopes.
b_1_plot <- numeric(B_plot)
b_2_plot <- numeric(B_plot)

# Repeat the sampling experiment.
for (s in 1:B_plot) {
  
  # Draw new random errors.
  e <- rnorm(N, mean = 0, sd = sigma)
  
  # Generate a new y-variable from the true population model.
  y <- beta_1 + beta_2 * x + e
  
  # Estimate the regression.
  model_sim <- lm(y ~ x)
  
  # Store the estimated intercept and slope.
  b_1_plot[s] <- coef(model_sim)[1]
  b_2_plot[s] <- coef(model_sim)[2]
}

# Store the estimated coefficients in a data frame.
simulated_lines <- tibble(
  simulation = 1:B_plot,
  b_1 = b_1_plot,
  b_2 = b_2_plot
)

# Create values of x covering the observed income range.
x_plot <- seq(
  min(x),
  max(x),
  length.out = 100
)

# Create one regression line for each simulated sample.
plot_lines <- simulated_lines |>
  tidyr::crossing(income = x_plot) |>
  mutate(
    y_hat = b_1 + b_2 * income
  )

# Plot all simulated regression lines.
plot_lines |>
  ggplot(aes(
    x = income,
    y = y_hat,
    group = simulation
  )) +
  geom_line(alpha = 0.35) +
  
  # Add the true population regression line.
  geom_abline(
    intercept = beta_1,
    slope = beta_2,
    colour = "red",
    linewidth = 1.2
  ) +
  labs(
    title = "Repeated samples produce different regression lines",
    subtitle = "20 estimated regressions and the true population relationship",
    x = "Weekly household income",
    y = "Food expenditure"
  ) +
  theme_minimal(base_size = 13)




##### LAB 4: What determines the precision of b_2? -------------------------------

# True population parameters.
beta_1 <- 80
beta_2 <- 0.10

# Keep the observed income values fixed.
x <- food$income
N <- length(x)

# Number of Monte Carlo simulations.
B <- 1000

##### LAB 4A: More noise ----------------------------------------------------------

# Compare three different levels of error variation.
sigma_values <- c(25, 50, 100)

# Empty data frame for storing results.
simulation_sigma <- tibble()

for (sigma_sim in sigma_values) {
  
  b_2_sim <- numeric(B)
  
  for (s in 1:B) {
    
    # Draw new random errors.
    e <- rnorm(N, mean = 0, sd = sigma_sim)
    
    # Generate a new dependent variable.
    y <- beta_1 + beta_2 * x + e
    
    # Estimate and store the slope.
    b_2_sim[s] <- coef(lm(y ~ x))[2]
  }
  
  # Add results for this value of sigma.
  simulation_sigma <- bind_rows(
    simulation_sigma,
    tibble(
      sigma = sigma_sim,
      b_2 = b_2_sim
    )
  )
}

simulation_sigma |>
  group_by(sigma) |>
  summarise(
    mean_b2 = mean(b_2),
    sd_b2 = sd(b_2)
  )

# Plot the sampling distributions together.
simulation_sigma |>
  ggplot(aes(
    x = b_2,
    fill = factor(sigma),
    color = factor(sigma)
  )) +
  geom_density(
    alpha = 0.20,
    linewidth = 1
  ) +
  geom_vline(
    xintercept = beta_2,
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = "More noise gives less precise slope estimates",
    subtitle = "Sampling distributions of b₂ for different values of σ",
    x = expression(b[2]),
    y = "Density",
    fill = expression(sigma),
    color = expression(sigma)
  ) +
  theme_minimal(base_size = 13)

##### LAB 4B: More variation in x -------------------------------------------------

# Start from the observed income values.
x_medium <- food$income

# Create a narrow version around the same mean.
x_narrow <- mean(x_medium) +
  0.5 * (x_medium - mean(x_medium))

# Create a wider version around the same mean.
x_wide <- mean(x_medium) +
  2 * (x_medium - mean(x_medium))

# Store them in a list.
x_values <- list(
  Narrow = x_narrow,
  Observed = x_medium,
  Wide = x_wide
)

simulation_x <- tibble()

for (x_name in names(x_values)) {
  
  x_sim <- x_values[[x_name]]
  
  b_2_sim <- numeric(B)
  
  for (s in 1:B) {
    
    # Keep sigma fixed.
    e <- rnorm(N, mean = 0, sd = 50)
    
    # Generate y using the same true slope.
    y <- beta_1 + beta_2 * x_sim + e
    
    # Estimate and store the slope.
    b_2_sim[s] <- coef(lm(y ~ x_sim))[2]
  }
  
  simulation_x <- bind_rows(
    simulation_x,
    tibble(
      x_variation = x_name,
      b_2 = b_2_sim
    )
  )
}

simulation_x |>
  group_by(x_variation) |>
  summarise(
    mean_b2 = mean(b_2),
    sd_b2 = sd(b_2)
  )

# Set the order of the groups.
simulation_x <- simulation_x |>
  mutate(
    x_variation = factor(
      x_variation,
      levels = c("Narrow", "Observed", "Wide")
    )
  )

# Plot the sampling distributions together.
simulation_x |>
  ggplot(aes(
    x = b_2,
    fill = x_variation,
    color = x_variation
  )) +
  geom_density(
    alpha = 0.20,
    linewidth = 1
  ) +
  geom_vline(
    xintercept = beta_2,
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = "More variation in x gives more precise slope estimates",
    subtitle = "Sampling distributions of b₂ for different spreads in x",
    x = expression(b[2]),
    y = "Density",
    fill = "Variation in x",
    color = "Variation in x"
  ) +
  theme_minimal(base_size = 13)


##### LAB 5: Theoretical versus simulated SD of b_2 -----------------------------

# We compare theory with the simulation that uses:
# - the observed x-values
# - sigma = 50
B <- 10000

sigma <- 50
x <- food$income


# Theoretical standard deviation of b_2
sd_b2_theoretical <- sigma /
  sqrt(sum((x - mean(x))^2))

sd_b2_theoretical


# Simulated standard deviation of b_2
# Select only the simulations using the observed x-values
sd_b2_simulated <- simulation_x |>
  filter(x_variation == "Observed") |>
  summarise(
    sd_b2 = sd(b_2)
  ) |>
  pull(sd_b2)

sd_b2_simulated 

##### LAB 6: Quadratic relationship ---------------------------------------------
rm(list = ls())
# Load the Baton Rouge house price dataset.
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/br.rdata"))

# Inspect the data.
head(br)

br |>
  ggplot(aes(x = sqft, y = price)) +
  geom_point(alpha = 0.25) +
  labs(
    title = "House price versus house size",
    x = "House size (square feet)",
    y = "House price"
  ) +
  theme_minimal(base_size = 13)

# Create squared house size.
br <- br |>
  mutate(
    sqft2 = sqft^2
  )

br |>
  ggplot(aes(x = sqft2, y = price)) +
  geom_point(alpha = 0.25) +
  labs(
    title = "House price versus house size",
    x = "House size (square feet)",
    y = "House price"
  ) +
  theme_minimal(base_size = 13)

# Estimate the quadratic model:
# price = alpha_1 + alpha_2 * sqft^2 + e
model_quadratic <- lm(price ~ sqft2, data = br)

summary(model_quadratic)


# Extract the coefficients.
alpha_1 <- coef(model_quadratic)[1]
alpha_2 <- coef(model_quadratic)[2]


# The marginal effect is:
#
# d(price)/d(sqft) = 2 * alpha_2 * sqft
#
# This means that the slope changes with house size.

# Marginal effect at selected house sizes.
2 * alpha_2 * 2000
2 * alpha_2 * 4000
2 * alpha_2 * 6000


# Add fitted values to the dataset.
br <- br |>
  mutate(
    fitted_quadratic = predict(model_quadratic)
  )


# Plot the data and the fitted quadratic relationship.
br |>
  arrange(sqft) |>
  ggplot(aes(x = sqft, y = price)) +
  geom_point(alpha = 0.25) +
  geom_line(
    aes(y = fitted_quadratic),
    linewidth = 1
  ) +
  labs(
    title = "Quadratic relationship between house size and price",
    subtitle = "The marginal effect changes with house size",
    x = "House size (square feet)",
    y = "House price"
  ) +
  theme_minimal(base_size = 13)


##### LAB 7: Log-linear relationship --------------------------------------------

# Estimate the log-linear model:
#
# log(price) = beta_1 + beta_2 * sqft + e

model_loglinear <- lm(log(price) ~ sqft, data = br)

summary(model_loglinear)


# Extract the slope coefficient.
beta_2_log <- coef(model_loglinear)[2]


# Approximate percentage effect of a one-unit increase in sqft.
100 * beta_2_log

# Add fitted values.
br <- br |>
  mutate(
    fitted_log_price = predict(model_loglinear),
    
    # Transform predictions back from log(price) to price.
    fitted_price_loglinear = exp(fitted_log_price)
  )


# Plot the fitted relationship in the original price scale.
br |>
  arrange(sqft) |>
  ggplot(aes(x = sqft, y = price)) +
  geom_point(alpha = 0.25) +
  geom_line(
    aes(y = fitted_price_loglinear),
    linewidth = 1
  ) +
  labs(
    title = "Log-linear relationship between house size and price",
    subtitle = "The fitted relationship is curved in the original price scale",
    x = "House size (square feet)",
    y = "House price"
  ) +
  theme_minimal(base_size = 13)

##### Compare the functional forms -----------------------------------------------

# Linear model for comparison.
model_linear <- lm(price ~ sqft, data = br)

# Compare SSE for models with the same dependent variable.
deviance(model_linear)
deviance(model_quadratic)

