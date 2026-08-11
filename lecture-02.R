# Lecture 02: Probability for Econometrics
# What Does a Regression Predict?
#
# Standalone live-lab script. No external packages or data files are required.
# The numbered sections match the lecture-02.qmd walkthrough.

# -----------------------------------------------------------------------------
# SETUP: Construct the 40-worker teaching dataset
# -----------------------------------------------------------------------------

educ <- rep(c(12, 14, 16, 18), each = 10)
deviation <- rep(c(-8, -5, -3, -2, -1, 1, 2, 3, 5, 8), times = 4)
conditional_mean <- -12 + 2.5 * educ

workers <- data.frame(
  worker = seq_along(educ),
  educ = educ,
  wage = conditional_mean + deviation
)

# Inspect the first observations so students can see what has been constructed.
head(workers, 12)

# =============================================================================
# LAB STEP 1 — What does the unconditional wage distribution tell us?
# =============================================================================

# PREDICTION STOP:
# Before running the next lines, ask:
# "If we know nothing else about a randomly selected worker, what would you
#  predict for wage?"

mean(workers$wage)     # Verified: 25.5
range(workers$wage)    # Verified: 10 to 41

hist(
  workers$wage,
  breaks = 8,
  main = "Unconditional wage distribution",
  xlab = "Hourly wage"
)

# WHY: The unconditional mean is our natural mean prediction before using any
# worker characteristics. It is not an exact individual forecast.

# =============================================================================
# LAB STEP 2 — Does conditioning on education change what we expect?
# =============================================================================

# PREDICTION STOP:
# Ask whether E(WAGE | EDUC) should be constant or should change with education.

group_means <- aggregate(wage ~ educ, data = workers, FUN = mean)
group_means

# Verified conditional means:
# EDUC 12 -> 18
# EDUC 14 -> 23
# EDUC 16 -> 28
# EDUC 18 -> 33

# WHY: E(WAGE) and E(WAGE | EDUC = x) answer different questions because the
# information set is different.

# =============================================================================
# LAB STEP 3 — Does knowing education eliminate uncertainty?
# =============================================================================

# PREDICTION STOP:
# Ask whether wages within each education group will be tightly clustered or
# whether substantial dispersion will remain.

set.seed(3020)  # Only for reproducible horizontal jitter in the plot.
jittered_educ <- jitter(workers$educ, amount = 0.12)

plot(
  jittered_educ,
  workers$wage,
  pch = 16,
  xlab = "Years of education",
  ylab = "Hourly wage",
  xaxt = "n"
)
axis(1, at = c(12, 14, 16, 18))

# Add the conditional means as larger points.
points(
  group_means$educ,
  group_means$wage,
  pch = 19,
  cex = 2
)

# Add the conditional-mean relationship.
abline(a = -12, b = 2.5, lwd = 2)

# Check the 16-year group explicitly.
workers_16 <- subset(workers, educ == 16)
mean(workers_16$wage)   # Verified: 28
range(workers_16$wage)  # Verified: 20 to 36

# WHY: The conditional mean can be informative even though individual outcomes
# remain dispersed around it.

# =============================================================================
# LAB STEP 4 — Between-group versus within-group variation
# =============================================================================

# PREDICTION STOP:
# Ask whether most total wage variation will occur between education groups or
# within education groups.

# Population variance helper. We use mean squared deviation because the
# textbook decomposition is a population identity.
pop_var <- function(x) {
  mean((x - mean(x))^2)
}

total_variance <- pop_var(workers$wage)

# Give each worker the mean wage of their education group.
group_mean_for_each_worker <- ave(
  workers$wage,
  workers$educ,
  FUN = mean
)

# Variation in conditional means across workers.
between_variance <- mean(
  (group_mean_for_each_worker - mean(workers$wage))^2
)

# Average variation around conditional means.
within_variance <- mean(
  (workers$wage - group_mean_for_each_worker)^2
)

variance_decomposition <- c(
  total = total_variance,
  between = between_variance,
  within = within_variance,
  check = between_variance + within_variance,
  between_share = between_variance / total_variance,
  within_share = within_variance / total_variance
)

variance_decomposition

# Verified values:
# total         = 51.85
# between       = 31.25
# within        = 20.60
# check         = 51.85
# between_share = 0.6027001
# within_share  = 0.3972999

# Optional visual summary of the decomposition.
barplot(
  c(Between = between_variance, Within = within_variance),
  main = "Where does wage variation occur?",
  ylab = "Population variance"
)

# WHY: Education can account for substantial systematic variation while a large
# amount of uncertainty remains among workers with the same education.

# =============================================================================
# LAB STEP 5 — Where does regression appear?
# =============================================================================

# PREDICTION STOP:
# From the group means 18, 23, 28, 33 at education 12, 14, 16, 18, ask students
# to predict the intercept and slope before running lm().

model <- lm(wage ~ educ, data = workers)

coef(model)                 # Verified: intercept -12, slope 2.5
summary(model)$r.squared    # Verified: 0.6027001

# Compare regression R-squared with between-group share of total variance.
between_variance / total_variance

# Full regression output if useful for discussion.
summary(model)

# DELIBERATE MISCONCEPTION / FAILURE:
# "The model predicts $28 for someone with 16 years of education, therefore a
#  worker with 16 years of education should earn approximately $28."
# Correction: $28 is a fitted conditional mean, not a deterministic outcome.
#
# Second misconception:
# "The slope is 2.5, therefore another year of education causes wage to rise by
#  $2.50."
# Correction: the observed conditional relationship is not automatically causal.

# =============================================================================
# OPTIONAL: 60-second correlation trap
# =============================================================================

# PREDICTION STOP:
# Ask: "If correlation is approximately zero, are X and Y unrelated?"

set.seed(123)
x <- runif(1000, -1, 1)
y <- x^2

cor(x, y)
plot(
  x,
  y,
  pch = 16,
  cex = 0.6,
  main = "Zero linear correlation does not imply independence",
  xlab = "X",
  ylab = "Y = X^2"
)

# WHY: Correlation measures linear association. Here Y is completely determined
# by X even though the linear correlation is approximately zero.

# =============================================================================
# FINAL CONSISTENCY CHECKS — should all return TRUE
# =============================================================================

stopifnot(isTRUE(all.equal(mean(workers$wage), 25.5)))
stopifnot(isTRUE(all.equal(total_variance, 51.85)))
stopifnot(isTRUE(all.equal(between_variance, 31.25)))
stopifnot(isTRUE(all.equal(within_variance, 20.6)))
stopifnot(isTRUE(all.equal(unname(coef(model)), c(-12, 2.5), tolerance = 1e-12)))
stopifnot(isTRUE(all.equal(summary(model)$r.squared, between_variance / total_variance)))

message("Lecture 02 consistency checks passed.")
