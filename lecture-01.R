# ============================================================
# Lecture 1: What Does an Economic Effect Mean?
# Introductory Econometrics
#
# Purpose:
#   Reproduce the numerical results and plots used in Lecture 1.
#   Base R only; no external packages required.
#
# Textbook basis:
#   Principles of Econometrics, 5th ed., Appendix A,
#   especially A.1.5-A.1.6, A.2-A.3.2, Exercise A.2.
# ============================================================

options(digits = 6)

# ------------------------------------------------------------
# 1. Define the three candidate economic relationships
# ------------------------------------------------------------

# Model 1: log-log relationship.
# The coefficient -0.5 is a constant elasticity.
m1 <- function(I) exp(7.5 - 0.5 * log(I))

# Model 2: quadratic relationship.
# Its marginal effect changes with income.
m2 <- function(I) 1400 - 100 * I + 1.67 * I^2

# Model 3: linear relationship.
# Its level marginal effect is constant at -50.
m3 <- function(I) 1500 - 50 * I

# ------------------------------------------------------------
# LAB 1 — What do the three models actually look like?
# ------------------------------------------------------------

# PREDICTION POINT:
# Before plotting, ask which two curves students expect to
# diverge most as income rises.

income <- seq(0.1, 30, length.out = 300)

# Create a figures directory if the script is run from the
# lecture-01 folder.
dir.create("figures", showWarnings = FALSE)

png("figures/lecture-01-models.png", width = 1200, height = 750, res = 140)
matplot(
  income,
  cbind(m1(income), m2(income), m3(income)),
  type = "l",
  lty = 1:3,
  lwd = 3,
  xlab = "Income ($1000)",
  ylab = "Predicted infant mortality",
  main = "Three functional forms, one substantive relationship"
)
abline(h = 0, lty = 3)
legend(
  "topright",
  legend = c("Log-log", "Quadratic", "Linear"),
  lty = 1:3,
  lwd = 3,
  bty = "n"
)
dev.off()

# ------------------------------------------------------------
# LAB 2 — Does 'the effect of income' have one value?
# ------------------------------------------------------------

# Marginal effects.
# For the log-log model, M = exp(7.5) * I^(-0.5), so
# dM/dI = -0.5 * M / I.
dm1 <- function(I) -0.5 * m1(I) / I

dm2 <- function(I) -100 + 3.34 * I

dm3 <- function(I) rep(-50, length(I))

# PREDICTION POINT:
# Which model's marginal effect changes most between I=1 and I=25?

I_test <- c(1, 3, 25)

marginal_effects <- data.frame(
  income = I_test,
  loglog = dm1(I_test),
  quadratic = dm2(I_test),
  linear = dm3(I_test)
)

cat("\nLAB 2: Marginal effects\n")
print(marginal_effects, row.names = FALSE)

# Expected values (rounded):
# income   loglog  quadratic  linear
#      1 -904.021    -96.66     -50
#      3 -173.979    -89.98     -50
#     25   -7.232    -16.50     -50

png("figures/lecture-01-marginal-effects.png", width = 1200, height = 750, res = 140)
plot(
  income,
  dm1(income),
  type = "l",
  lwd = 3,
  ylim = range(c(dm1(income), dm2(income), dm3(income))),
  xlab = "Income ($1000)",
  ylab = "Marginal effect dM/dI",
  main = "Marginal effects depend on functional form"
)
lines(income, dm2(income), lty = 2, lwd = 3)
lines(income, dm3(income), lty = 3, lwd = 3)
abline(h = 0, lty = 3)
legend(
  "bottomright",
  legend = c("Log-log", "Quadratic", "Linear"),
  lty = 1:3,
  lwd = 3,
  bty = "n"
)
dev.off()

# ------------------------------------------------------------
# LAB 3 — Ask the question in percentages: elasticities
# ------------------------------------------------------------

# Elasticity = (dM/dI) * I/M.
e1 <- function(I) dm1(I) * I / m1(I)
e2 <- function(I) dm2(I) * I / m2(I)
e3 <- function(I) dm3(I) * I / m3(I)

elasticities <- data.frame(
  income = I_test,
  loglog = e1(I_test),
  quadratic = e2(I_test),
  linear = e3(I_test)
)

cat("\nLAB 3: Elasticities\n")
print(elasticities, row.names = FALSE)

# Expected values (rounded):
# income loglog quadratic  linear
#      1   -0.5    -0.074  -0.034
#      3   -0.5    -0.242  -0.111
#     25   -0.5     7.333  -5.000

# DELIBERATE MISCONCEPTION:
# A purely mechanical reading might interpret +7.333 at I=25
# as evidence that higher income raises mortality. Do not accept
# that interpretation before inspecting the predicted outcome.

# ------------------------------------------------------------
# LAB 4 — Deliberate failure: inspect predictions first
# ------------------------------------------------------------

predictions <- data.frame(
  income = I_test,
  mortality_loglog = m1(I_test),
  mortality_quadratic = m2(I_test),
  mortality_linear = m3(I_test)
)

cat("\nLAB 4: Predicted mortality\n")
print(predictions, row.names = FALSE)

# At I=25 the quadratic predicts -56.25.
stopifnot(abs(m2(25) - (-56.25)) < 1e-10)
stopifnot(abs(dm2(25) - (-16.5)) < 1e-10)
stopifnot(abs(e2(25) - 7.333333333333333) < 1e-10)

# Find both roots of the quadratic using the quadratic formula.
a <- 1.67
b <- -100
c <- 1400
disc <- b^2 - 4 * a * c
roots <- sort(c((-b - sqrt(disc)) / (2 * a),
                (-b + sqrt(disc)) / (2 * a)))

cat("\nQuadratic zero-mortality points:\n")
print(roots)

# Verify the first root with uniroot, matching the lecture page.
first_root <- uniroot(m2, c(20, 25))$root
cat("\nFirst zero from uniroot:\n")
print(first_root)

stopifnot(abs(first_root - 22.31860954) < 1e-6)

# ------------------------------------------------------------
# LAB 5 — Domain matters
# ------------------------------------------------------------

cat("\nLAB 5: Domain check for the log-log model\n")
cat("m1(0) = ")
print(m1(0))

# R returns Inf because log(0) = -Inf and the transformed
# expression explodes. Economically/mathematically, the log-log
# model requires I > 0.

# ------------------------------------------------------------
# Additional visual: elasticities across income
# ------------------------------------------------------------

# Avoid the singularity where the quadratic prediction is zero.
income_e <- seq(0.2, 30, length.out = 800)
q_pred <- m2(income_e)
q_elast <- e2(income_e)
q_elast[abs(q_pred) < 15] <- NA  # omit values near division by zero

png("figures/lecture-01-elasticities.png", width = 1200, height = 750, res = 140)
plot(
  income_e,
  e1(income_e),
  type = "l",
  lwd = 3,
  ylim = c(-6, 6),
  xlab = "Income ($1000)",
  ylab = "Elasticity",
  main = "Elasticity is a different object from the marginal effect"
)
lines(income_e, q_elast, lty = 2, lwd = 3)
lines(income_e, e3(income_e), lty = 3, lwd = 3)
abline(h = 0, lty = 3)
abline(v = first_root, lty = 3)
legend(
  "bottomleft",
  legend = c("Log-log", "Quadratic", "Linear", "Quadratic M=0"),
  lty = c(1, 2, 3, 3),
  lwd = c(3, 3, 3, 1),
  bty = "n"
)
dev.off()

# ------------------------------------------------------------
# Numerical consistency checks used in the lecture page
# ------------------------------------------------------------

stopifnot(abs(m1(25) - 361.6084828912126) < 1e-8)
stopifnot(abs(m3(25) - 250) < 1e-10)
stopifnot(all(abs(e1(I_test) + 0.5) < 1e-12))
stopifnot(abs(e3(25) - (-5)) < 1e-12)

cat("\nAll lecture numerical consistency checks passed.\n")
cat("Plots written to the figures/ directory.\n")
