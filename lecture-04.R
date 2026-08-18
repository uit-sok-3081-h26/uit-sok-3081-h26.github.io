# Lecture 04: From Uncertainty to Evidence
# When Is an Estimate Different Enough?
#
# Standalone live-lab script. No external packages or data files are required.
# The numbered sections match lecture-04.qmd exactly.
#
# This lecture continues directly from Lecture 03. It reconstructs the same
# synthetic wage population and the same anchor sample, then uses that sample
# to test whether the firm's NOK 300/hour budgeting assumption is too low.

# -----------------------------------------------------------------------------
# SETUP: Reconstruct the Lecture 03 synthetic population and anchor sample
# -----------------------------------------------------------------------------

population_size <- 100000L
target_mean <- 315
target_sd <- 105
shape_k <- 3

u_grid <- (seq_len(population_size) - 0.5) / population_size
z <- u_grid + shape_k * u_grid^3
scale_wage <- target_sd / sqrt(mean((z - mean(z))^2))
base_wage <- target_mean - scale_wage * mean(z)
wage_population <- base_wage + scale_wage * z

population_mean <- mean(wage_population)
population_sd <- sqrt(mean((wage_population - population_mean)^2))

# Deterministic pseudo-random generator, identical to Lecture 03.
lcg_uniform <- function(n, seed) {
  modulus <- 2^32
  multiplier <- 1664525
  increment <- 1013904223
  state <- seed %% modulus
  out <- numeric(n)

  for (i in seq_len(n)) {
    state <- (multiplier * state + increment) %% modulus
    out[i] <- state / modulus
  }

  out
}

draw_wages <- function(n, seed, target_population_mean = 315) {
  u <- lcg_uniform(n, seed)
  idx <- floor(u * length(wage_population)) + 1L
  wage_population[idx] + (target_population_mean - 315)
}

# For the Type I / power simulation we use normally distributed wages so the
# textbook t-test assumptions are exactly represented and the nominal 5% Type I
# error rate is easy to see.
draw_normal_wages <- function(n, seed, mu = 300, sigma = 105) {
  u <- lcg_uniform(n, seed)
  u <- pmin(pmax(u, 1e-12), 1 - 1e-12)
  qnorm(u, mean = mu, sd = sigma)
}

simulate_normal_samples <- function(n, B, seed, mu = 300, sigma = 105) {
  values <- draw_normal_wages(n * B, seed, mu, sigma)
  matrix(values, nrow = B, ncol = n, byrow = TRUE)
}

sample_t_statistics <- function(samples, mu0 = 300) {
  n <- ncol(samples)
  sample_means <- rowMeans(samples)
  sample_sds <- apply(samples, 1, sd)
  (sample_means - mu0) / (sample_sds / sqrt(n))
}

figure_dir <- "figures"
if (!dir.exists(figure_dir)) dir.create(figure_dir, recursive = TRUE)

# Same sample used in Lecture 03.
anchor_seed <- 4820
wage <- draw_wages(50, anchor_seed)

anchor_mean <- mean(wage)
anchor_sd <- sd(wage)
anchor_se <- anchor_sd / sqrt(length(wage))

round(c(
  sample_mean = anchor_mean,
  sample_sd = anchor_sd,
  sample_se = anchor_se
), 2)
# Verified: mean 325.42, SD 102.98, SE 14.56.

# =============================================================================
# LAB STEP 1 — How far is 325.42 from 300?
# =============================================================================

# QUESTION: Is a NOK 25.42 difference large relative to sampling uncertainty?
# PREDICTION STOP:
# Ask how persuasive the same raw difference would be if the SE were 50 versus 1.

mu0 <- 300
n <- length(wage)
df <- n - 1

t_stat <- (anchor_mean - mu0) / anchor_se
round(t_stat, 4)
# Verified: 1.7457.

# INTERPRETATION:
# The estimate lies about 1.75 standard errors above the null boundary of 300.

# =============================================================================
# LAB STEP 2 — What would t look like if 300 were really true?
# =============================================================================

# QUESTION: Under H0: mu = 300, how unusual is t = 1.7457?
# PREDICTION STOP:
# Most t-statistics generated under H0 should be near zero.

alpha <- 0.05
t_critical_one <- qt(1 - alpha, df = df)
p_one <- 1 - pt(t_stat, df = df)

round(c(
  t_observed = t_stat,
  critical_value = t_critical_one,
  one_sided_p = p_one
), 4)
# Verified: t = 1.7457, critical value = 1.6766, p = 0.0436.

# Theoretical null distribution plot.
x <- seq(-4, 4, length.out = 1000)
y <- dt(x, df = df)
plot(
  x, y, type = "l", lwd = 2,
  main = "One-sided test under H0: mean wage = 300",
  xlab = "t-statistic", ylab = "Density"
)
abline(v = t_stat, lty = 2, lwd = 2)
abline(v = t_critical_one, lty = 3, lwd = 2)

png(file.path(figure_dir, "one-sided-test.png"), width = 1280, height = 768, res = 150)
plot(
  x, y, type = "l", lwd = 2,
  main = "One-sided test: how unusual is the observed t-statistic under H0?",
  xlab = "t-statistic under H0: mean wage = 300", ylab = "Density"
)
ix <- x >= t_stat
polygon(c(t_stat, x[ix], max(x[ix])), c(0, y[ix], 0), density = 18)
abline(v = t_stat, lty = 2, lwd = 2)
abline(v = t_critical_one, lty = 3, lwd = 2)
legend(
  "topright",
  legend = c(
    paste0("Observed t = ", round(t_stat, 3)),
    paste0("5% critical value = ", round(t_critical_one, 3)),
    paste0("One-sided p = ", round(p_one, 4))
  ),
  lty = c(2, 3, NA), lwd = c(2, 2, NA), bty = "n"
)
dev.off()

# DECISION:
# At alpha = 0.05, reject H0: mu <= 300 in favor of H1: mu > 300.

# =============================================================================
# LAB STEP 3 — The significance threshold is a decision rule
# =============================================================================

# QUESTION: Does the same evidence lead to the same decision at 10%, 5%, and 1%?
# PREDICTION STOP:
# The p-value stays fixed; only the decision threshold changes.

significance_decisions <- data.frame(
  alpha = c(0.10, 0.05, 0.01),
  p_value = p_one,
  reject = p_one <= c(0.10, 0.05, 0.01)
)
significance_decisions

# Verified: reject at 10% and 5%, do not reject at 1%.

# INTERPRETATION:
# Evidence is continuous. The significance decision is a rule placed on it.

# =============================================================================
# LAB STEP 4 — DELIBERATE FAILURE: the confidence-interval "paradox"
# =============================================================================

# QUESTION: How can 300 lie inside the previous 95% CI while a 5% test rejects it?
# PREDICTION STOP:
# Ask whether a 95% two-sided CI corresponds to a one-sided or two-sided 5% test.

t_critical_two <- qt(0.975, df = df)
p_two <- 2 * (1 - pt(abs(t_stat), df = df))
ci_two <- anchor_mean + c(-1, 1) * t_critical_two * anchor_se
lower_bound_one <- anchor_mean - t_critical_one * anchor_se

round(c(
  two_sided_p = p_two,
  ci95_two_sided_lo = ci_two[1],
  ci95_two_sided_hi = ci_two[2],
  lower_95_one_sided = lower_bound_one
), 4)
# Verified:
# two-sided p = 0.0871
# two-sided 95% CI = [296.1575, 354.6901]
# one-sided 95% lower bound = 301.0075

# Two-sided test: do NOT reject H0: mu = 300 at alpha = .05.
p_two <= 0.05

# One-sided test: reject H0: mu <= 300 at alpha = .05.
p_one <= 0.05

png(file.path(figure_dir, "one-vs-two-sided.png"), width = 1280, height = 768, res = 150)
plot(x, y, type = "l", lwd = 2,
     main = "The same t-statistic answers different questions with different tails",
     xlab = "t-statistic", ylab = "Density")
abline(v = c(-abs(t_stat), abs(t_stat)), lty = 2)
legend("topright",
       legend = c(paste0("One-sided p = ", round(p_one, 4)),
                  paste0("Two-sided p = ", round(p_two, 4))),
       bty = "n")
dev.off()

# INTERPRETATION:
# The 95% two-sided CI is equivalent to a two-sided 5% test, not to a one-sided
# 5% test. The matching one-sided 95% lower bound is above 300.

# =============================================================================
# LAB STEP 5 — DELIBERATE FAILURE: choose the tail after seeing the estimate
# =============================================================================

# MISLEADING REASONING:
# "The sample mean is above 300, therefore I should use H1: mu > 300."
#
# CORRECTION:
# The alternative is determined by the economic/research question before looking
# at the observed sign. If the original question was simply "has the mean changed?",
# the appropriate alternative remains H1: mu != 300.

# No new computation is required here. The point is specification discipline.

# =============================================================================
# LAB STEP 6 — Type I error, Type II error, and power
# =============================================================================

# To make the nominal error rates transparent, use a normal population with
# sigma = 105, matching the scale of the wage example.

# QUESTION A: If H0 is true, do we ever reject it?
# PREDICTION STOP:
# At alpha = .05, a valid test should reject a true H0 about 5% of the time.

null_samples <- simulate_normal_samples(
  n = 50, B = 10000, seed = 51050, mu = 300, sigma = 105
)
null_t <- sample_t_statistics(null_samples, mu0 = 300)
type1_rate <- mean(null_t >= qt(0.95, df = 49))
round(type1_rate, 4)
# Verified: 0.0493.

# QUESTION B: If the true mean is 325, how does N affect our chance of rejecting H0?
# PREDICTION STOP:
# The economic difference stays 25 NOK, but larger N should improve detectability.

power_results <- data.frame(N = c(20, 50, 200), power = NA_real_)
power_seeds <- c(51220, 51250, 51400)

for (j in seq_len(nrow(power_results))) {
  n_j <- power_results$N[j]
  alt_samples <- simulate_normal_samples(
    n = n_j, B = 5000, seed = power_seeds[j], mu = 325, sigma = 105
  )
  alt_t <- sample_t_statistics(alt_samples, mu0 = 300)
  crit_j <- qt(0.95, df = n_j - 1)
  power_results$power[j] <- mean(alt_t >= crit_j)
}

round(power_results, 4)
# Verified simulated power:
# N = 20  -> 0.2720
# N = 50  -> 0.5136
# N = 200 -> 0.9510

# Theoretical power under a normal model uses the noncentral t distribution.
N_grid <- 10:250
ncp <- (325 - 300) / (105 / sqrt(N_grid))
critical_grid <- qt(0.95, df = N_grid - 1)
theoretical_power <- 1 - pt(critical_grid, df = N_grid - 1, ncp = ncp)

png(file.path(figure_dir, "power-sample-size.png"), width = 1280, height = 768, res = 150)
plot(
  N_grid, theoretical_power, type = "l", lwd = 2, ylim = c(0, 1),
  main = "Larger samples improve the chance of detecting a real difference",
  xlab = "Sample size N", ylab = "Probability of rejecting H0"
)
points(power_results$N, power_results$power, pch = 19)
abline(h = 0.05, lty = 3)
legend(
  "bottomright",
  legend = c("Theoretical power, true mean = 325", "Simulated rejection rates", "Nominal Type I rate = 5%"),
  lty = c(1, NA, 3), pch = c(NA, 19, NA), lwd = c(2, NA, 1), bty = "n"
)
dev.off()

# INTERPRETATION:
# Larger N reduces Type II error and raises power, even though the underlying
# economic difference (25 NOK/hour) has not changed.

# =============================================================================
# FINAL CONSISTENCY CHECKS — should all return TRUE
# =============================================================================

stopifnot(abs(anchor_mean - 325.423821971289) < 1e-8)
stopifnot(abs(anchor_sd - 102.978874073191) < 1e-8)
stopifnot(abs(anchor_se - 14.563412035222) < 1e-8)
stopifnot(abs(t_stat - 1.745732518575) < 1e-8)
stopifnot(abs(p_one - 0.043562650143) < 1e-8)
stopifnot(abs(p_two - 0.087125300286) < 1e-8)
stopifnot(abs(ci_two[1] - 296.157549777197) < 1e-8)
stopifnot(abs(ci_two[2] - 354.690094165380) < 1e-8)
stopifnot(abs(lower_bound_one - 301.007520524091) < 1e-8)
stopifnot(abs(type1_rate - 0.0493) < 5e-4)
stopifnot(abs(power_results$power[power_results$N == 20] - 0.2720) < 5e-4)
stopifnot(abs(power_results$power[power_results$N == 50] - 0.5136) < 5e-4)
stopifnot(abs(power_results$power[power_results$N == 200] - 0.9510) < 5e-4)

message("Lecture 04 consistency checks passed.")
