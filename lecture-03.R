# Lecture 03: From Sample to Population
# How Certain Is an Estimate?
#
# Standalone live-lab script. No external packages or data files are required.
# The numbered sections match lecture-03.qmd exactly.
#
# The script constructs a fixed, mildly right-skewed synthetic population of
# hourly wages with population mean 315 NOK and population SD 105 NOK. A simple
# linear-congruential pseudo-random generator is used so the same numerical
# teaching results can be independently verified without relying on R's RNG
# implementation or version.

# -----------------------------------------------------------------------------
# SETUP: Construct the synthetic wage population and reproducible sampler
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

# A deterministic pseudo-random generator. All intermediate integer values are
# below 2^53, so they are represented exactly by R doubles.
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

draw_wages <- function(n, seed) {
  u <- lcg_uniform(n, seed)
  idx <- floor(u * length(wage_population)) + 1L
  wage_population[idx]
}

simulate_samples <- function(n, B, seed) {
  values <- draw_wages(n * B, seed)
  matrix(values, nrow = B, ncol = n, byrow = TRUE)
}

# Optional output folder for the figures used in the lecture page.
figure_dir <- "figures"
if (!dir.exists(figure_dir)) dir.create(figure_dir, recursive = TRUE)

# Inspect the population used for the teaching simulation.
round(c(
  population_mean = population_mean,
  population_sd = population_sd,
  minimum = min(wage_population),
  maximum = max(wage_population)
), 2)
# Verified: mean 315.00, SD 105.00, range about 197.91 to 572.59.

png(file.path(figure_dir, "population-wages.png"), width = 1280, height = 768, res = 150)
hist(
  wage_population,
  breaks = 45,
  main = "Synthetic population of hourly wages",
  xlab = "Hourly wage (NOK)"
)
abline(v = population_mean, lwd = 2)
dev.off()

# =============================================================================
# LAB STEP 1 — One sample gives one answer
# =============================================================================

# QUESTION: What would we conclude if this were the only sample we observed?
# PREDICTION STOP:
# The sample mean should be near the population mean but almost never equal to it.

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

# INTERPRETATION:
# 325.42 is one estimate of the unknown population mean. It is not a population
# fact, and another random sample would generally produce another number.

# DECISION:
# Use the mean as a point estimate, but do not treat it as known without error.

# =============================================================================
# LAB STEP 2 — What if we repeated the study?
# =============================================================================

# QUESTION: What would happen if we repeatedly sampled 50 workers?
# PREDICTION STOP:
# The estimates should differ, but their distribution should be centered near 315.

samples_50 <- simulate_samples(n = 50, B = 5000, seed = 31050)
means_50 <- rowMeans(samples_50)

round(c(
  mean_of_sample_means = mean(means_50),
  sd_of_sample_means = sd(means_50)
), 2)
# Verified: 314.86 and 14.81.

hist(
  means_50,
  breaks = 35,
  main = "Sampling distribution of the sample mean, N = 50",
  xlab = "Sample mean hourly wage (NOK)"
)
abline(v = population_mean, lwd = 2)
abline(v = anchor_mean, lty = 2, lwd = 2)

png(file.path(figure_dir, "sampling-distribution-n50.png"), width = 1280, height = 768, res = 150)
hist(
  means_50,
  breaks = 35,
  main = "Sampling distribution of the sample mean, N = 50",
  xlab = "Sample mean hourly wage (NOK)"
)
abline(v = population_mean, lwd = 2)
abline(v = anchor_mean, lty = 2, lwd = 2)
legend(
  "topright",
  legend = c(
    paste0("Population mean = ", round(population_mean, 0)),
    paste0("Anchor sample mean = ", round(anchor_mean, 1))
  ),
  lty = c(1, 2),
  lwd = 2,
  bty = "n"
)
dev.off()

# INTERPRETATION:
# This distribution is the sampling distribution of the estimator, not the
# population distribution of individual wages.

# DECISION:
# The original sample mean is one realization from this distribution.

# =============================================================================
# LAB STEP 3 — What does sample size buy us?
# =============================================================================

# QUESTION: What changes when N grows from 10 to 50 to 200?
# PREDICTION STOP:
# Predict what happens to the center, spread, and shape before running the code.

samples_10 <- simulate_samples(n = 10, B = 5000, seed = 31010)
samples_200 <- simulate_samples(n = 200, B = 5000, seed = 31200)

means_10 <- rowMeans(samples_10)
means_200 <- rowMeans(samples_200)

precision_table <- data.frame(
  N = c(10, 50, 200),
  theoretical_SE = population_sd / sqrt(c(10, 50, 200)),
  simulated_SD_of_means = c(sd(means_10), sd(means_50), sd(means_200))
)

round(precision_table, 2)
# Verified:
#   N  theoretical_SE  simulated_SD_of_means
#  10       33.20              33.29
#  50       14.85              14.81
# 200        7.42               7.53

# Four times as many observations approximately halves the standard error.
round((population_sd / sqrt(50)) / (population_sd / sqrt(200)), 2)
# Verified: 2.00.

png(file.path(figure_dir, "sample-size-precision.png"), width = 1280, height = 768, res = 150)
d10 <- density(means_10)
d50 <- density(means_50)
d200 <- density(means_200)
plot(
  d10,
  main = "More observations narrow the sampling distribution",
  xlab = "Sample mean hourly wage (NOK)",
  ylab = "Density",
  lwd = 2,
  xlim = range(c(d10$x, d50$x, d200$x)),
  ylim = c(0, max(d10$y, d50$y, d200$y))
)
lines(d50, lwd = 2, lty = 2)
lines(d200, lwd = 2, lty = 3)
abline(v = population_mean, lwd = 1.5)
legend("topright", legend = c("N = 10", "N = 50", "N = 200"), lty = 1:3, lwd = 2, bty = "n")
dev.off()

# INTERPRETATION:
# More observations improve precision at the square-root rate: SE is proportional
# to 1/sqrt(N). Doubling N does not halve the SE; quadrupling N approximately does.

# DECISION:
# The firm can buy precision with more data, but there are diminishing returns.

# =============================================================================
# LAB STEP 4 — The CLT: do not look at the wrong distribution
# =============================================================================

# QUESTION: Which distribution needs to become approximately normal for inference
# about the population mean: individual wages, or the sample mean estimator?
# PREDICTION STOP:
# Compare the visibly right-skewed population with the distributions of sample means.

par(mfrow = c(1, 2))
hist(
  wage_population,
  breaks = 45,
  main = "Individual wages",
  xlab = "Hourly wage (NOK)"
)
hist(
  means_50,
  breaks = 35,
  main = "Sample means, N = 50",
  xlab = "Sample mean (NOK)"
)
par(mfrow = c(1, 1))

# INTERPRETATION:
# The population can be right-skewed while the sampling distribution of the mean
# is much closer to normal. That is the useful CLT result for inference here.

# DECISION:
# A skewed wage histogram does not by itself invalidate inference about the mean.

# =============================================================================
# LAB STEP 5 — DELIBERATE FAILURE: SD is not SE
# =============================================================================

# QUESTION: Which measure belongs in an interval for the population mean?
# PREDICTION STOP:
# Ask whether the following very wide interval looks like uncertainty about a mean
# or like variation among individual workers.

wrong_interval <- anchor_mean + c(-1, 1) * 1.96 * anchor_sd
round(wrong_interval, 2)
# Verified wrong interval: [123.59, 527.26].

# DELIBERATE MISCONCEPTION / FAILURE:
# "Use the standard deviation of wages as the uncertainty around the sample mean."
# This confuses variation between workers with variation between sample means.

# CORRECTION:
t_critical <- qt(0.975, df = length(wage) - 1)
correct_interval <- anchor_mean + c(-1, 1) * t_critical * anchor_se

round(c(
  t_critical = t_critical,
  lower = correct_interval[1],
  upper = correct_interval[2]
), 2)
# Verified: t = 2.01; CI = [296.16, 354.69].

# WHY:
# SD(wage) answers "How different are workers?"
# SE(mean) answers "How much would the estimated mean vary across repeated samples?"

# DECISION:
# The firm has much more precise information about average labour cost than the
# raw cross-worker wage dispersion would suggest.

# =============================================================================
# LAB STEP 6 — What does 95% actually mean?
# =============================================================================

# QUESTION: If we build 100 nominal 95% intervals, should all contain the true mean?
# PREDICTION STOP:
# No. Roughly 95% is a repeated-sampling property, not a guarantee for one interval.

ci_samples <- simulate_samples(n = 50, B = 100, seed = 35010)
ci_means <- rowMeans(ci_samples)
ci_sds <- apply(ci_samples, 1, sd)
ci_ses <- ci_sds / sqrt(50)
ci_lo <- ci_means - t_critical * ci_ses
ci_hi <- ci_means + t_critical * ci_ses
ci_cover <- ci_lo <= population_mean & population_mean <= ci_hi

sum(ci_cover)
mean(ci_cover)
# Verified for this reproducible set of 100 intervals: 95 and 0.95.

png(file.path(figure_dir, "confidence-interval-coverage.png"), width = 1280, height = 1100, res = 150)
plot(
  NA,
  xlim = range(c(ci_lo, ci_hi, population_mean)),
  ylim = c(1, 100),
  xlab = "Hourly wage (NOK)",
  ylab = "Repeated sample",
  main = paste0("100 repeated 95% confidence intervals: ", sum(ci_cover), " contain the true mean")
)
for (i in seq_len(100)) {
  segments(ci_lo[i], i, ci_hi[i], i, lwd = if (ci_cover[i]) 1 else 3)
  if (!ci_cover[i]) points(ci_means[i], i, pch = 16)
}
abline(v = population_mean, lwd = 2)
dev.off()

# OPTIONAL INSTRUCTOR DIAGNOSTIC:
# With a right-skewed finite population and N = 50, the t interval is an
# approximation. Over 10,000 repeated samples from this teaching population the
# empirical coverage is close to, but not exactly, 95%.
long_samples <- simulate_samples(n = 50, B = 10000, seed = 35050)
long_means <- rowMeans(long_samples)
long_sds <- apply(long_samples, 1, sd)
long_ses <- long_sds / sqrt(50)
long_lo <- long_means - t_critical * long_ses
long_hi <- long_means + t_critical * long_ses
long_coverage <- mean(long_lo <= population_mean & population_mean <= long_hi)
round(long_coverage, 4)
# Verified: approximately 0.9446.

# INTERPRETATION:
# Confidence attaches to the repeated procedure. A realized interval either
# contains the fixed population mean or it does not.

# DECISION ON THE ANCHOR:
# Use the sample mean as a point estimate, but communicate sampling uncertainty
# with its standard error and confidence interval.

# =============================================================================
# AI AUDIT NUMBERS — optional calculation check
# =============================================================================

ai_mean <- 326
ai_sd <- 103
ai_n <- 50
ai_se <- ai_sd / sqrt(ai_n)
ai_t <- qt(0.975, df = ai_n - 1)
ai_ci <- ai_mean + c(-1, 1) * ai_t * ai_se
round(c(SE = ai_se, lower = ai_ci[1], upper = ai_ci[2]), 2)
# Approximately SE = 14.57 and 95% CI = [296.72, 355.28].

# Increasing N from 50 to 200 with the same SD halves the standard error.
round(c(
  SE_N50 = ai_sd / sqrt(50),
  SE_N200 = ai_sd / sqrt(200)
), 2)
# Approximately 14.57 and 7.28.

# =============================================================================
# EXIT PROBLEM CHECK
# =============================================================================

salmon_mean <- 160
salmon_sd <- 30
salmon_n <- 100
salmon_se <- salmon_sd / sqrt(salmon_n)
salmon_ci <- salmon_mean + c(-1, 1) * 1.96 * salmon_se
round(c(SE = salmon_se, lower = salmon_ci[1], upper = salmon_ci[2]), 2)
# Verified: SE = 3.00; approximate interval [154.12, 165.88].

# =============================================================================
# FINAL CONSISTENCY CHECKS — should all return TRUE / stop silently
# =============================================================================

stopifnot(abs(population_mean - 315) < 1e-8)
stopifnot(abs(population_sd - 105) < 1e-8)
stopifnot(abs(anchor_mean - 325.423821971289) < 1e-6)
stopifnot(abs(anchor_sd - 102.978874073191) < 1e-6)
stopifnot(abs(anchor_se - 14.563412035222) < 1e-6)
stopifnot(abs(correct_interval[1] - 296.157549777197) < 1e-6)
stopifnot(abs(correct_interval[2] - 354.690094165380) < 1e-6)
stopifnot(abs(sd(means_10) - 33.294682011374) < 1e-6)
stopifnot(abs(sd(means_50) - 14.808244814843) < 1e-6)
stopifnot(abs(sd(means_200) - 7.529269653197) < 1e-6)
stopifnot(sum(ci_cover) == 95)
stopifnot(abs(long_coverage - 0.9446) < 1e-4)

message("Lecture 03 consistency checks passed.")
