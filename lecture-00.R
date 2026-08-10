# Lecture 00 — Why Study Econometrics?
# From Economic Questions to Credible Answers
#
# Standalone live-lab script.
# The script uses only base R and constructs a deterministic synthetic dataset.
# No external packages or files are required.
#
# Pedagogical anchor:
#   Should UiT conclude that requiring students to attend econometrics lectures
#   would improve their grades?
#
# IMPORTANT: The data are synthetic and designed for teaching. They are not
# UiT student data and should never be presented as such.

# -----------------------------------------------------------------------------
# 0. SETUP — construct the synthetic student dataset
# -----------------------------------------------------------------------------

# Population-standardization helper. Using the population SD makes this script
# numerically identical to the deterministic construction used for the lecture.
zpop <- function(x) {
  (x - mean(x)) / sqrt(mean((x - mean(x))^2))
}

n <- 180
student <- 1:n

# Deterministic latent variation. Trigonometric sequences are used instead of
# random-number generation so the lecture results are exactly reproducible.
u1 <- zpop(sin(student * 1.73) + 0.30 * cos(student * 0.37))
u2 <- zpop(cos(student * 1.11) + 0.40 * sin(student * 0.53))
u3 <- zpop(sin(student * 0.83) + 0.35 * cos(student * 1.91))
u4 <- zpop(cos(student * 0.63) + 0.40 * sin(student * 1.37))
u5 <- zpop(sin(student * 0.41) + 0.45 * cos(student * 2.07))
u6 <- zpop(cos(student * 0.29) + 0.33 * sin(student * 2.31))

# Measured and unmeasured student characteristics.
motivation <- u1
ability <- 0.50 * motivation + 0.90 * u5   # deliberately unobserved later
job_hours <- 12 + 5 * u2 - 1.20 * motivation + 1.20 * u4
study <- 10 + 1.40 * motivation - 0.06 * job_hours + 2.00 * u3 + 0.60 * ability

# Students with stronger motivation/ability tend to skip less; job commitments
# and other unmeasured influences also affect attendance.
skip <- 25 - 5.00 * motivation + 0.22 * job_hours - 0.45 * study +
  5.00 * u4 - 1.50 * ability

# The true direct effect embedded in the synthetic data is deliberately small:
# one percentage point more absence lowers grade by only 0.08 points, holding
# all other determinants fixed. Observational selection makes the naive
# regression look much larger.
grade <- 65 - 0.08 * skip + 0.40 * study + 2.50 * motivation -
  0.20 * job_hours + 1.00 * ability + 2.80 * u6

students <- data.frame(
  student = student,
  grade = grade,
  skip = skip,
  study = study,
  motivation = motivation,
  job_hours = job_hours
)

cat("Synthetic dataset created:", nrow(students), "students\n\n")

# -----------------------------------------------------------------------------
# LAB 1 — Is attendance informative?
# -----------------------------------------------------------------------------
# QUESTION: Can lecture attendance help us predict exam performance?
# PREDICTION STOP: Ask students for the expected sign before running the model.

plot(
  students$skip, students$grade,
  xlab = "Lectures skipped (%)",
  ylab = "Exam grade",
  main = "Attendance is strongly associated with grades",
  pch = 19
)

m1 <- lm(grade ~ skip, data = students)
abline(m1, lwd = 2)

cat("LAB 1 — Model 1: grade ~ skip\n")
print(summary(m1))
cat("\nKey values:\n")
cat("  Intercept =", round(coef(m1)["(Intercept)"], 3), "\n")
cat("  Skip coefficient =", round(coef(m1)["skip"], 3), "\n")
cat("  R-squared =", round(summary(m1)$r.squared, 3), "\n")
cat("  Predicted grade change for +10 pp absence =",
    round(10 * coef(m1)["skip"], 2), "points\n\n")

# EXPECTED RESULT:
#   intercept ≈ 76.103
#   skip ≈ -0.497
#   R^2 ≈ 0.586
# INTERPRETATION:
#   Students who skip 10 percentage points more lectures are predicted to score
#   about 5 points lower.
# DECISION:
#   Attendance is informative for prediction. Causality has NOT been established.

# -----------------------------------------------------------------------------
# LAB 2 — Look behind the coefficient
# -----------------------------------------------------------------------------
# QUESTION: Are students who skip more also different in study effort?
# PREDICTION STOP: Higher absence -> higher, lower, or unrelated study time?

plot(
  students$skip, students$study,
  xlab = "Lectures skipped (%)",
  ylab = "Study hours per week",
  main = "Students who skip more also tend to study less",
  pch = 19
)
abline(lm(study ~ skip, data = students), lwd = 2)

skip_study_cor <- cor(students$skip, students$study)
cat("LAB 2 — Correlation between skip and study =",
    round(skip_study_cor, 3), "\n\n")

# EXPECTED RESULT:
#   cor(skip, study) ≈ -0.627
# INTERPRETATION:
#   The original regression compares students who differ not only in attendance,
#   but also in study behaviour.

# -----------------------------------------------------------------------------
# LAB 3 — What happens when we change the specification?
# -----------------------------------------------------------------------------
# QUESTION: What is the attendance relationship among students with the same
# measured study time?
# PREDICTION STOP: Will the skip coefficient become more negative, less negative,
# or stay approximately unchanged?

m2 <- lm(grade ~ skip + study, data = students)

cat("LAB 3 — Model 2: grade ~ skip + study\n")
print(summary(m2))
cat("\nKey value:\n")
cat("  Skip coefficient =", round(coef(m2)["skip"], 3), "\n")
cat("  Predicted grade change for +10 pp absence =",
    round(10 * coef(m2)["skip"], 2), "points\n\n")

# EXPECTED RESULT:
#   skip ≈ -0.350
# INTERPRETATION:
#   Part of the -0.497 naive relationship reflected systematic differences in
#   study effort. The model is answering a different conditional question.

# -----------------------------------------------------------------------------
# LAB 4 — DELIBERATE FAILURE / MISCONCEPTION
# -----------------------------------------------------------------------------
# QUESTION: Could the initial result mainly reflect who skips rather than only
# what skipping does?
# PREDICTION STOP: What happens when motivation and paid work are added?

m3 <- lm(grade ~ skip + study + motivation + job_hours, data = students)

cat("LAB 4 — Model 3: grade ~ skip + study + motivation + job_hours\n")
print(summary(m3))
cat("\nKey value:\n")
cat("  Skip coefficient =", round(coef(m3)["skip"], 3), "\n")
cat("  Predicted grade change for +10 pp absence =",
    round(10 * coef(m3)["skip"], 2), "points\n\n")

cat("Coefficient path:\n")
coef_path <- c(
  `Model 1: skip only` = coef(m1)["skip"],
  `Model 2: + study` = coef(m2)["skip"],
  `Model 3: + motivation + job hours` = coef(m3)["skip"]
)
print(round(coef_path, 3))
cat("\n")

# EXPECTED RESULT:
#   -0.497 -> -0.350 -> -0.148
# DELIBERATE FAILURE:
#   The naive model produces a real and useful association, but interpreting
#   -0.497 as the causal effect of attendance badly overstates the direct effect.
# INSTRUCTOR-ONLY FACT:
#   The synthetic data-generating process contains a direct skip effect of -0.08.
#   Model 3 is still observational and does not perfectly recover that value,
#   because ability remains unobserved.

# Optional visual summary of specification sensitivity.
plot(
  1:3,
  as.numeric(coef_path),
  type = "b",
  xaxt = "n",
  xlab = "Specification",
  ylab = "Coefficient on skip",
  main = "The attendance coefficient changes with specification"
)
axis(1, at = 1:3, labels = c("Skip only", "+ Study", "+ Motivation + Job"))
abline(h = -0.08, lty = 2)

# -----------------------------------------------------------------------------
# LAB 5 — Was Model 1 useless?
# -----------------------------------------------------------------------------
# QUESTION: If the goal is simply to identify students at risk, should Model 1
# be discarded because it is not a credible causal model?

cat("LAB 5 — Predictive usefulness versus causal interpretation\n")
cat("  Model 1 R-squared =", round(summary(m1)$r.squared, 3), "\n")
cat("  Model 2 R-squared =", round(summary(m2)$r.squared, 3), "\n")
cat("  Model 3 R-squared =", round(summary(m3)$r.squared, 3), "\n\n")

# INTERPRETATION:
#   No. Model 1 can be useful for prediction/risk flagging even though its skip
#   coefficient should not be interpreted as the effect of forcing attendance.

# -----------------------------------------------------------------------------
# CONSISTENCY CHECK — numbers used in the lecture page
# -----------------------------------------------------------------------------
expected <- c(m1 = -0.497296, m2 = -0.349974, m3 = -0.147525)
observed <- c(
  m1 = unname(coef(m1)["skip"]),
  m2 = unname(coef(m2)["skip"]),
  m3 = unname(coef(m3)["skip"])
)

stopifnot(max(abs(expected - observed)) < 1e-5)
stopifnot(abs(skip_study_cor - (-0.626911)) < 1e-5)

cat("Consistency check passed. Lecture numbers reproduce correctly.\n")

# -----------------------------------------------------------------------------
# OPTIONAL — reproduce the static figures used by lecture-00.qmd
# -----------------------------------------------------------------------------
# Run this section from the lecture-00 directory. It creates/updates figures/.

if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)

png("figures/lecture-00-grade-skip.png", width = 1280, height = 800, res = 160)
plot(
  students$skip, students$grade,
  xlab = "Lectures skipped (%)",
  ylab = "Exam grade",
  main = "Attendance is strongly associated with grades",
  pch = 19
)
abline(m1, lwd = 2)
dev.off()

png("figures/lecture-00-skip-study.png", width = 1280, height = 800, res = 160)
plot(
  students$skip, students$study,
  xlab = "Lectures skipped (%)",
  ylab = "Study hours per week",
  main = "Students who skip more also tend to study less",
  pch = 19
)
abline(lm(study ~ skip, data = students), lwd = 2)
dev.off()

png("figures/lecture-00-coefficient-path.png", width = 1280, height = 800, res = 160)
plot(
  1:3,
  as.numeric(coef_path),
  type = "b",
  xaxt = "n",
  xlab = "Specification",
  ylab = "Coefficient on skip",
  main = "The attendance coefficient changes with specification"
)
axis(1, at = 1:3, labels = c("Skip only", "+ Study", "+ Motivation + Job"))
abline(h = -0.08, lty = 2)
dev.off()

cat("Static lecture figures written to figures/.\n")

