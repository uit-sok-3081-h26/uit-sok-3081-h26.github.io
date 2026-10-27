# SOK-3081 | Chapter 6: Further Inference in Multiple Regression
# Run Part 0 once, then one numbered part at a time alongside the slides.
# Part 1-5: Session 1. Part 6-10: Session 2.
# The goal is to explain a result and defend a decision, not memorise commands.
# Requires base R only and an internet connection. No local data files are needed.

# Part 0: Setup and load the data --------------------------------------------
options(scipen = 10, digits = 4)

# Load the textbook data directly from the internet, as in Chapter 5.
load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/andy.rdata"))
# load() creates the data frame andy; do not write andy <- load(...).
stopifnot(nrow(andy) == 75, all(c('sales', 'price', 'advert') %in% names(andy)),
          !anyNA(andy))
# sales: monthly revenue, in THOUSANDS of dollars.
# price: price index, in dollars. advert: advertising, in THOUSANDS of dollars.
# Source: Hill, Griffiths and Lim, Principles of Econometrics, publisher's Andy data.

# Part 1: Back to Andy's quadratic model -------------------------------------
# PREDICT: Does a small p-value on advert alone answer whether advertising matters?
linear_model <- lm(sales ~ price + advert, data = andy)
quadratic_model <- lm(sales ~ price + advert + I(advert^2), data = andy)
summary(quadratic_model)
# The marginal advertising effect is beta_3 + 2*beta_4*advert.
# If BOTH advertising coefficients are zero, advertising has no effect here.
coef(quadratic_model)['advert'] +
  2 * coef(quadratic_model)['I(advert^2)'] * 1.9

# Part 2: A joint hypothesis and the cost of imposing it ----------------------
# H0: beta_3 = beta_4 = 0. H1: at least one of these coefficients is not zero.
# The restricted model imposes H0 by removing BOTH advertising terms.
restricted_model <- lm(sales ~ price, data = andy)
SSE_R <- sum(residuals(restricted_model)^2)
SSE_U <- sum(residuals(quadratic_model)^2)
N <- nobs(quadratic_model)
K <- length(coef(quadratic_model))  # Includes the intercept: K = 4.
J <- 2                            # Two independent restrictions.
F_advert <- ((SSE_R - SSE_U) / J) / (SSE_U / (N - K))
p_advert <- pf(F_advert, df1 = J, df2 = N - K, lower.tail = FALSE)
critical_F <- qf(0.95, df1 = J, df2 = N - K)
c(SSE_R = SSE_R, SSE_U = SSE_U, F = F_advert,
  critical_F = critical_F, p = p_advert)
# INTERPRET: compare the loss of fit per restriction with unrestricted error variance.
# Verify the SAME test using the nested-model comparison.
anova(restricted_model, quadratic_model)
# Both models must use the same outcome and the same observations.

# Part 3: Overall significance and F = t^2 ------------------------------------
# Overall H0: all THREE slopes are zero; the intercept remains unrestricted.
intercept_model <- lm(sales ~ 1, data = andy)
anova(intercept_model, quadratic_model)
# One restriction: is the squared advertising term needed?
curvature_test <- anova(linear_model, quadratic_model)
t_squared_term <- coef(summary(quadratic_model))['I(advert^2)', 't value']
c(F = curvature_test$F[2], t_squared = t_squared_term^2)
# F=t^2 applies to one linear restriction and a TWO-SIDED t-test,
# using the same model and conventional covariance estimator.
# An overall F-test does not establish causality or good future prediction.

# Part 4: A general restriction with an economic meaning ---------------------
# Is advertising of 1,900 dollars compatible with marginal revenue = marginal
# advertising expenditure? H0: beta_3 + 3.8*beta_4 = 1.
# This is ONE restriction even though it involves two coefficients.
b <- coef(quadratic_model)
V <- vcov(quadratic_model)
effect_1900 <- unname(b['advert'] + 3.8 * b['I(advert^2)'])
effect_variance <- V['advert', 'advert'] +
  3.8^2 * V['I(advert^2)', 'I(advert^2)'] +
  2 * 3.8 * V['advert', 'I(advert^2)']
effect_se <- sqrt(effect_variance)
t_1900 <- (effect_1900 - 1) / effect_se
p_1900 <- 2 * pt(-abs(t_1900), df = N - K)
c(marginal_revenue = effect_1900, SE = effect_se, t = t_1900, p = p_1900)
# Revenue per advertising dollar is not profit: production costs are unobserved.

# Part 5: Specification, omitted variables and precision ----------------------
# PREDICT: what might happen to the price estimate if we omit advertising?
coefficient_comparison <- data.frame(
  Model = c('Price only', 'Price + advertising', 'Quadratic advertising'),
  Price_coefficient = c(coef(restricted_model)['price'],
    coef(linear_model)['price'], coef(quadratic_model)['price']))
print(coefficient_comparison, row.names = FALSE)
cor(andy$price, andy$advert)
# Changes in estimates illustrate sensitivity; they do not prove a causal effect.
# Collinearity: ask how well the OTHER regressors predict advertising.
advert_auxiliary <- lm(advert ~ price + I(advert^2), data = andy)
VIF_advert <- 1 / (1 - summary(advert_auxiliary)$r.squared)
VIF_advert
# A quadratic and its linear term can be highly correlated by construction.
# A large VIF alone is not a reason to delete a theoretically needed term.

# Part 6: Freeze candidate models, then split the data ------------------------
# QUESTION: how well will these models predict sales in another similar city?
# Fix the candidate list BEFORE examining held-out sales.
# The flexible model is a teaching comparison, not a claim about the true curve.
candidate_formulas <- list(
  Linear = sales ~ price + advert,
  Quadratic = sales ~ price + advert + I(advert^2),
  Flexible = sales ~ price + poly(advert, degree = 6, raw = TRUE))
# poly(..., raw=TRUE) includes advert, advert^2, ..., advert^6.
# PREDICT: which model has the smallest TRAINING error? Must it win on TEST data?
set.seed(308106)
train_id <- sample(seq_len(nrow(andy)), size = floor(0.75 * nrow(andy)))
train <- andy[train_id, ]
test <- andy[-train_id, ]
c(training_rows = nrow(train), test_rows = nrow(test))
# Seed fixes the split for reproducibility. It does not make it representative.
# A random split assumes new cities resemble this sample. This is not a test
# of forecasting future months or transferring to a different population.
split_plot <- function() {
  plot(train$advert, train$sales, pch = 16, col = '#286099',
       xlab = 'Advertising (thousand dollars)',
       ylab = 'Monthly sales (thousand dollars)')
  points(test$advert, test$sales, pch = 17, col = '#B44832', cex = 1.2)
  legend('bottomright', c('Training: 56 cities', 'Test: 19 cities'),
         pch = c(16, 17), col = c('#286099', '#B44832'), bty = 'n')
}
if (interactive()) split_plot()

# Part 7: Fit using TRAINING observations only -------------------------------
train_linear <- lm(candidate_formulas$Linear, data = train)
train_quadratic <- lm(candidate_formulas$Quadratic, data = train)
train_flexible <- lm(candidate_formulas$Flexible, data = train)
train_models <- list(Linear = train_linear, Quadratic = train_quadratic,
                     Flexible = train_flexible)
# Root mean squared error is in the same units as sales.
# Training RMSE divides by n_train, not by residual degrees of freedom.
training_RMSE <- c(
  Linear = sqrt(mean(residuals(train_linear)^2)),
  Quadratic = sqrt(mean(residuals(train_quadratic)^2)),
  Flexible = sqrt(mean(residuals(train_flexible)^2)))
training_RMSE
# Nested OLS models cannot increase training SSE when terms are added.
# DO NOT decide the winner from these training errors.

# Part 8: Predict the untouched observations and evaluate errors -------------
# predict() uses fitted coefficients; it does NOT fit a new model to test sales.
prediction_linear <- predict(train_linear, newdata = test)
prediction_quadratic <- predict(train_quadratic, newdata = test)
prediction_flexible <- predict(train_flexible, newdata = test)
error_quadratic <- test$sales - prediction_quadratic
test_RMSE <- c(
  Linear = sqrt(mean((test$sales - prediction_linear)^2)),
  Quadratic = sqrt(mean(error_quadratic^2)),
  Flexible = sqrt(mean((test$sales - prediction_flexible)^2)))
comparison <- data.frame(Model = names(training_RMSE),
  Training_RMSE = unname(training_RMSE), Test_RMSE = unname(test_RMSE))
print(comparison, row.names = FALSE)
best_this_split <- names(which.min(test_RMSE))
best_this_split
# INTERPRET: the winner on THIS split. Small test sets give uncertain rankings.
prediction_examples <- data.frame(
  City = rownames(test), Observed = test$sales,
  Predicted = prediction_quadratic, Error = error_quadratic)
head(prediction_examples, 5)
error_plot <- function() {
  x <- seq_along(training_RMSE)
  yrange <- range(c(training_RMSE, test_RMSE)) + c(-0.2, 0.2)
  plot(x, training_RMSE, type = 'b', pch = 16, lwd = 2, col = '#286099',
       xaxt = 'n', ylim = yrange, xlab = 'Candidate model',
       ylab = 'RMSE (thousand dollars)')
  axis(1, at = x, labels = names(training_RMSE))
  lines(x, test_RMSE, type = 'b', pch = 17, lty = 2, lwd = 2, col = '#B44832')
  legend('topright', c('Training', 'Test'), pch = c(16, 17),
         lty = c(1, 2), col = c('#286099', '#B44832'), bty = 'n')
}
if (interactive()) error_plot()

# Part 9: What changes when the split changes? -------------------------------
# This is a sensitivity demonstration using the SAME 75 cities repeatedly.
# It is NOT 200 independent new datasets or a new untouched final test set.
# Candidate formulas remain fixed. Do not search for a favourable seed.
set.seed(308107)
repeat_RMSE <- matrix(NA_real_, nrow = 200, ncol = 3,
                     dimnames = list(NULL, names(candidate_formulas)))
for (s in seq_len(nrow(repeat_RMSE))) {
  id <- sample(seq_len(nrow(andy)), size = nrow(train))
  training_split <- andy[id, ]
  heldout_split <- andy[-id, ]
  for (m in names(candidate_formulas)) {
    fit <- lm(candidate_formulas[[m]], data = training_split)
    prediction <- predict(fit, newdata = heldout_split)
    repeat_RMSE[s, m] <- sqrt(mean((heldout_split$sales - prediction)^2))
  }
}
repeat_winner <- colnames(repeat_RMSE)[max.col(-repeat_RMSE, ties.method = 'first')]
repeat_summary <- data.frame(Model = colnames(repeat_RMSE),
  Median_RMSE = apply(repeat_RMSE, 2, median),
  Wins = as.integer(table(factor(repeat_winner, levels = colnames(repeat_RMSE)))))
print(repeat_summary, row.names = FALSE)
repeat_plot <- function() {
  boxplot(as.data.frame(repeat_RMSE), outline = FALSE,
          col = c('#C2D5E8', '#91B5D5', '#D9B3A9'),
          ylab = 'Held-out RMSE (thousand dollars)', xlab = '')
  mtext('200 random splits of the same 75 cities; outliers hidden', side = 3, cex = 0.85)
}
if (interactive()) repeat_plot()
# After choosing/tuning models using held-out results, use a separate final test
# set or nested evaluation for an honest assessment of the selection procedure.
# At seminar: compare splitting proportions; explain changes without seed hunting.

# Part 10: Evidence, model selection and the decision -------------------------
# Compare criteria using the SAME training observations and outcome.
criteria <- data.frame(Model = names(train_models),
  Adjusted_R2 = vapply(train_models, function(m) summary(m)$adj.r.squared, numeric(1)),
  AIC = vapply(train_models, AIC, numeric(1)),
  BIC = vapply(train_models, BIC, numeric(1)))
print(criteria, row.names = FALSE)
# Higher adjusted R2; lower AIC/BIC are preferred by their respective criteria.
# R uses likelihood-based AIC/BIC; values differ from the book's normalisation.
# On the same sample and response, the rankings agree with the book's formulas.
# STOP AND EXPLAIN:
# 1. Which question did the joint F-test answer?
# 2. Which question did the held-out errors answer?
# 3. Why does neither establish a causal advertising effect?
# 4. What evidence would change your recommendation to Andy?

# PRESENTATION SUPPORT / CONSISTENCY CHECKS ----------------------------------
# No new modelling here. These objects keep slide numbers tied to this script.
f_test_table <- data.frame(Model = c('Restricted: price only', 'Unrestricted: quadratic'),
  SSE = c(SSE_R, SSE_U), Parameters = c(length(coef(restricted_model)), K))
coefficient_table <- data.frame(Term = names(b), Estimate = unname(b),
                               SE = sqrt(diag(V)), row.names = NULL)
fmt <- function(x, digits = 3) formatC(x, digits = digits, format = 'f')
stopifnot(SSE_R >= SSE_U, abs(F_advert - anova(restricted_model, quadratic_model)$F[2]) < 1e-8,
  abs(t_squared_term^2 - curvature_test$F[2]) < 1e-8,
  length(intersect(rownames(train), rownames(test))) == 0,
  nrow(train) + nrow(test) == nrow(andy),
  all(diff(training_RMSE) <= 1e-8), all(is.finite(repeat_RMSE)))
