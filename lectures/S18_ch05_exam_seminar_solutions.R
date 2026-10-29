# SOK-3081 | Worked solutions: 2022 and 2019 open exams
# Questions 1-16 match the seminar worksheet.
# Each part has its own START UP and can be run independently.
# START UP for Part 2 clears the objects created in Part 1.
# Methods follow the earlier lecture scripts and the 2022 exam solution.

# Part 1: Tuna - 2022 open exam -----------------------------------------------

# START UP -------------------------------------------------------------------

rm(list = ls())
options(scipen = 10, digits = 4)

# Install once if needed:
# install.packages(c("mosaic", "car"))
library(mosaic)
library(car)

load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/tuna.rdata"))

# Question 1 -----------------------------------------------------------------
tuna_model <- lm(sal1 ~ apr1 + apr2 + apr3 + dispad, data = tuna)
summary(tuna_model)

# Question 2 -----------------------------------------------------------------
round(coef(tuna_model)[2])
round(coef(tuna_model)[3])
round(coef(tuna_model)[5])

# The alternative one-cent interpretations are also valid:
round(coef(tuna_model)[2] * 0.01)
round(coef(tuna_model)[3] * 0.01)
# BLANK1 one dollar; BLANK2 decrease; BLANK3 25214.
# BLANK4 one dollar; BLANK5 increase; BLANK6 8020.
# BLANK7 presence; BLANK8 11925.

# Question 3 -----------------------------------------------------------------
round(summary(tuna_model)$r.squared, 3)
# A: 0.779.

# Question 4 -----------------------------------------------------------------
round(confint(tuna_model, level = 0.90), 2)
# A: (-34768.04, -15659.97) for apr1.

# Question 5 -----------------------------------------------------------------
f <- makeFun(tuna_model)

# Sales with and without dispad, keeping competitor prices at their means.
f(0.8, mean(tuna$apr2), mean(tuna$apr3), 1)
f(0.8, mean(tuna$apr2), mean(tuna$apr3), 0)

# Own-price elasticity with dispad:
round(coef(tuna_model)[2] * 0.8 /
        f(0.8, mean(tuna$apr2), mean(tuna$apr3), 1), 2)

# Own-price elasticity without dispad:
round(coef(tuna_model)[2] * 0.8 /
        f(0.8, mean(tuna$apr2), mean(tuna$apr3), 0), 2)
# BLANK9 -1.25; BLANK10 -4.79; BLANK11 16138; BLANK12 4214;
# BLANK13 inelastic; BLANK14 makes sense; BLANK15 higher; BLANK16 more.
# The exam's "more inelastic" means less elastic; both estimates are elastic.

# Question 6 -----------------------------------------------------------------
before <- f(0.8, mean(tuna$apr2), mean(tuna$apr3), 1)
after <- f(0.8, mean(tuna$apr2) - 0.1, mean(tuna$apr3) - 0.1, 1)

before
after
after - before
100 * (after - before) / before
# BLANK17 16138; BLANK18 decreases; BLANK19 1733;
# BLANK20 10.74; BLANK21 drop.
# Signed changes are -1733.09 cans and -10.74 percent.

# Question 7 -----------------------------------------------------------------
change <- deltaMethod(
  tuna_model, "-0.1*b3-0.1*b4",
  parameterNames = paste("b", 1:5, sep = "")
)
change$Estimate
change$SE
# B: 710.7. deltaMethod includes the covariance automatically.

# Question 8 -----------------------------------------------------------------
# This is the method used in the 2022 exam solution.
# deltaMethod reports a normal-approximation confidence interval.
deltaMethod(
  tuna_model, "-0.1*b3-0.1*b4",
  parameterNames = paste("b", 1:5, sep = ""),
  level = 0.95
)
# E: approximately (-3126.04, -340.15).

# For the Chapter 5 t interval, follow the manual interval calculation
# in the earlier Chapter 5 lecture script.
tc <- qt(0.975, df.residual(tuna_model))
LB <- change$Estimate - tc * change$SE
UB <- change$Estimate + tc * change$SE
round(c(LB, UB), 2)
# (-3162.84, -303.35). No original exam option matches this t interval.


# Part 2: Asparagus - 2019 open exam ------------------------------------------

# START UP -------------------------------------------------------------------

# Begin a clean part: nothing from Part 1 is needed.
rm(list = ls())
options(scipen = 10, digits = 4)

library(mosaic)
library(car)

# Read the 200 data rows; the file has an extra end-of-file marker.
asparagus <- read.table(
  "https://userpages.umbc.edu/~coates/ec611/Waugh.txt",
  header = TRUE, nrows = 200
)
names(asparagus) <- tolower(names(asparagus))

# price is the index 100*P/PM, with PM = 2.782 dollars.
# green is measured in hundredths of an inch.

# Question 9 -----------------------------------------------------------------
asparagus_model <- lm(price ~ green + nostalks + disperse, data = asparagus)
summary(asparagus_model)

# Question 10 ----------------------------------------------------------------
round(coef(asparagus_model)[2] * 200, 2)
round(coef(asparagus_model)[3] * -5, 2)
round(coef(asparagus_model)[4] * 10, 2)
# BLANK22 two inches; BLANK23 increase; BLANK24 27.52.
# BLANK25 5; BLANK26 increase; BLANK27 6.79; BLANK28 3.45.
# Price changes are index points, not percentages of the initial bunch price.
# The exam's "10 percent" increase in disperse means 10 percentage points.

# Question 11 ----------------------------------------------------------------
change <- deltaMethod(
  asparagus_model, "5*b3",
  parameterNames = paste("b", 1:4, sep = "")
)
change$Estimate
change$SE

# Use confint for the t interval and scale it by five.
round(5 * confint(asparagus_model, level = 0.95)[3, ], 2)
# A: (-8.27, -5.30).

# Question 12 ----------------------------------------------------------------
f <- makeFun(asparagus_model)

before <- f(
  green = mean(asparagus$green),
  nostalks = mean(asparagus$nostalks), disperse = 0
)
after <- f(
  green = mean(asparagus$green),
  nostalks = mean(asparagus$nostalks), disperse = 50
)

before
after
after - before
100 * (after - before) / before
# BLANK29 95.23; BLANK30 drops; BLANK31 77.97; BLANK32 17.26.
# Signed change: -17.26 index points. The drop's magnitude is 17.26.
# Relative to the initial price, the change is about -18.13 percent.

# Question 13 ----------------------------------------------------------------
change <- deltaMethod(
  asparagus_model, "50*b4",
  parameterNames = paste("b", 1:4, sep = "")
)
change$Estimate
change$SE
# Corrected answer D: 6.48.

# Question 14 ----------------------------------------------------------------
tc <- qt(0.995, df.residual(asparagus_model))
LB <- change$Estimate - tc * change$SE
UB <- change$Estimate + tc * change$SE
round(c(LB, UB), 2)
# Corrected answer C: (-34.13, -0.40).
# This is a t interval, calculated as in the earlier Chapter 5 script.

# Question 15 ----------------------------------------------------------------
f(green = 550, nostalks = 20, disperse = 14, interval = "prediction")
# A: (84.46, 53.76, 115.16).

# Compare with a confidence interval for the conditional mean:
f(green = 550, nostalks = 20, disperse = 14, interval = "confidence")

# Question 16 ----------------------------------------------------------------
# Effect in dollars:
round(coef(asparagus_model)[3] * 10 * 2.782 / 100, 2)

# Effect in cents:
round(coef(asparagus_model)[3] * 10 * 2.782)
# BLANK33 decreases; BLANK34 38.
# The price decreases by about 0.38 dollars, or 38 cents.

