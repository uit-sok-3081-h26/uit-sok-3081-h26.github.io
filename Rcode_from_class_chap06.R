

#' ## Chapter 6

rm(list=ls())


#' A sample of hamburger franchises in 75 cities from Big Andy's Burger Barn.
#' Data definition file:
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/andy.def")

#' sales = S    Monthly sales revenue ($1000s)
#' price = P    A price index in $ for all products sold in a given month.
#' advert = A   Expenditure on advertising ($1000s)

#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/andy.rdata"))
names(andy)

summary(andy)


#' unrestricted Model, all variables
fit_u <- lm(sales~price+advert+I(advert^2), data=andy) 
summary(fit_u) 


#' Restricted model, remove advertising
#' The restriction is that, H0: b3 = b4 = 0, 5% level of significance
fit_r<- lm(sales~price, data=andy) 
summary(fit_r)


#' F-test
anova(fit_r,fit_u) 

#' Store the F-value
Ftest <- anova(fit_r,fit_u) 
str(Ftest)
Ftest$F[2] # the calculated F

#' The critical F-value
qf(0.95,2,71)

pf(Ftest$F[2],2,71, lower.tail = FALSE) # p-value
#' We reject H0, and conclude that advertising contribute in 
#' explaining the variance of sales


#' Testing the significance of the model
 
#' (compare with model that only has an intercept)
onlyinterc <- lm(sales~1, data=andy) # restricted model
summary(onlyinterc)  

anova(onlyinterc,fit_u) # F-test
#' The critical F-value
qf(0.95,3,71)

summary(fit_u)
#' NOte that comparing the two models by estimating both, 
#' is the same as the F-test in the summary of the fit_u model



#' The relationship between t- and F-tests

#' when are t-tests and F-test equivalent?
#' When the F-test has 1 df!
fit_u <- lm(sales~price+advert+I(advert^2), data=andy) # fit_u model
fit_r <- lm(sales~advert+I(advert^2), data=andy) # fit_rmodel, remove price, 1 df difference

#' F-test anova(Restricted,fit_u) 
anova(fit_u, fit_r)

# save anova for later
F2 <- anova(fit_u, fit_r)

summary(fit_u)
b <- coef(fit_u) ; b # estimated coefficients
vcov(fit_u) # variance-covariance matrix
diag(vcov(fit_u)) # variance
b/sqrt(diag(vcov(fit_u))) # t-values=estimated coefficients/se(b's)

t <- b/sqrt(diag(vcov(fit_u))) # store t-values in vector
t[2] # t-value for price
t[2]^2 # t-value for price squared
F2$F # F-value equals the square of t-value

qt(0.975,71) # 5% critical t-value
qt(0.975,71)^2 # 5% critical t-value squared
qf(0.95,1,71) # is the same as 5% critical F-value with 1 df in numerator

#' It is also possible to start with the unrestricted model,
#' and then drop/remove the price variable.
#' This is the same as above.

