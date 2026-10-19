

rm(list=ls())

library(mosaic)
library(tidyverse)

#' The data defenition file: 
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/newbroiler.def")

# Load the data:
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/newbroiler.rdata"))

names(newbroiler)



#  Model 1. The reciprocal model 

m1 <- lm(q ~ I(1/p), data = newbroiler)
summary(m1)

coef(m1)

coef(m1)[1]

coef(m1)[2] # alpha , which is positive as we initial expected. 


# The maringla effect  -----the slope 
p0 <- median(newbroiler$p)  # the median 
ME1 <- -1*coef(m1)[2]/(p0^2)
ME1

# Elasticy 
library(mosaic)
f <- makeFun(m1) # i.e, f=q
f(p0) # compute the value of q at the medain price 

elas1 <- ME1*p0/f(p0)
elas1  
  

# ==================== Model 2 ====================
m2 <- lm(q ~ log(p), data = newbroiler)
summary(m2)

coef(m2)[2]

ME2 <- coef(m2)[2]/p0  # the slope, model 2 
ME2  

f2 <- makeFun(m2)
elas2 <- coef(m2)[2]/f2(p0)  # the elasticity, model 2
elas2  

# Intepretation: slope, elasticity in words. 


#============================ model 3=================

m3 <- lm(log(q) ~ p, data = newbroiler)
summary(m3)  
coef(m3)[2]   # the coeff of p

f3 <- makeFun(m3)
ME3 <- coef(m3)[2]*f3(p0) # the marginal effect, i.e, slope 
ME3

Elas3  <- coef(m3)[2]*p0  # the elasiity 
Elas3

# Interpretation, ME3=?, Elas3 = ?


#======================Model 4 
m4 <- lm(log(q) ~ log(p), data = newbroiler)
summary(m4)  
coef(m4)[2]




