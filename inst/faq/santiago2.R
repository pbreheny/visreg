## Problem?
source("~/dev/.visreg.setup.R")
data(Insurance, package="MASS")
fit <- glm(Claims ~ Holders, family = quasipoisson, data = Insurance)
visreg(fit, scale="response")

## Solution 1
visreg(fit, scale="response")
visreg(fit)

## Solution 2
visreg(fit, scale="response")
visreg(fit)

## Linear
set.seed(2)
x <- 1:10
y <- exp(x + rnorm(length(x)))
fit <- lm(log(y)~x)
visreg(fit)
visreg(fit, trans=exp)
