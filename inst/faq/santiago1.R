## Problem
require(visreg)
utils::data(Insurance, package="MASS")
fit <- glm(Claims ~ Holders, family = quasipoisson, data = Insurance)
visreg(fit, scale="response")
max(Insurance$Claims)

## Suggested plot
visreg(fit, scale="response", ylim=c(0,1000), partial=FALSE)
with(Insurance, points(Holders, Claims, cex=0.5, pch=19, col="gray50"))

## Investigate residual
ind <- which.max(residuals(fit))
visreg(fit)
lam <- predict(fit, newdata=Insurance[ind,,drop=FALSE], type="response")
ppois(Insurance$Claims[12], lam, lower.tail=FALSE)
pnorm(Insurance$Claims[12], lam, sqrt(lam), lower.tail=FALSE)
pnorm(Insurance$Claims[12], lam, sqrt(summary(fit)$dispersion*lam), lower.tail=FALSE)

## What about regular Poisson?
fit.pois <- glm(Claims ~ Holders, family = poisson, data = Insurance)
visreg(fit.pois)
visreg(fit.pois, scale="response")
cbind(residuals(fit), residuals(fit.pois))
cbind(residuals(fit, "pearson"), residuals(fit.pois, "pearson"))
cbind(rstudent(fit), rstudent(fit.pois))

## Negbin
require(MASS)
fit.nb <- glm.nb(Claims ~ Holders, data = Insurance)
visreg(fit.nb)
visreg(fit.nb, scale="response", ylim=c(0,1000))

fit.nb <- glm.nb(Claims ~ Holders + I(Holders^2), data = Insurance)
visreg(fit.nb, scale="response")
