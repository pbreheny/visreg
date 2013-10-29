## Make up some data
n <- 10
j <- 3
alpha <- rnorm(n)
ID <- rep(1:n, j)
x <- runif(n*j)
y <- rnorm(n*j, mean=x + alpha[ID], sd=0.5)
df <- data.frame(y=y, x=x, ID=factor(ID))

## nlme
## LMM
require(nlme)
fit <- lme(y ~ x, random = ~1|ID, data=df)
visreg(fit, "x")
visreg(fit, "x", by="ID")
xyplot(y~x|ID, data=df, pch=19) ## Note: Not the same as the above!  Random effects have been eliminated in visreg
visreg(fit, "x", by="ID", level=1) ## Adds the random effects back in

## lmer
require(lme4)
fit <- lmer(y ~ x + (1|ID), data=df)
visreg(fit, "x")
visreg(fit, "x", by="ID")
visreg(fit, "x", by="ID", REform = ~1|ID) ## Adds random effects back in
