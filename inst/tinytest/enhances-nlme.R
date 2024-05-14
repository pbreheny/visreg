suppressPackageStartupMessages(library(nlme))

# Make up some data
n <- 10
j <- 3
alpha <- rnorm(n)
ID <- rep(1:n, j)
x <- runif(n*j)
y <- rnorm(n*j, mean=x + alpha[ID], sd=0.5)
df <- data.frame(y=y, x=x, ID=factor(ID))

# Fit and plot
fit <- lme(y ~ x, random = ~1|ID, data=df)
visreg(fit, "x")
visreg(fit, "x", type="contrast")
visreg(fit, "x", by="ID")
lattice::xyplot(y~x|ID, data=df, pch=19) # Note: Not the same as the above!
                                         # Random effects eliminated in visreg
visreg(fit, "x", by="ID", level=1)       # Add the random effects back in

# Works with random intercept
vf1 <- varIdent(c(Female = 0.5), form = ~ 1 | Sex)
fit <- lme(distance ~ age + Sex, data = Orthodont, random = ~1, weights=vf1)
visreg(fit, 'age')
