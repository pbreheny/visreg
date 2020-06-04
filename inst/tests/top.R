DF <- data.frame(A = sample(LETTERS[1:3], 100, replace=TRUE), B = runif(100))
DF$y <- (DF$A=="A") + DF$B + rnorm(100)
fit <- lm(y ~ A + B, DF)

# Base
visreg(fit, "A", top='points', points.par=list(cex=2))
visreg(fit, "A", top='line', points.par=list(cex=2))
visreg(fit, "B", top='points', points.par=list(cex=2))
visreg(fit, "B", top='line', points.par=list(cex=2))

# Lattice
visreg(fit, "A", "B", top='points', points.par=list(cex=2))
visreg(fit, "A", "B", top='line', points.par=list(cex=2))
visreg(fit, "B", "A", top='points', points.par=list(cex=2))
visreg(fit, "B", "A", top='line', points.par=list(cex=2))

# ggplot
visreg(fit, "A", "B", top='points', gg=TRUE, points.par=list(size=2))
visreg(fit, "A", "B", top='line', gg=TRUE, points.par=list(size=2))
visreg(fit, "B", "A", top='points', gg=TRUE, points.par=list(size=2))
visreg(fit, "B", "A", top='line', gg=TRUE, points.par=list(size=2))
visreg(fit, "A", top='points', gg=TRUE, points.par=list(size=2))
visreg(fit, "A", top='line', gg=TRUE, points.par=list(size=2))
visreg(fit, "B", top='points', gg=TRUE, points.par=list(size=2))
visreg(fit, "B", top='line', gg=TRUE, points.par=list(size=2))
