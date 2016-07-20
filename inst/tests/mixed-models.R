# Make up some data
n <- 10
j <- 3
alpha <- rnorm(n)
ID <- rep(1:n, j)
x <- runif(n*j)
y <- rnorm(n*j, mean=x + alpha[ID], sd=0.5)
df <- data.frame(y=y, x=x, ID=factor(ID))

if (require(nlme)) {
  fit <- lme(y ~ x, random = ~1|ID, data=df)
  visreg(fit, "x")
  visreg(fit, "x", type="contrast")
  visreg(fit, "x", by="ID")
  if (require(lattice)) {
    xyplot(y~x|ID, data=df, pch=19) ## Note: Not the same as the above!  Random effects have been eliminated in visreg
  }
  visreg(fit, "x", by="ID", level=1) ## Adds the random effects back in
  detach(package:nlme)
}

if (require(lme4)) {
  fit <- lmer(y ~ x + (1|ID), data=df)
  visreg(fit, "x")
  visreg(fit, "x", by="ID")
  visreg(fit, "x", by="ID", re.form = ~1|ID) ## Adds random effects back in

  visreg(fit, "x", by="ID", overlay=TRUE, strip.names=TRUE)
  v <- visreg(fit, "x", by="ID", re.form = ~1|ID)
  plot(v, overlay=TRUE, strip.names=FALSE)
  plot(v, overlay=TRUE, strip.names=TRUE)
  plot(v, overlay=TRUE, strip.names=LETTERS[1:10])

  visreg(fit, "x", type="contrast")
  visreg(fit, "x", by="ID", type="contrast")
  detach(package:lme4)
}

if (require(gamm4)) {
  x <- runif(100)
  fac <- factor(sample(1:20,100,replace=TRUE))
  eta <- x^2*3 + as.numeric(fac)/20
  y <- rpois(100,exp(eta))
  fit <- gamm4(y~s(x),family=poisson,random=~(1|fac))
  visreg(fit$gam, "x")
  visreg(fit$gam, "x", type="contrast")
  detach(package:gamm4)
}
