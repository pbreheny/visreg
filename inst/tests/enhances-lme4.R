library(lme4)

# Make up some data
n <- 10
j <- 3
alpha <- rnorm(n)
ID <- rep(1:n, j)
x <- runif(n*j)
y <- rnorm(n*j, mean=x + alpha[ID], sd=0.5)
df <- data.frame(y=y, x=x, ID=factor(ID))

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
