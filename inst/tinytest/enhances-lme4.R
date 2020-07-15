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

# Works with a cbind() formula
y1 <- sample(101:200, n*j, replace=TRUE)
y2 <- sample(0:100, n*j, replace=TRUE)
df <- data.frame(y1=y1, y2=y2, x=x, ID=factor(ID))

fit <- glmer(cbind(y1, y2) ~ x + (1|ID), data=df, family='binomial')
visreg(fit, "x")
