suppressPackageStartupMessages(library(gamm4))

# Make up some data
x <- runif(100)
ID <- factor(sample(1:20, 100, replace=TRUE))
eta <- x^2*3 + as.numeric(ID)/20
y <- rpois(100, exp(eta))
df <- data.frame(y=y, x=x, ID=factor(ID))

fit <- gamm4(y~s(x), family=poisson, random=~(1|ID), data=df)
visreg(fit$gam, "x")
visreg(fit$gam, "x", type="contrast")
