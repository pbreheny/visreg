#library(visreg)
y <- rnorm(100)
x <- rnorm(100) |> exp()

fit <- lm(y ~ splines::ns(x))
v <- visreg(fit, 'x', xtrans=log)

head(v$fit)
tail(v$fit)

v <- visreg(fit, 'x', xtrans=log, plot=FALSE)


xx <- -5:5
x <- exp(xx)

x <- seq(0.0001, 1000, len=11)
f(x)
f <- log
fi <- approxfun(f(x), x)
seq(f(min(x)), f(max(x)), len=11) |> fi() |> f()
