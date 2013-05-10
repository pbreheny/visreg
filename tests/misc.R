## A bunch of odds mixed types in various locations
a <- rep(LETTERS[1:4],25)
b <- rep(c(TRUE, FALSE), 50)
df <- data.frame(c=rnorm(100), d=factor(rep(1:10,10)), y=rnorm(100))
fit <- lm(y~a+b+c+d, df)
par(mfrow=c(2,2))
visreg(fit)
