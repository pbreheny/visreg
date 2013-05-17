## Tests missing / subsetted data in various / mixed locations
require(visreg)
ozone <- airquality

fit <- lm(Ozone ~ Solar.R,data=ozone)
visreg(fit,"Solar.R")

x <- rnorm(nrow(ozone))
x[sample(1:nrow(ozone),20)] <- NA
fit <- lm(Ozone ~ Solar.R+x,data=ozone)
visreg(fit,"Solar.R")
visreg(fit,"x")

fit <- lm(Ozone ~ I(Solar.R^2)+Wind,data=ozone)
visreg(fit,"Solar.R")

fit <- lm(log(Ozone) ~ Solar.R + Wind + I(Temp^2),data=ozone)
visreg(fit,"Wind",trans=exp)

y <- rnorm(100)
x <- rnorm(100)
x[c(10,20)] <- NA
fit <- lm(y~x)
visreg(fit,"x")

## Subset
fit <- lm(Ozone ~ Wind, data=airquality)
visreg(fit, ylim=c(-50, 200))
fit <- lm(Ozone ~ Wind, data=airquality, subset=(Ozone < 150))
visreg(fit, ylim=c(-50, 200))
fit <- lm(Ozone ~ Wind, data=airquality)
visreg(fit, ylim=c(-50, 200), type="effect")
fit <- lm(Ozone ~ Wind, data=airquality, subset=(Ozone < 150))
visreg(fit, ylim=c(-50, 200), type="effect")

## A bunch of mixed types in various locations
a <- rep(LETTERS[1:4],25)
b <- rep(c(TRUE, FALSE), 50)
df <- data.frame(c=rnorm(100), d=factor(rep(1:10,10)), y=rnorm(100))
fit <- lm(y~a+b+c+d, df)
par(mfrow=c(2,2))
visreg(fit)
