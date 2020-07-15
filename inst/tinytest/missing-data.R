# Tests missing / subsetted data in various / mixed locations
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

# Subset
fit <- lm(Ozone ~ Wind, data=airquality)
visreg(fit, ylim=c(-50, 200))
fit <- lm(Ozone ~ Wind, data=airquality, subset=(Ozone < 150))
visreg(fit, ylim=c(-50, 200))
fit <- lm(Ozone ~ Wind, data=airquality)
visreg(fit, ylim=c(-50, 200), type="contrast")
fit <- lm(Ozone ~ Wind, data=airquality, subset=(Ozone < 150))
visreg(fit, ylim=c(-50, 200), type="contrast")

# A bunch of mixed types in various locations
a <- rep(LETTERS[1:4],25)
b <- rep(c(TRUE, FALSE), 50)
df <- data.frame(c=rnorm(100), d=factor(rep(1:10,10)), y=rnorm(100))
fit <- lm(y~a+b+c+d, df)
par(mfrow=c(2,2))
visreg(fit)

# Data not in global scope
myFun <- function(form) {
  Data <- data.frame(x=rnorm(100), y=rnorm(100), z=rnorm(100))
  fit <- lm(form, data=Data)
  print(environment())
  print(environment(fit$terms))
  visreg(fit)
  visreg2d(fit,"x","y")
}
myFun(z~x+y)

# Missing factor levels
x <- factor(rep(LETTERS[1:5],rep(5,5)))
y <- rnorm(length(x))
y[11:15] <- NA
fit <- lm(y~x)
visreg(fit)
visreg(fit, type='contrast')

# data option
data("birthwt", package="MASS")
TMP <- birthwt
fit <- lm(bwt ~ age + race, TMP)
rm(TMP)
visreg(fit, 'age', data=birthwt)
visreg2d(fit, 'age', 'race', data=birthwt)
