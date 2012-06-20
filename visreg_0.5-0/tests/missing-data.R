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
