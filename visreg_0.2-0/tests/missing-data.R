ozone <- airquality

fit <- lm(Ozone ~ Solar.R,data=ozone)
visreg(fit,"Solar.R")

fit <- lm(Ozone ~ I(Solar.R^2)+Wind,data=ozone)
visreg(fit,"Solar.R")

ozone <- airquality[complete.cases(airquality),]
fit <- lm(log(Ozone) ~ Solar.R + Wind + I(Temp^2),data=ozone)
visreg(fit,"Wind",trans=exp)
