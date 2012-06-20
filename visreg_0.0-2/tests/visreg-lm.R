ozone <- airquality[complete.cases(airquality),]
fit <- lm(Ozone ~ Solar.R + Wind + Temp,data=ozone)

visreg(fit,"Wind")
visreg(fit,"Wind",type="term")
visreg(fit,c("Solar.R","Wind","Temp"))
visreg(fit,"Wind",fill="median")

fit <- lm(Ozone ~ Solar.R + Wind + Temp + I(Wind^2),data=ozone)

visreg(fit,"Wind")
visreg(fit,"Wind",type="term")

fit <- lm(log(Ozone) ~ Solar.R + Wind + Temp,data=ozone)
visreg(fit,"Solar")
