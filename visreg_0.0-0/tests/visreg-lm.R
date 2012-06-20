ozone <- airquality[complete.cases(airquality),]

fit <- lm(Ozone ~ Solar.R + Wind + Temp,data=ozone)
visreg(fit,"Wind")
visreg(fit,c("Solar.R","Wind","Temp"))

fit <- lm(Ozone ~ Solar.R + Wind + Temp + I(Wind^2),data=ozone)
visreg(fit,"Wind")
