ozone <- airquality[complete.cases(airquality),]
fit <- lm(Ozone ~ Solar.R + Wind + Temp,data=ozone)

visreg(fit,"Wind")
visreg(fit,"Wind",type="term")
visreg(fit,c("Solar.R","Wind","Temp"))

fit <- lm(Ozone ~ Solar.R + Wind + Temp + I(Wind^2),data=ozone)

## Note: Currently causes bug
## fit <- lm(Ozone ~ Solar.R + Wind + I(Temp^2) + I(Wind^2),data=ozone)

visreg(fit,"Wind")
visreg(fit,"Wind",type="term")

fit <- lm(log(Ozone) ~ Solar.R + Wind + Temp,data=ozone)
visreg(fit,"Wind",trans=exp)

fit <- lm(sqrt(Ozone) ~ Solar.R + Wind + Temp,data=ozone)
sqr = function(x){ return(x^2)}
visreg(fit,"Wind",trans=sqr)
