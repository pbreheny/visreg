ozone <- airquality[complete.cases(airquality),]

fit <- lm(Ozone ~ Solar.R + Wind + Temp + I(Wind^2) + I(Temp^2) + I(Wind*Temp)+I(Wind*Temp^2) + I(Temp*Wind^2) + I(Temp^2*Wind^2),data=ozone)

visreg2d(fit,x="Wind",y="Temp")
visreg2d(fit,x="Wind",y="Temp",type="persp",theta=-30,ticktype="detailed",col="blue",shade=.7)
visreg2d(fit,x="Wind",y="Temp",type="rgl")
