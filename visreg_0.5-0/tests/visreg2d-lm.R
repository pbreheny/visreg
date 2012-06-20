ozone <- airquality[complete.cases(airquality),]

fit <- lm(Ozone ~ Solar.R + Wind + Temp + I(Wind^2) + I(Temp^2) + I(Wind*Temp)+I(Wind*Temp^2) + I(Temp*Wind^2) + I(Temp^2*Wind^2),data=ozone)

visreg2d(fit,x="Wind",y="Temp")
visreg2d(fit,x="Wind",y="Temp",type="terms")
visreg2d(fit,x="Wind",y="Temp",plot.type="persp",theta=-30,ticktype="detailed",col="blue",shade=.7)
visreg2d(fit,x="Wind",y="Temp",plot.type="rgl")

visreg2d(fit,x="Wind",y="Temp",cond=list('Solar.R'=500))

## Test for the case of X2 being of length 1 
fit <- lm(Ozone ~ Solar.R + Wind + Temp,data=ozone)
visreg2d(fit,x="Wind",y="Temp",cond=list('Solar.R'=500))


fit <- lm(log(Ozone) ~ Solar.R + Wind + Temp + I(Wind^2) + I(Temp^2) + I(Wind*Temp)+I(Wind*Temp^2) + I(Temp*Wind^2) + I(Temp^2*Wind^2),data=ozone)
visreg2d(fit,x="Wind",y="Temp",trans=exp)

## No longer causes error
visreg2d(fit,x="Wind",y="Temp",trans=exp,type="terms")

## Tests for factors

ozone$Heat <- cut(ozone$Temp,3,labels=c("Cool","Mild","Hot"))	
ozone$Windy <- cut(ozone$Wind,2,labels=c("Windy","NotWindy"))

fit <- lm(Ozone ~ Solar.R + Wind + Heat,data=ozone)
visreg2d(fit,x="Wind",y="Solar.R")
visreg2d(fit,x="Wind",y="Solar.R",cond=list('Heat'='Hot'))
visreg2d(fit,x="Wind",y="Solar.R",cond=list('Heat'='Cool'))

visreg2d(fit,x="Wind",y="Heat")
visreg2d(fit,x="Heat",y="Wind")

fit <- lm(Ozone ~ Solar.R + Windy + Heat + Month,data=ozone)
visreg2d(fit,x="Heat",y="Windy")
visreg2d(fit,x="Solar.R",y="Windy")
visreg2d(fit,x="Heat",y="Solar.R")
visreg2d(fit,x="Heat",y="Windy",plot.type="rgl")## Fix axis labels
