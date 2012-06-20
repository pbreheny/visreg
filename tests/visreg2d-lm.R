require(visreg)
fit <- lm(Ozone ~ Solar.R + Wind + Temp + I(Wind^2) + I(Temp^2) + I(Wind*Temp)+I(Wind*Temp^2) + I(Temp*Wind^2) + I(Temp^2*Wind^2),data=airquality)

## Conditional plots
visreg2d(fit,x="Wind",y="Temp")
visreg2d(fit,x="Wind",y="Temp",plot.type="persp",col="slateblue")
##visreg2d(fit,x="Wind",y="Temp",plot.type="rgl")

## Effect plots
visreg2d(fit,x="Wind",y="Temp",type="effect")
visreg2d(fit,x="Wind",y="Temp",plot.type="persp",theta=-30,ticktype="detailed",col="slateblue",type="effect")
##visreg2d(fit,x="Wind",y="Temp",plot.type="rgl",type="effect")

## Cond
visreg2d(fit,x="Wind",y="Temp",cond=list('Solar.R'=500))

## Simple model
fit <- lm(Ozone ~ Solar.R + Wind + Temp,data=airquality)
visreg2d(fit,x="Wind",y="Temp")
visreg2d(fit,x="Wind",y="Temp",cond=list('Solar.R'=500))

## Simple + transformation
fit <- lm(log(Ozone) ~ Solar.R + Wind + Temp,data=airquality)
visreg2d(fit,x="Wind",y="Temp",trans=exp)
visreg2d(fit,x="Wind",y="Temp",trans=exp,type="effect")

## Changing options
visreg2d(fit,x="Wind",y="Temp",xlab="MyLabel")
visreg2d(fit,x="Wind",y="Temp",color.palette=topo.colors)

## Transformation
fit <- lm(log(Ozone) ~ Solar.R + Wind + Temp + I(Wind^2) + I(Temp^2) + I(Wind*Temp)+I(Wind*Temp^2) + I(Temp*Wind^2) + I(Temp^2*Wind^2),data=airquality)
visreg2d(fit,x="Wind",y="Temp",trans=exp)
visreg2d(fit,x="Wind",y="Temp",trans=exp,plot.type="persp")

## Tests for factors
airquality$Heat <- cut(airquality$Temp,3,labels=c("Cool","Mild","Hot"))	
airquality$Windy <- cut(airquality$Wind,2,labels=c("Windy","NotWindy"))

fit <- lm(Ozone ~ Solar.R + Wind + Heat,data=airquality)
visreg2d(fit,x="Wind",y="Solar.R")
visreg2d(fit,x="Wind",y="Solar.R",cond=list('Heat'='Hot'))
visreg2d(fit,x="Wind",y="Solar.R",cond=list('Heat'='Cool'))

visreg2d(fit,x="Wind",y="Heat")
visreg2d(fit,x="Heat",y="Wind")

fit <- lm(Ozone ~ Solar.R + Windy + Heat + Month,data=airquality)
visreg2d(fit,x="Heat",y="Windy")
visreg2d(fit,x="Solar.R",y="Windy")
visreg2d(fit,x="Heat",y="Solar.R")

visreg2d(fit,x="Heat",y="Windy",plot.type="persp") ## Doesn't seem to be any
##visreg2d(fit,x="Heat",y="Windy",plot.type="rgl")   ## way to add labels here
