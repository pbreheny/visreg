ozone <- airquality[complete.cases(airquality),]
ozone$Heat <- cut(ozone$Temp,3,labels=c("Cool","Mild","Hot"))
fit <- lm(Ozone ~ Wind*Heat,data=ozone)

visreg(fit,"Wind",by="Heat")
visreg(fit,"Wind",by="Heat",layout=c(3,1))
## Does not work correctly:
visreg(fit,"Wind",type="effect",cond="Mild")

# fixed whitespace and x-axis
visreg(fit,"Heat",by="Wind")
visreg(fit,"Heat",by="Wind",breaks=9)

fit <- lm(Ozone ~ Wind*Solar.R,data=ozone)
visreg(fit,"Wind",by="Solar.R")
visreg(fit,"Wind",by="Solar.R",breaks=9)

fit <- lm(log(Ozone) ~ Solar.R + Wind + Temp + Wind*Temp + Wind*Solar.R,data=ozone)
visreg(fit,"Wind",by='Temp',breaks=9,trans=exp)
visreg(fit,"Wind",by='Solar.R',trans=exp)
