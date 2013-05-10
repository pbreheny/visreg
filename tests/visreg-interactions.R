require(visreg)
ozone <- airquality[complete.cases(airquality),]
ozone$Heat <- cut(ozone$Temp,3,labels=c("Cool","Mild","Hot"))
fit <- lm(Ozone ~ Wind*Heat,data=ozone)

visreg(fit,"Wind",by="Heat")
visreg(fit,"Wind",by="Heat",layout=c(3,1))
visreg(fit, "Wind", type="effect", cond=list(Heat="Cool"))
visreg(fit, "Wind", type="effect", cond=list(Heat="Mild"))
visreg(fit, "Wind", type="effect", cond=list(Heat="Hot"))
visreg(fit, "Wind", by="Heat", overlay=TRUE)
visreg(fit, "Wind", by="Heat", type="effect", overlay=TRUE)
visreg(fit, "Wind", by="Heat", type="effect", overlay=TRUE, partial=FALSE, band=FALSE)

# Factor on x axis
visreg(fit,"Heat", by="Wind")
visreg(fit,"Heat", by="Wind", breaks=9)
visreg(fit,"Heat", by="Wind", breaks=c(0, 10, 20))
visreg(fit,"Heat", by="Wind", overlay=TRUE)
visreg(fit,"Heat", by="Wind", overlay=TRUE, partial=FALSE, band=FALSE)

## Breaks
fit <- lm(Ozone ~ Wind*Solar.R,data=ozone)
visreg(fit,"Wind",by="Solar.R")
visreg(fit,"Wind",by="Solar.R",breaks=9)
visreg(fit,"Wind", by="Solar.R", overlay=TRUE, breaks=9)

fit <- lm(log(Ozone) ~ Solar.R + Wind + Temp + Wind*Temp + Wind*Solar.R,data=ozone)
visreg(fit,"Wind",by='Temp',breaks=9,trans=exp)
visreg(fit,"Wind",by='Solar.R',trans=exp)
visreg(fit,"Wind", by='Solar.R', trans=exp, overlay=TRUE)

## Numeric variables with few unique values
ozone$Heat <- as.numeric(cut(ozone$Temp,3,labels=c("Cool","Mild","Hot")))
fit <- lm(Ozone ~ Solar.R + Wind*Heat, data=ozone)
visreg(fit, "Wind", by="Heat")
visreg(fit, "Wind", by="Heat", overlay=TRUE)
