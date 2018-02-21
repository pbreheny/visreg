fit <- lm(Ozone ~ Solar.R + Wind + Temp + I(Wind^2) + I(Temp^2) + I(Wind*Temp)+I(Wind*Temp^2) + I(Temp*Wind^2) + I(Temp^2*Wind^2),data=airquality)

# Conditional plots
visreg2d(fit, x="Wind", y="Temp")
visreg2d(fit, x="Wind", y="Temp", plot.type="persp")
visreg2d(fit, x="Wind", y="Temp", plot.type="rgl")
visreg2d(fit, x="Wind", y="Temp", plot.type="gg")

# Contrast plots
visreg2d(fit, x="Wind", y="Temp", type="contrast")
visreg2d(fit, x="Wind", y="Temp", type="contrast", plot.type="persp", col="slateblue")
visreg2d(fit, x="Wind", y="Temp", type="contrast", plot.type="rgl")
visreg2d(fit, x="Wind", y="Temp", type="contrast", plot.type='gg')

# Cond
visreg2d(fit, x="Wind", y="Temp", cond=list('Solar.R'=100))
visreg2d(fit, x="Wind", y="Temp", cond=list('Solar.R'=500))

# Transformation
fit <- lm(log(Ozone) ~ Solar.R + Wind + Temp, data=airquality)
visreg2d(fit, "Wind", "Temp")
visreg2d(fit, "Wind", "Temp", trans=exp)
visreg2d(fit, "Wind", "Temp", trans=exp, plot.type='persp')
visreg2d(fit, "Wind", "Temp", trans=exp, plot.type='gg')

# Changing options
visreg2d(fit, "Wind", "Temp", xlab="MyAxis", zlab="MyTitle")
visreg2d(fit, "Wind", "Temp", color.palette=topo.colors)
visreg2d(fit, "Wind", "Temp", color=c('green', 'blue'))
visreg2d(fit, "Wind", "Temp", color=c('green', 'blue'), plot.type='gg')
visreg2d(fit, "Wind", "Temp", plot.type='gg', xlab="MyLabel", zlab="MyTitle")

# Factor on one axis
airquality$Heat <- cut(airquality$Temp,3,labels=c("Cool","Mild","Hot"))
fit <- lm(Ozone ~ Solar.R + Wind + Heat,data=airquality)
visreg2d(fit, x="Wind", y="Solar.R")
visreg2d(fit, x="Wind", y="Solar.R",cond=list('Heat'='Hot'))
visreg2d(fit, x="Wind", y="Solar.R",cond=list('Heat'='Cool'))
visreg2d(fit, x="Wind", y="Heat")
visreg2d(fit, x="Heat", y="Wind")
visreg2d(fit, x="Wind", y="Heat", plot.type='gg')
visreg2d(fit, x="Heat", y="Wind", plot.type='gg')

# Factor on both axes
airquality$Windy <- cut(airquality$Wind,2,labels=c("Windy","NotWindy"))
fit <- lm(Ozone ~ Solar.R + Windy + Heat + Month,data=airquality)
visreg2d(fit, "Heat", "Windy")
visreg2d(fit, "Solar.R", "Windy")
visreg2d(fit, "Heat", "Solar.R")
visreg2d(fit, "Heat", "Windy", plot.type='gg')
visreg2d(fit, "Heat", "Windy", plot.type="persp") ## Doesn't seem to be any
visreg2d(fit, "Heat", "Windy", plot.type="rgl")   ## way to add labels here
