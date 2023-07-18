if (interactive()) library(tinytest)

# Basic
fit <- lm(Ozone ~ Solar.R + Wind + Temp, data=airquality)
par(mfrow=c(1,3))
visreg(fit)
par(mfrow=c(1,1))
visreg(fit,"Wind")
visreg(fit,"Wind",type="contrast")

# Transformations of X
fit <- lm(Ozone ~ Solar.R + Wind + Temp + I(Wind^2), data=airquality)
visreg(fit, "Wind")
visreg(fit, "Wind", type="contrast")
fit <- lm(Ozone ~ Solar.R + Wind + I(Temp^2) + I(Wind^2), data=airquality)
visreg(fit, "Temp")

# Transformations of y
fit <- lm(log(Ozone) ~ Solar.R + Wind + Temp, data=airquality)
visreg(fit,"Wind", trans=exp, ylab="Ozone")
fit <- lm(log(Ozone) ~ log(Wind), data=airquality)
visreg(fit,"Wind", xtrans=log, ylab="log(Ozone)", xlab="log(Wind)")
fit <- lm(sqrt(Ozone) ~ Solar.R + Wind + Temp, data=airquality)
visreg(fit, "Wind", trans=function(x) x^2, ylab="Ozone")

# Cond
visreg(fit, "Wind", cond=list('Temp'=100))
visreg(fit, "Wind", cond=list('Temp'=0, 'Solar.R'=0))

# Factors
airquality$Heat <- cut(airquality$Temp, 3, labels=c("Cool", "Mild", "Hot"))
fit <- lm(Ozone ~ Solar.R + Wind + Heat, data=airquality)
visreg(fit,"Wind")
visreg(fit,"Wind", cond=list(Heat='Mild')) ## Same as above
visreg(fit,"Wind", type="contrast")
visreg(fit,"Wind", cond=list(Solar.R=250))
visreg(fit,"Wind", cond=list(Heat='Cool'))
visreg(fit,"Heat")

# Reorder
airquality$Heat <- factor(airquality$Heat,levels=c("Hot", "Mild", "Cool"))
fit <- lm(Ozone ~ Solar.R + Wind + Heat,data=airquality)
visreg(fit,"Heat")

# Whitespace option tests
visreg(fit,"Heat",whitespace=.1)
visreg(fit,"Heat",whitespace=.5)

# Plotting options
airquality$Heat <- cut(airquality$Temp,3,labels=c("Cool","Mild","Hot"))
fit <- lm(Ozone ~ Solar.R + Wind*Heat, data=airquality)
visreg(fit,"Heat", whitespace=.1, xlab="Heat Category", line=list(col="blue", lwd=10), points=list(col="red", cex=2), alpha=.001, fill=list(col="yellow", border="green"), print.cond=interactive())
visreg(fit, "Wind", line=list(col="blue", lwd=10), points=list(col="red", cex=2), fill=list(col="yellow", border="green"), print.cond=interactive())
visreg(fit, "Wind", by="Heat", line=list(col="blue", lwd=10), points=list(col="red", cex=2), alpha=.001, fill=list(col="yellow", border="green"))
visreg(fit, "Heat", by="Wind", line=list(col="blue", lwd=10), points=list(col="red", cex=2), alpha=.001, fill=list(col="yellow", border="green"))
col <- c("purple", "orange", "yellow")
visreg(fit, "Wind", by="Heat", line=list(col=col, lwd=10), points=list(col=col, cex=2), fill=list(col=rgb(.1,.1,.1,.3), border="green"), overlay=TRUE)
col <- c("purple", "orange", "yellow", "red")
visreg(fit, "Heat", by="Wind", line=list(col=col, lwd=10), points=list(col=col, cex=2), fill=list(col=rgb(.1,.1,.1,.3), border="green"), overlay=TRUE)

# Axis label size
fit <- lm(Ozone ~ Solar.R + Wind + Heat, data=airquality)
visreg(fit, "Wind", cex.axis=2)
visreg(fit, "Heat", cex.axis=2)

# Specifying by and cond at the same time
fit <- lm(Ozone ~ Solar.R + Wind*Heat, data=airquality)
visreg(fit,"Heat", by="Wind", cond=list(Solar.R=0))
visreg(fit,"Heat", by="Wind", cond=list(Solar.R=500))

# Extrapolation
fit <- lm(Ozone ~ Solar.R + Wind + Temp, data=airquality)
par(mfrow=c(1,1))
visreg(fit, "Temp", xlim=c(50,150))
visreg(fit, "Temp", type="contrast", xlim=c(50,150))

# Rug
airquality$Heat <- cut(airquality$Temp,3,labels=c("Cool","Mild","Hot"))
fit <- lm(Ozone ~ Solar.R + Wind + Heat, data=airquality)
visreg(fit, "Wind", rug=TRUE, jitter=TRUE)
visreg(fit, "Heat", rug=TRUE)

