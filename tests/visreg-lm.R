require(visreg)
ozone <- airquality[complete.cases(airquality),]

## Basic
fit <- lm(Ozone ~ Solar.R + Wind + Temp,data=ozone)
visreg(fit,"Wind")
visreg(fit,"Wind",type="effect")
visreg(fit,c("Solar.R","Wind","Temp"))

## Transformations of X
fit <- lm(Ozone ~ Solar.R + Wind + Temp + I(Wind^2),data=ozone)
visreg(fit,"Wind")
visreg(fit,"Wind",type="effect")
fit <- lm(Ozone ~ Solar.R + Wind + I(Temp^2) + I(Wind^2),data=ozone)
visreg(fit,"Temp")

## Transformations of y
fit <- lm(log(Ozone) ~ Solar.R + Wind + Temp,data=ozone)
visreg(fit,"Wind",trans=exp,ylab="Ozone")
fit <- lm(sqrt(Ozone) ~ Solar.R + Wind + Temp,data=ozone)
sqr = function(x){ return(x^2)}
visreg(fit,"Wind",trans=sqr,ylab="Ozone")

## Cond
visreg(fit,"Wind",cond=list('Temp'=100))
visreg(fit,"Wind",cond=list('Temp'=0,'Solar.R'=0))

## Factors
ozone$Heat <- cut(ozone$Temp,3,labels=c("Cool","Mild","Hot"))
fit <- lm(Ozone ~ Solar.R + Wind + Heat,data=ozone)
visreg(fit,"Wind")
visreg(fit,"Wind",cond=list(Heat='Mild')) ## Same as above
visreg(fit,"Wind",type="effect")
visreg(fit,"Wind",cond=list(Solar.R=250))
visreg(fit,"Wind",cond=list(Heat = 'Cool'))
visreg(fit,"Heat")
## Reorder
ozone$Heat <- factor(ozone$Heat,levels=c("Hot","Mild","Cool"))
fit <- lm(Ozone ~ Solar.R + Wind + Heat,data=ozone)
visreg(fit,"Heat")
## Whitespace option tests
visreg(fit,"Heat",whitespace=.1)
visreg(fit,"Heat",whitespace=.5)

## Numeric variables with few unique values
ozone$Hot <- ozone$Temp > 85
ozone$Hot <- ozone$Hot + (ozone$Temp > 92)
fit <- lm(Ozone ~ Solar.R + Wind*Hot,data=ozone)
visreg(fit, "Wind", by="Hot")

## Plotting options
airquality$Heat <- cut(airquality$Temp,3,labels=c("Cool","Mild","Hot"))
fit <- lm(Ozone ~ Solar.R + Wind*Heat, data=airquality)
visreg(fit,"Heat", whitespace=.1, xlab="Heat Category", line=list(col="blue", lwd=10), points=list(col="red", cex=2), alpha=.001, fill=list(col="yellow", border="green"))
visreg(fit, "Wind", line=list(col="blue", lwd=10), points=list(col="red", cex=2), alpha=.001, fill=list(col="yellow", border="green"))
visreg(fit, "Wind", by="Heat", line=list(col="blue", lwd=10), points=list(col="red", cex=2), alpha=.001, fill=list(col="yellow", border="green"))
visreg(fit, "Heat", by="Wind", line=list(col="blue", lwd=10), points=list(col="red", cex=2), alpha=.001, fill=list(col="yellow", border="green"))
