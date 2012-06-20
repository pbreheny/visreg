ozone <- airquality[complete.cases(airquality),]

## Basic
fit <- lm(Ozone ~ Solar.R + Wind + Temp,data=ozone)
visreg(fit,"Wind")
visreg(fit,"Wind",type="terms")
visreg(fit,c("Solar.R","Wind","Temp"))

## Transformations of X
fit <- lm(Ozone ~ Solar.R + Wind + Temp + I(Wind^2),data=ozone)
visreg(fit,"Wind")
visreg(fit,"Wind",type="terms")

## Note: Currently causes bug
## fit <- lm(Ozone ~ Solar.R + Wind + I(Temp^2) + I(Wind^2),data=ozone)
## visreg(fit,"Temp")

## Transformations of y
fit <- lm(log(Ozone) ~ Solar.R + Wind + Temp,data=ozone)
visreg(fit,"Wind",trans=exp)

fit <- lm(sqrt(Ozone) ~ Solar.R + Wind + Temp,data=ozone)
sqr = function(x){ return(x^2)}
visreg(fit,"Wind",trans=sqr)

## Cond
visreg(fit,"Wind",cond=list('Temp'=100))
## Should be same plot:
visreg(fit,"Wind",cond=list('Temp'=0,'Solar.R'=0))
visreg(fit,"Wind",fill='zero')

## Factors
ozone$Heat <- cut(ozone$Temp,3,labels=c("Cool","Mild","Hot"))
fit <- lm(Ozone ~ Solar.R + Wind + Heat,data=ozone)
visreg(fit,"Wind")
visreg(fit,"Wind",cond=list(Heat='Mild')) ## Should be same as above
visreg(fit,"Wind",type="terms")
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

## Plotting options
visreg(fit,"Heat",whitespace=.1,xlab="Heat Category")
