# Tests of contrast plots
if (interactive()) library(tinytest)

# Basic functionality
ozone <- airquality[complete.cases(airquality),]
ozone$Heat <- cut(ozone$Temp,3,labels=c("Cool","Mild","Hot"))
fit <- lm(Ozone ~ Wind*Heat,data=ozone)
visreg(fit,"Wind", type="contrast", by="Heat",layout=c(3,1))
visreg(fit,"Wind", type="contrast", cond=list(Heat="Cool"), print.cond=FALSE)
visreg(fit,"Wind", type="contrast", cond=list(Heat="Hot"), print.cond=FALSE)
fit <- lm(Ozone ~ Wind + Heat,data=ozone)
visreg(fit, "Wind", type="contrast", cond=list(Heat="Cool"))
visreg(fit, "Wind", type="contrast", cond=list(Heat="Hot"))
visreg(fit, "Wind", type="contrast", cond=list(Wind=5))
visreg(fit, "Wind", type="contrast", cond=list(Wind=20))
visreg(fit, "Heat", type="contrast")
visreg(fit, "Heat", type="contrast", cond=list(Heat="Hot"))

# Real example; overparameterized model
CO2$Plant <- factor(as.character(CO2$Plant))
fit <- lm(uptake~., CO2)
expect_warning(visreg(fit, "conc", type="contrast"))
expect_warning(visreg(fit, "Plant", type="contrast"))

# Toy example
x1 <- rep(1:10,2)
x2 <- factor(rep(c("A","B"),rep(10,2)))
y <- x1*rep(c(1,-1),rep(10,2))+ rnorm(20,sd=0.5)
fit <- lm(y~x1*x2)
visreg(fit, "x1", by="x2")
visreg(fit, "x1", cond=list(x2="B"), print.cond = interactive())
visreg(fit, "x1", by="x2", type="contrast")
visreg(fit, "x1", cond=list(x2="B"), type="contrast", print.cond = interactive())

x2 <- relevel(x2, "B")
fit <- lm(y~x1*x2)
visreg(fit,"x1",by="x2",type="contrast")
