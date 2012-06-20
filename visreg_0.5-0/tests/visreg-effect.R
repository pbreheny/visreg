## Tests of effect plots
## Real example; overparameterized model
CO2$Plant <- factor(as.character(CO2$Plant))
fit <- lm(uptake~.,CO2)
visreg(fit,"conc",type="effect")
visreg(fit,"Plant",type="effect")

ozone <- airquality[complete.cases(airquality),]
ozone$Heat <- cut(ozone$Temp,3,labels=c("Cool","Mild","Hot"))
fit <- lm(Ozone ~ Wind*Heat,data=ozone)

visreg(fit,"Wind",by="Heat",layout=c(3,1))
visreg(fit,"Wind",type="effect",cond=list(Heat="Mild"))

## Toy example
x1 <- rep(1:10,2)
x2 <- factor(rep(c("A","B"),rep(10,2)))
y <- x1*rep(c(1,-1),rep(10,2))+ rnorm(20,sd=0.5)
fit <- lm(y~x1*x2)
visreg(fit,"x1",by="x2")
visreg(fit,"x1",cond=list(x2="B"))
visreg(fit,"x1",cond=list(x2="B"),type="effect")
visreg(fit,"x1",by="x2",type="effect")

x2 <- factor(rep(c("A","B"),rep(10,2)),levels=c("B","A"))
fit <- lm(y~x1*x2)
visreg(fit,"x1",by="x2",type="effect")

x1 <- rep(1:10,2)
x2 <- rep(c("A","B"),rep(10,2))
y <- x1*rep(c(1,-1),rep(10,2))+ rnorm(20,sd=0.5)
fit <- lm(y~x1*x2)
visreg(fit,"x1",by="x2",type="effect")

