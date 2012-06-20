## Tests of effect plots
CO2$Plant <- factor(as.character(CO2$Plant))
fit <- lm(uptake~.,CO2)
visreg(fit,"Plant",type="effect")
visreg(fit,"conc",type="effect")
