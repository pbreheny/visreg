suppressPackageStartupMessages(library(betareg))
data("GasolineYield", package = "betareg")
fit <- betareg(yield ~ batch + temp, data = GasolineYield)
visreg(fit, 'temp')
visreg(fit, 'temp', type='contrast')

suppressPackageStartupMessages(library(betareg))
data("GasolineYield", package = "betareg")
fit <- betareg(yield ~ scale(gravity) + scale(temp), data = GasolineYield, link='logit')
visreg(fit, 'temp', partial=FALSE, rug=FALSE)

fit <- betareg(yield ~ gravity + temp, data = GasolineYield, link='logit')
visreg(fit, 'temp')
