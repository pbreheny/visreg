library(gamlss)
fit <- gamlss(Ozone ~ Solar.R + pb(Wind) + cs(Temp), data = na.omit(airquality))
visreg(fit, 'Temp')
visreg2d(fit, 'Temp', 'Wind')
