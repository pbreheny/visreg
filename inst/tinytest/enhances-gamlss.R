suppressPackageStartupMessages(library(gamlss))
fit <- gamlss(Ozone ~ Solar.R + pb(Wind) + cs(Temp), data = na.omit(airquality), control=gamlss.control(trace=FALSE))
visreg(fit, 'Temp')
visreg2d(fit, 'Temp', 'Wind')
