suppressPackageStartupMessages(library(gamlss))
fit <- gamlss(Ozone ~ Solar.R + pb(Wind) + cs(Temp), data = na.omit(airquality), control=gamlss.control(trace=FALSE))

# gamlss prints out annoying "new prediction" messages
visreg(fit, 'Temp') |> capture.output() |> invisible()
visreg2d(fit, 'Temp', 'Wind') |> capture.output() |> invisible()
