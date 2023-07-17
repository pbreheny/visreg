suppressPackageStartupMessages(library(quantreg))

# Basic usage
fit <- rq(Ozone ~ ., data=airquality)
visreg(fit, 'Wind')
fit <- rq(Ozone ~ ., data=airquality, tau=0.1)
visreg(fit, 'Wind')

# Multiple quantile overlay
fit <- rq(Ozone ~ Wind + Temp, tau=c(.25, .5, .75), data=airquality)
v <- visreg(fit, "Wind", plot=FALSE, collapse=TRUE)
plot(v, overlay=TRUE)

# If you want confidence bands
fit1 <- rq(Ozone ~ Wind + Temp, tau=.25, data=airquality)
fit2 <- rq(Ozone ~ Wind + Temp, tau=.5, data=airquality)
fit3 <- rq(Ozone ~ Wind + Temp, tau=.75, data=airquality)
v <- visregList(visreg(fit1, "Wind", plot=FALSE),
                visreg(fit2, "Wind", plot=FALSE),
                visreg(fit3, "Wind", plot=FALSE),
                collapse=TRUE)
plot(v)
v <- visregList(visreg(fit1, "Wind", plot=FALSE),
                visreg(fit2, "Wind", plot=FALSE),
                visreg(fit3, "Wind", plot=FALSE),
                labels=paste("tau", c(.25, .5, .75), sep="="),
                collapse=TRUE)
plot(v, ylab="Ozone")
