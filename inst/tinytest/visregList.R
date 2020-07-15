# Subsets
fit1 <- lm(Ozone ~ Wind + Temp, airquality, subset=(Month <= 7))
fit2 <- lm(Ozone ~ Wind + Temp, airquality, subset=(Month > 7))
v <- visregList(visreg(fit1, 'Wind', plot=FALSE),
                visreg(fit2, 'Wind', plot=FALSE))
op <- par(mfcol=c(2,1))
plot(v)
par(op)
v <- visregList(visreg(fit1, 'Wind', plot=FALSE),
                visreg(fit2, 'Wind', plot=FALSE),
                labels=c('May-Jun', 'Jul-Aug'),
                collapse=TRUE)
plot(v)
plot(v, overlay=TRUE)

# Models
fit1 <- lm(Ozone ~ Wind + Solar.R, airquality)
fit2 <- lm(Ozone ~ Wind + Temp, airquality)
v <- visregList(visreg(fit1, 'Wind', plot=FALSE),
                visreg(fit2, 'Wind', plot=FALSE))
op <- par(mfcol=c(2,1))
plot(v)
par(op)
v <- visregList(visreg(fit1, 'Wind', plot=FALSE),
                visreg(fit2, 'Wind', plot=FALSE),
                labels=c('Adj for Solar', 'Adj for Temp'),
                collapse=TRUE)
plot(v)
plot(v, overlay=TRUE)
