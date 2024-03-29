suppressPackageStartupMessages(library(ggplot2))

airquality$Heat <- cut(airquality$Temp, 3, labels=c("Cool","Mild","Hot"))
airquality$Mon <- factor(month.abb[airquality$Month], levels=month.abb[5:9])
fit <- lm(Ozone ~ Wind + Heat + Solar.R + Mon, data=airquality)
visreg(fit, "Wind", gg=TRUE)
visreg(fit, "Wind", gg=TRUE) + geom_smooth(col='red', method='loess')
visreg(fit, "Wind", gg=TRUE, line.par=list(col='green'))
visreg(fit, "Heat", gg=TRUE)
visreg(fit, "Heat", gg=TRUE, line.par=list(col="green"))

fit <- lm(Ozone ~ Wind * Heat + Solar.R + Mon, data=airquality)
visreg(fit, "Heat", by="Wind", gg=TRUE)
visreg(fit, "Heat", by="Wind", gg=TRUE, strip.names=FALSE)
visreg(fit, "Heat", by="Wind", gg=TRUE, strip.names=LETTERS[1:3])
visreg(fit, "Heat", by="Wind", gg=TRUE, type="contrast")
visreg(fit, "Heat", by="Wind", gg=TRUE, partial=FALSE)
visreg(fit, "Heat", by="Wind", gg=TRUE, band=FALSE)
visreg(fit, "Heat", by="Wind", gg=TRUE, partial=FALSE, band=FALSE)

fit <- lm(Ozone ~ Wind * Heat + Solar.R + Mon, data=airquality)
visreg(fit, "Wind", by="Heat", gg=TRUE)
visreg(fit, "Wind", by="Heat", gg=TRUE, strip.names=TRUE)
visreg(fit, "Wind", by="Heat", gg=TRUE, strip.names=LETTERS[1:3])
visreg(fit, "Wind", by="Heat", gg=TRUE, type="contrast")
visreg(fit, "Wind", by="Heat", gg=TRUE, partial=FALSE)
visreg(fit, "Wind", by="Heat", gg=TRUE, band=FALSE)
visreg(fit, "Wind", by="Heat", gg=TRUE, partial=FALSE, band=FALSE)

# Overlays
visreg(fit, "Wind", by="Heat", gg=TRUE, overlay=TRUE)
visreg(fit, "Wind", by="Heat", gg=TRUE, type="contrast", overlay=TRUE)
visreg(fit, "Wind", by="Heat", gg=TRUE, type="contrast", overlay=TRUE, partial=FALSE, band=FALSE)
visreg(fit, "Heat", by="Wind", gg=TRUE, overlay=TRUE)
visreg(fit, "Heat", by="Wind", gg=TRUE, overlay=TRUE, breaks=c(0,10,20))
visreg(fit, "Heat", by="Wind", gg=TRUE, overlay=TRUE, partial=FALSE, band=FALSE)
visreg(fit, "Wind", by="Solar.R", gg=TRUE, overlay=TRUE, breaks=9)
visreg(fit, "Wind", by="Heat", gg=TRUE, rug=2, overlay=TRUE)
visreg(fit, "Heat", by="Wind", gg=TRUE, rug=2, overlay=TRUE)

# Breaks
visreg(fit, "Heat", by="Wind", gg=TRUE, breaks=4)
visreg(fit, "Heat", by="Mon", gg=TRUE)
visreg(fit, "Wind", by="Solar.R", gg=TRUE)
visreg(fit, "Wind", by="Solar.R", gg=TRUE, breaks=4)

# Aesthetic options
visreg(fit, "Wind", by="Heat", gg=TRUE, xlab="XXX", ylab="YYY")
visreg(fit, "Wind", by="Heat", gg=TRUE, line=list(col="blue", size=5), points=list(col="red", size=3), fill=list(fill="yellow", col="green"))
visreg(fit, "Heat", by="Wind", gg=TRUE, xlab="XXX", ylab="YYY")
visreg(fit, "Heat", by="Wind", gg=TRUE, line=list(col="blue", size=5), points=list(col="red", size=3), fill=list(fill="yellow", col="green"))

# A transformation
fit <- lm(log(Ozone) ~ Wind * Heat + Solar.R + Mon, data=airquality)
visreg(fit,"Wind", by='Solar.R', gg=TRUE, trans=exp, overlay=TRUE)

# Numeric variables with few unique values
airquality$Hotness <- as.numeric(cut(airquality$Temp,2,labels=c("Cold","Hot")))
fit <- lm(Ozone ~ Solar.R + Wind*Hotness, data=airquality)
visreg(fit, "Wind", by="Hotness", gg=TRUE)
visreg(fit, "Wind", by="Hotness", gg=TRUE, overlay=TRUE)
