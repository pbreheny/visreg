if (interactive()) library(tinytest)

# Basics
airquality$Heat <- cut(airquality$Temp, 3, labels=c("Cool","Mild","Hot"))
fit <- lm(Ozone ~ Wind*Heat, data=airquality)
visreg(fit, "Wind", by="Heat")
visreg(fit, "Wind", by="Heat", layout=c(3,1))
visreg(fit, "Wind", by="Heat", layout=c(3,1), print.cond=interactive())
visreg(fit, "Wind", by="Heat", layout=c(3,1), type="contrast")
visreg(fit, "Wind", by="Heat", layout=c(3,1), type="contrast", strip.names=TRUE)
visreg(fit, "Wind", type="contrast", cond=list(Heat="Cool"), print.cond=interactive())
visreg(fit, "Wind", type="contrast", cond=list(Heat="Mild"), print.cond=interactive())
visreg(fit, "Wind", type="contrast", cond=list(Heat="Hot"), print.cond=interactive())
visreg(fit, "Wind", by="Heat", overlay=TRUE)
visreg(fit, "Wind", by="Heat", type="contrast", overlay=TRUE)
visreg(fit, "Wind", by="Heat", type="contrast", overlay=TRUE, partial=FALSE, band=FALSE)

# Print conditions
airquality$Heat <- cut(airquality$Temp, 3, labels=c("Cool","Mild","Hot"))
fit <- lm(Ozone ~ Wind*Heat, data=airquality)
expect_warning(visreg(fit, "Wind") |> capture.output() |> invisible())
expect_stdout(visreg(fit, "Wind", print.cond=TRUE), pattern='Conditions used')
expect_stdout(visreg(fit, "Wind", by = "Heat", print.cond=TRUE), pattern='Conditions used')

# Factor on x axis
visreg(fit,"Heat", by="Wind")
visreg(fit,"Heat", by="Wind", breaks=9)
visreg(fit,"Heat", by="Wind", breaks=c(0, 10, 20))
visreg(fit,"Heat", by="Wind", overlay=TRUE)
visreg(fit,"Heat", by="Wind", overlay=TRUE, breaks=c(0,10,20))
visreg(fit,"Heat", by="Wind", overlay=TRUE, partial=FALSE, band=FALSE)

# Breaks: Numeric
fit <- lm(Ozone ~ Wind*Solar.R, data=airquality)
visreg(fit,"Wind", by="Solar.R")
visreg(fit,"Wind", by="Solar.R", breaks=9)
visreg(fit,"Wind", by="Solar.R", overlay=TRUE, breaks=9)

# Breaks: categorical
fit <- lm(Ozone ~ Wind*Heat, data=airquality)
v <- visreg(fit, "Wind", "Heat", breaks=c("Hot", "Cool"))

# Breaks + transformation
fit <- lm(log(Ozone) ~ Solar.R + Wind + Temp + Wind*Temp + Wind*Solar.R, data=airquality)
visreg(fit,"Wind", by='Temp', breaks=9, trans=exp)
visreg(fit,"Wind", by='Solar.R', trans=exp)
visreg(fit,"Wind", by='Solar.R', trans=exp, overlay=TRUE)

# Numeric variables with few unique values
airquality$Hotness <- as.numeric(cut(airquality$Temp,2,labels=c("Cold","Hot")))
fit <- lm(Ozone ~ Solar.R + Wind*Hotness, data=airquality)
visreg(fit, "Wind", by="Hotness")
visreg(fit, "Wind", by="Hotness", overlay=TRUE)

# Rug
fit <- lm(Ozone ~ Wind*Heat, data=airquality)
visreg(fit, "Wind", by="Heat", rug=TRUE)
visreg(fit, "Heat", by="Wind", rug=TRUE)
visreg(fit, "Wind", by="Heat", rug=TRUE, overlay=TRUE)
visreg(fit, "Heat", by="Wind", rug=TRUE, overlay=TRUE)
