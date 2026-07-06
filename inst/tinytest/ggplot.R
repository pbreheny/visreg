suppressPackageStartupMessages(library(ggplot2))

airquality$Heat <- cut(airquality$Temp, 3, labels = c("Cool", "Mild", "Hot"))
airquality$Mon <- factor(month.abb[airquality$Month], levels = month.abb[5:9])
fit <- lm(Ozone ~ Wind + Heat + Solar.R + Mon, data = airquality)
visreg(fit, "Wind")
visreg(fit, "Wind") + geom_smooth(col = 'red', method = 'loess')
visreg(fit, "Wind", line = list(color = 'green'))
visreg(fit, "Heat")
visreg(fit, "Heat", line = list(color = "green"))

fit <- lm(Ozone ~ Wind * Heat + Solar.R + Mon, data = airquality)
visreg(fit, "Heat", by = "Wind")
visreg(fit, "Heat", by = "Wind", strip.names = FALSE)
visreg(fit, "Heat", by = "Wind", strip.names = LETTERS[1:3])
visreg(fit, "Heat", by = "Wind", type = "contrast")
visreg(fit, "Heat", by = "Wind", partial = FALSE)
visreg(fit, "Heat", by = "Wind", band = FALSE)
visreg(fit, "Heat", by = "Wind", partial = FALSE, band = FALSE)

fit <- lm(Ozone ~ Wind * Heat + Solar.R + Mon, data = airquality)
visreg(fit, "Wind", by = "Heat")
visreg(fit, "Wind", by = "Heat", strip.names = TRUE)
visreg(fit, "Wind", by = "Heat", strip.names = LETTERS[1:3])
visreg(fit, "Wind", by = "Heat", type = "contrast")
visreg(fit, "Wind", by = "Heat", partial = FALSE)
visreg(fit, "Wind", by = "Heat", band = FALSE)
visreg(fit, "Wind", by = "Heat", partial = FALSE, band = FALSE)

# Overlays
visreg(fit, "Wind", by = "Heat", overlay = TRUE)
visreg(fit, "Wind", by = "Heat", type = "contrast", overlay = TRUE)
visreg(
  fit,
  "Wind",
  by = "Heat",
  type = "contrast",
  overlay = TRUE,
  partial = FALSE,
  band = FALSE
)
visreg(fit, "Heat", by = "Wind", overlay = TRUE)
visreg(
  fit,
  "Heat",
  by = "Wind",
  overlay = TRUE,
  breaks = c(0, 10, 20)
)
visreg(
  fit,
  "Heat",
  by = "Wind",
  overlay = TRUE,
  partial = FALSE,
  band = FALSE
)
visreg(fit, "Wind", by = "Solar.R", overlay = TRUE, breaks = 9)
visreg(fit, "Wind", by = "Heat", rug = 2, overlay = TRUE)
visreg(fit, "Heat", by = "Wind", rug = 2, overlay = TRUE)

# Breaks
visreg(fit, "Heat", by = "Wind", breaks = 4)
visreg(fit, "Heat", by = "Mon")
visreg(fit, "Wind", by = "Solar.R")
visreg(fit, "Wind", by = "Solar.R", breaks = 4)

# Aesthetic options
visreg(fit, "Wind", by = "Heat", xlab = "XXX", ylab = "YYY")
visreg(
  fit,
  "Wind",
  by = "Heat",
  line = list(color = "blue", linewidth = 5),
  points = list(color = "red", size = 3),
  fill = list(fill = "yellow", color = "green")
)
visreg(fit, "Heat", by = "Wind", xlab = "XXX", ylab = "YYY")
visreg(
  fit,
  "Heat",
  by = "Wind",
  line = list(color = "blue", linewidth = 5),
  points = list(color = "red", size = 3),
  fill = list(fill = "yellow", color = "green")
)

# A transformation
fit <- lm(log(Ozone) ~ Wind * Heat + Solar.R + Mon, data = airquality)
visreg(fit, "Wind", by = 'Solar.R', trans = exp, overlay = TRUE)

# Numeric variables with few unique values
airquality$Hotness <- as.numeric(cut(
  airquality$Temp,
  2,
  labels = c("Cold", "Hot")
))
fit <- lm(Ozone ~ Solar.R + Wind * Hotness, data = airquality)
visreg(fit, "Wind", by = "Hotness")
visreg(fit, "Wind", by = "Hotness", overlay = TRUE)
