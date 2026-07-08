suppressPackageStartupMessages(library(ggplot2))

airquality$Heat <- cut(airquality$Temp, 3, labels = c("Cool", "Mild", "Hot"))
airquality$Mon <- factor(month.abb[airquality$Month], levels = month.abb[5:9])
fit <- lm(Ozone ~ Wind + Heat + Solar.R + Mon, data = airquality)
p <- visreg(fit, "Wind", gg = TRUE)
print(p) |> expect_silent()
p <- visreg(fit, "Wind", gg = TRUE) + geom_smooth(col = "red", method = "loess")
print(p) |> expect_silent()
p <- visreg(fit, "Wind", gg = TRUE, line.par = list(col = "green"))
print(p) |> expect_silent()
p <- visreg(fit, "Heat", gg = TRUE)
print(p) |> expect_silent()
p <- visreg(fit, "Heat", gg = TRUE, line.par = list(col = "green"))
print(p) |> expect_silent()

fit <- lm(Ozone ~ Wind * Heat + Solar.R + Mon, data = airquality)
p <- visreg(fit, "Heat", by = "Wind", gg = TRUE)
print(p) |> expect_silent()
p <- visreg(fit, "Heat", by = "Wind", gg = TRUE, strip.names = FALSE)
print(p) |> expect_silent()
p <- visreg(fit, "Heat", by = "Wind", gg = TRUE, strip.names = LETTERS[1:3])
print(p) |> expect_silent()
p <- visreg(fit, "Heat", by = "Wind", gg = TRUE, type = "contrast")
print(p) |> expect_silent()
p <- visreg(fit, "Heat", by = "Wind", gg = TRUE, partial = FALSE)
print(p) |> expect_silent()
p <- visreg(fit, "Heat", by = "Wind", gg = TRUE, band = FALSE)
print(p) |> expect_silent()
p <- visreg(fit, "Heat", by = "Wind", gg = TRUE, partial = FALSE, band = FALSE)
print(p) |> expect_silent()

fit <- lm(Ozone ~ Wind * Heat + Solar.R + Mon, data = airquality)
p <- visreg(fit, "Wind", by = "Heat", gg = TRUE)
print(p) |> expect_silent()
p <- visreg(fit, "Wind", by = "Heat", gg = TRUE, strip.names = TRUE)
print(p) |> expect_silent()
p <- visreg(fit, "Wind", by = "Heat", gg = TRUE, strip.names = LETTERS[1:3])
print(p) |> expect_silent()
p <- visreg(fit, "Wind", by = "Heat", gg = TRUE, type = "contrast")
print(p) |> expect_silent()
p <- visreg(fit, "Wind", by = "Heat", gg = TRUE, partial = FALSE)
print(p) |> expect_silent()
p <- visreg(fit, "Wind", by = "Heat", gg = TRUE, band = FALSE)
print(p) |> expect_silent()
p <- visreg(fit, "Wind", by = "Heat", gg = TRUE, partial = FALSE, band = FALSE)
print(p) |> expect_silent()

# Overlays
p <- visreg(fit, "Wind", by = "Heat", gg = TRUE, overlay = TRUE)
print(p) |> expect_silent()
p <- visreg(fit, "Wind", by = "Heat", gg = TRUE, type = "contrast", overlay = TRUE)
print(p) |> expect_silent()
p <- visreg(
  fit,
  "Wind",
  by = "Heat",
  gg = TRUE,
  type = "contrast",
  overlay = TRUE,
  partial = FALSE,
  band = FALSE
)
print(p) |> expect_silent()
p <- visreg(fit, "Heat", by = "Wind", gg = TRUE, overlay = TRUE)
print(p) |> expect_silent()
p <- visreg(fit, "Heat", by = "Wind", gg = TRUE, overlay = TRUE, breaks = c(0, 10, 20))
print(p) |> expect_silent()
p <- visreg(fit, "Heat", by = "Wind", gg = TRUE, overlay = TRUE, partial = FALSE, band = FALSE)
print(p) |> expect_silent()
p <- visreg(fit, "Wind", by = "Solar.R", gg = TRUE, overlay = TRUE, breaks = 9)
print(p) |> expect_silent()
p <- visreg(fit, "Wind", by = "Heat", gg = TRUE, rug = 2, overlay = TRUE)
print(p) |> expect_silent()
p <- visreg(fit, "Heat", by = "Wind", gg = TRUE, rug = 2, overlay = TRUE)
print(p) |> expect_silent()

# Breaks
p <- visreg(fit, "Heat", by = "Wind", gg = TRUE, breaks = 4)
print(p) |> expect_silent()
p <- visreg(fit, "Heat", by = "Mon", gg = TRUE)
print(p) |> expect_silent()
p <- visreg(fit, "Wind", by = "Solar.R", gg = TRUE)
print(p) |> expect_silent()
p <- visreg(fit, "Wind", by = "Solar.R", gg = TRUE, breaks = 4)
print(p) |> expect_silent()

# Aesthetic options
p <- visreg(fit, "Wind", by = "Heat", gg = TRUE, xlab = "XXX", ylab = "YYY")
print(p) |> expect_silent()
p <- visreg(
  fit,
  "Wind",
  by = "Heat",
  gg = TRUE,
  line = list(col = "blue", linewidth = 5),
  points = list(col = "red", size = 3),
  fill = list(fill = "yellow", col = "green")
)
print(p) |> expect_silent()
p <- visreg(fit, "Heat", by = "Wind", gg = TRUE, xlab = "XXX", ylab = "YYY")
print(p) |> expect_silent()
p <- visreg(
  fit,
  "Heat",
  by = "Wind",
  gg = TRUE,
  line = list(col = "blue", linewidth = 5),
  points = list(col = "red", size = 3),
  fill = list(fill = "yellow", col = "green")
)
print(p) |> expect_silent()

# A transformation
fit <- lm(log(Ozone) ~ Wind * Heat + Solar.R + Mon, data = airquality)
p <- visreg(fit, "Wind", by = "Solar.R", gg = TRUE, trans = exp, overlay = TRUE)
print(p) |> expect_silent()

# Numeric variables with few unique values
airquality$Hotness <- as.numeric(cut(airquality$Temp, 2, labels = c("Cold", "Hot")))
fit <- lm(Ozone ~ Solar.R + Wind * Hotness, data = airquality)
p <- visreg(fit, "Wind", by = "Hotness", gg = TRUE)
print(p) |> expect_silent()
p <- visreg(fit, "Wind", by = "Hotness", gg = TRUE, overlay = TRUE)
print(p) |> expect_silent()
