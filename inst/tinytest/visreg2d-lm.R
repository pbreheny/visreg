fit <- lm(
  Ozone ~ Solar.R +
    Wind +
    Temp +
    I(Wind^2) +
    I(Temp^2) +
    I(Wind * Temp) +
    I(Wind * Temp^2) +
    I(Temp * Wind^2) +
    I(Temp^2 * Wind^2),
  data = airquality
)

# Conditional plots
visreg2d(fit, x = "Wind", y = "Temp") |> expect_silent()
visreg2d(fit, x = "Wind", y = "Temp", plot.type = "persp") |> expect_silent()
visreg2d(fit, x = "Wind", y = "Temp", plot.type = "rgl") |> expect_silent()
visreg2d(fit, x = "Wind", y = "Temp", plot.type = "gg") |> print() |> expect_silent()

v <- visreg2d(fit, x = "Wind", y = "Temp", plot = FALSE)
expect_equal(dim(v$z), c(99, 99))
expect_equal(round(v$z[1, 1], 3), -88.169)
expect_equal(round(v$z[50, 50], 3), 20.095)
expect_equal(round(v$z[99, 99], 3), 155.071)

# Contrast plots
visreg2d(fit, x = "Wind", y = "Temp", type = "contrast") |> expect_silent()
# Note: 'col' here partial-matches plot.visreg2d's own 'color' formal (there's
# no separate 'col' parameter), so this exercises the same path as 'color ='
# below, not the ... passthrough to persp(); 'border' below covers that.
visreg2d(
  fit,
  x = "Wind",
  y = "Temp",
  type = "contrast",
  plot.type = "persp",
  col = "slateblue"
) |>
  expect_silent()
visreg2d(fit, x = "Wind", y = "Temp", type = "contrast", plot.type = "persp", border = "red") |>
  expect_silent()
visreg2d(fit, x = "Wind", y = "Temp", type = "contrast", plot.type = "rgl") |> expect_silent()
visreg2d(fit, x = "Wind", y = "Temp", type = "contrast", plot.type = "rgl", alpha = 0.8) |>
  expect_silent()
visreg2d(fit, x = "Wind", y = "Temp", type = "contrast", plot.type = 'gg') |>
  print() |>
  expect_silent()

vc <- visreg2d(fit, x = "Wind", y = "Temp", type = "contrast", plot = FALSE)
expect_equal(round(vc$z[1, 1], 3), -116.771)
expect_equal(round(vc$z[50, 50], 3), -8.506)

# Cond
visreg2d(fit, x = "Wind", y = "Temp", cond = list('Solar.R' = 100)) |> expect_silent()
visreg2d(fit, x = "Wind", y = "Temp", cond = list('Solar.R' = 500)) |> expect_silent()

# Transformed response
fit <- lm(log(Ozone) ~ Solar.R + Wind + Temp, data = airquality)
visreg2d(fit, "Wind", "Temp") |> expect_silent()

# Changing options
visreg2d(fit, "Wind", "Temp", xlab = "MyAxis", zlab = "MyTitle") |> expect_silent()
visreg2d(fit, "Wind", "Temp", color.palette = topo.colors) |> expect_silent()
visreg2d(fit, "Wind", "Temp", color = c('green', 'blue')) |> expect_silent()
visreg2d(fit, "Wind", "Temp", color = c('green', 'blue'), plot.type = 'gg') |>
  print() |>
  expect_silent()
visreg2d(
  fit,
  "Wind",
  "Temp",
  plot.type = 'gg',
  xlab = "MyLabel",
  zlab = "MyTitle"
) |>
  print() |>
  expect_silent()

# Factor on one axis
airquality$Heat <- cut(airquality$Temp, 3, labels = c("Cool", "Mild", "Hot"))
fit <- lm(Ozone ~ Solar.R + Wind + Heat, data = airquality)
visreg2d(fit, x = "Wind", y = "Solar.R") |> expect_silent()
visreg2d(fit, x = "Wind", y = "Solar.R", cond = list('Heat' = 'Hot')) |> expect_silent()
visreg2d(fit, x = "Wind", y = "Solar.R", cond = list('Heat' = 'Cool')) |> expect_silent()
visreg2d(fit, x = "Wind", y = "Heat") |> expect_silent()
visreg2d(fit, x = "Heat", y = "Wind") |> expect_silent()
visreg2d(fit, x = "Wind", y = "Heat", plot.type = 'gg') |> print() |> expect_silent()
visreg2d(fit, x = "Heat", y = "Wind", plot.type = 'gg') |> print() |> expect_silent()

# Factor on both axes
airquality$Windy <- cut(airquality$Wind, 2, labels = c("Windy", "NotWindy"))
fit <- lm(Ozone ~ Solar.R + Windy + Heat + Month, data = airquality)
visreg2d(fit, "Heat", "Windy") |> expect_silent()
visreg2d(fit, "Solar.R", "Windy") |> expect_silent()
visreg2d(fit, "Heat", "Solar.R") |> expect_silent()
visreg2d(fit, "Heat", "Windy", plot.type = 'gg') |> print() |> expect_silent()
visreg2d(fit, "Heat", "Windy", plot.type = "persp") |> expect_silent()
visreg2d(fit, "Heat", "Windy", plot.type = "rgl") |> expect_silent()
