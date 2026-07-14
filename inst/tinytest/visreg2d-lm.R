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
visreg2d(fit, x = "Wind", y = "Temp") |> print() |> expect_silent()
visreg2d(fit, x = "Wind", y = "Temp", plot = FALSE) |> persp() |> expect_silent()
visreg2d(fit, x = "Wind", y = "Temp", plot = FALSE) |> rgl::persp3d() |> expect_silent()

v <- visreg2d(fit, x = "Wind", y = "Temp", plot = FALSE)
expect_equal(dim(v$z), c(99, 99))
expect_equal(round(v$z[1, 1], 3), -88.169)
expect_equal(round(v$z[50, 50], 3), 20.095)
expect_equal(round(v$z[99, 99], 3), 155.071)

# Contrast plots
visreg2d(fit, x = "Wind", y = "Temp", type = "contrast") |> print() |> expect_silent()
# Note: 'col' here partial-matches persp.visreg2d's own 'color' formal (there's
# no separate 'col' parameter), so this exercises the same path as 'color ='
# below, not the ... passthrough to persp(); 'border' below covers that.
visreg2d(fit, x = "Wind", y = "Temp", type = "contrast", plot = FALSE) |>
  persp(col = "slateblue") |>
  expect_silent()
visreg2d(fit, x = "Wind", y = "Temp", type = "contrast", plot = FALSE) |>
  persp(border = "red") |>
  expect_silent()
visreg2d(fit, x = "Wind", y = "Temp", type = "contrast", plot = FALSE) |>
  rgl::persp3d() |>
  expect_silent()
visreg2d(fit, x = "Wind", y = "Temp", type = "contrast", plot = FALSE) |>
  rgl::persp3d(alpha = 0.8) |>
  expect_silent()
visreg2d(fit, x = "Wind", y = "Temp", type = "contrast") |>
  print() |>
  expect_silent()

vc <- visreg2d(fit, x = "Wind", y = "Temp", type = "contrast", plot = FALSE)
expect_equal(round(vc$z[1, 1], 3), -116.771)
expect_equal(round(vc$z[50, 50], 3), -8.506)

# Cond
visreg2d(fit, x = "Wind", y = "Temp", cond = list("Solar.R" = 100)) |> print() |> expect_silent()
visreg2d(fit, x = "Wind", y = "Temp", cond = list("Solar.R" = 500)) |> print() |> expect_silent()

# Transformed response
fit <- lm(log(Ozone) ~ Solar.R + Wind + Temp, data = airquality)
visreg2d(fit, "Wind", "Temp") |> print() |> expect_silent()

# Changing options
visreg2d(fit, "Wind", "Temp", xlab = "MyAxis", zlab = "MyTitle") |> print() |> expect_silent()
visreg2d(fit, "Wind", "Temp", color = c("green", "blue")) |> print() |> expect_silent()
visreg2d(fit, "Wind", "Temp", xlab = "MyLabel", zlab = "MyTitle") |>
  print() |>
  expect_silent()

# Factor on one axis
airquality$Heat <- cut(airquality$Temp, 3, labels = c("Cool", "Mild", "Hot"))
fit <- lm(Ozone ~ Solar.R + Wind + Heat, data = airquality)
visreg2d(fit, x = "Wind", y = "Solar.R") |> print() |> expect_silent()
visreg2d(fit, x = "Wind", y = "Solar.R", cond = list("Heat" = "Hot")) |> print() |> expect_silent()
visreg2d(fit, x = "Wind", y = "Solar.R", cond = list("Heat" = "Cool")) |> print() |> expect_silent()
visreg2d(fit, x = "Wind", y = "Heat") |> print() |> expect_silent()
visreg2d(fit, x = "Heat", y = "Wind") |> print() |> expect_silent()

# Factor on both axes
airquality$Windy <- cut(airquality$Wind, 2, labels = c("Windy", "NotWindy"))
fit <- lm(Ozone ~ Solar.R + Windy + Heat + Month, data = airquality)
visreg2d(fit, "Heat", "Windy") |> print() |> expect_silent()
visreg2d(fit, "Solar.R", "Windy") |> print() |> expect_silent()
visreg2d(fit, "Heat", "Solar.R") |> print() |> expect_silent()
visreg2d(fit, "Heat", "Windy", plot = FALSE) |> persp() |> expect_silent()
visreg2d(fit, "Heat", "Windy", plot = FALSE) |> rgl::persp3d() |> expect_silent()
