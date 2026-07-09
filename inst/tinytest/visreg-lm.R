# Basic
fit <- lm(Ozone ~ Solar.R + Wind + Temp, data = airquality)
visreg(fit, "Wind") |> print() |> expect_silent()
visreg(fit, "Wind", type = "contrast") |> print() |> expect_silent()

v <- visreg(fit, "Wind", plot = FALSE)
expect_equal(round(head(v$fit$visreg_fit), 3), c(70.889, 70.275, 69.662, 69.049, 68.435, 67.822))
expect_equal(round(head(v$fit$visreg_lwr), 3), c(60.410, 60.015, 59.620, 59.224, 58.827, 58.429))
expect_equal(round(head(v$fit$visreg_upr), 3), c(81.368, 80.536, 79.704, 78.873, 78.044, 77.215))

# Transformations of X
fit <- lm(Ozone ~ Solar.R + Wind + Temp + I(Wind^2), data = airquality)
visreg(fit, "Wind") |> print() |> expect_silent()
visreg(fit, "Wind", type = "contrast") |> print() |> expect_silent()
fit <- lm(Ozone ~ Solar.R + Wind + I(Temp^2) + I(Wind^2), data = airquality)
visreg(fit, "Temp") |> print() |> expect_silent()

# Transformed response; visreg automatically plots log(Wind) back on its
# natural axis, since it's a model term rather than a `trans`/`xtrans` option
fit <- lm(log(Ozone) ~ Solar.R + Wind + Temp, data = airquality)
visreg(fit, "Wind") |> print() |> expect_silent()
fit <- lm(log(Ozone) ~ log(Wind), data = airquality)
visreg(fit, "Wind") |> print() |> expect_silent()

# Cond
fit <- lm(Ozone ~ Solar.R + Wind + Temp, data = airquality)
visreg(fit, "Wind", cond = list('Temp' = 100)) |> print() |> expect_silent()
visreg(fit, "Wind", cond = list('Temp' = 0, 'Solar.R' = 0)) |> print() |> expect_silent()

# Factors
airquality$Heat <- cut(airquality$Temp, 3, labels = c("Cool", "Mild", "Hot"))
fit <- lm(Ozone ~ Solar.R + Wind + Heat, data = airquality)
visreg(fit, "Wind") |> print() |> expect_silent()
visreg(fit, "Wind", cond = list(Heat = 'Mild')) |> print() |> expect_silent() ## Same as above
visreg(fit, "Wind", type = "contrast") |> print() |> expect_silent()
visreg(fit, "Wind", cond = list(Solar.R = 250)) |> print() |> expect_silent()
visreg(fit, "Wind", cond = list(Heat = 'Cool')) |> print() |> expect_silent()
visreg(fit, "Heat") |> print() |> expect_silent()

v <- visreg(fit, "Heat", plot = FALSE)
expect_equal(round(v$fit$visreg_fit, 3), c(27.852, 36.905, 70.991))
expect_equal(round(v$fit$visreg_lwr, 3), c(19.385, 31.376, 63.510))
expect_equal(round(v$fit$visreg_upr, 3), c(36.319, 42.435, 78.473))

# Reorder
airquality$Heat <- factor(airquality$Heat, levels = c("Hot", "Mild", "Cool"))
fit <- lm(Ozone ~ Solar.R + Wind + Heat, data = airquality)
visreg(fit, "Heat") |> print() |> expect_silent()

# Band width option tests
visreg(fit, "Heat", fill = list(width = .1)) |> print() |> expect_silent()
visreg(fit, "Heat", fill = list(width = .9)) |> print() |> expect_silent()

# Plotting options
airquality$Heat <- cut(airquality$Temp, 3, labels = c("Cool", "Mild", "Hot"))
fit <- lm(Ozone ~ Solar.R + Wind * Heat, data = airquality)
visreg(
  fit,
  "Heat",
  xlab = "Heat Category",
  line = list(color = "blue", linewidth = 10),
  points = list(color = "red", size = 2),
  alpha = .001,
  fill = list(fill = "yellow", color = "green"),
  print_cond = FALSE
) |>
  print() |>
  expect_silent()
visreg(
  fit,
  "Wind",
  line = list(color = "blue", linewidth = 10),
  points = list(color = "red", size = 2),
  fill = list(fill = "yellow", color = "green"),
  print_cond = FALSE
) |>
  print() |>
  expect_silent()
visreg(
  fit,
  "Wind",
  by = "Heat",
  line = list(color = "blue", linewidth = 10),
  points = list(color = "red", size = 2),
  alpha = .001,
  fill = list(fill = "yellow", color = "green")
) |>
  print() |>
  expect_silent()
visreg(
  fit,
  "Heat",
  by = "Wind",
  line = list(color = "blue", linewidth = 10),
  points = list(color = "red", size = 2),
  alpha = .001,
  fill = list(fill = "yellow", color = "green")
) |>
  print() |>
  expect_silent()
color <- c("purple", "orange", "yellow")
(visreg(
  fit,
  "Wind",
  by = "Heat",
  line = list(linewidth = 10),
  points = list(size = 2),
  fill = list(fill = rgb(.1, .1, .1, .3)),
  overlay = TRUE
) +
  ggplot2::scale_color_manual(values = color) +
  ggplot2::scale_fill_manual(values = color)) |>
  print() |>
  expect_silent()
color <- c("purple", "orange", "yellow", "red")
(visreg(
  fit,
  "Heat",
  by = "Wind",
  line = list(linewidth = 10),
  points = list(size = 2),
  fill = list(fill = rgb(.1, .1, .1, .3)),
  overlay = TRUE
) +
  ggplot2::scale_color_manual(values = color) +
  ggplot2::scale_fill_manual(values = color)) |>
  print() |>
  expect_silent()

# Specifying by and cond at the same time
fit <- lm(Ozone ~ Solar.R + Wind * Heat, data = airquality)
visreg(fit, "Heat", by = "Wind", cond = list(Solar.R = 0)) |> print() |> expect_silent()
visreg(fit, "Heat", by = "Wind", cond = list(Solar.R = 500)) |> print() |> expect_silent()

# Rug
airquality$Heat <- cut(airquality$Temp, 3, labels = c("Cool", "Mild", "Hot"))
fit <- lm(Ozone ~ Solar.R + Wind + Heat, data = airquality)
visreg(fit, "Wind", rug = TRUE, jitter = TRUE) |> print() |> expect_silent()
visreg(fit, "Heat", rug = TRUE) |> print() |> expect_silent()
