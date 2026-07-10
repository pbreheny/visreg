# Basics
airquality$Heat <- cut(airquality$Temp, 3, labels = c("Cool", "Mild", "Hot"))
airquality$Mon <- factor(month.abb[airquality$Month], levels = month.abb[5:9])
fit <- lm(Ozone ~ Wind * Heat, data = airquality)
visreg(fit, "Wind", by = "Heat") |> print() |> expect_silent()
visreg(fit, "Wind", by = "Heat", print_cond = FALSE) |> print() |> expect_silent()
visreg(fit, "Wind", by = "Heat", type = "contrast") |> print() |> expect_silent()
visreg(fit, "Wind", by = "Heat", type = "contrast", strip_names = TRUE) |>
  print() |>
  expect_silent()
visreg(fit, "Wind", by = "Heat", strip_names = FALSE) |> print() |> expect_silent()
visreg(fit, "Wind", by = "Heat", strip_names = LETTERS[1:3]) |> print() |> expect_silent()
visreg(fit, "Wind", type = "contrast", cond = list(Heat = "Cool"), print_cond = FALSE) |>
  print() |>
  expect_silent()
visreg(fit, "Wind", type = "contrast", cond = list(Heat = "Mild"), print_cond = FALSE) |>
  print() |>
  expect_silent()
visreg(fit, "Wind", type = "contrast", cond = list(Heat = "Hot"), print_cond = FALSE) |>
  print() |>
  expect_silent()
visreg(fit, "Wind", by = "Heat", overlay = TRUE) |> print() |> expect_silent()
visreg(fit, "Wind", by = "Heat", type = "contrast", overlay = TRUE) |> print() |> expect_silent()
visreg(
  fit,
  "Wind",
  by = "Heat",
  type = "contrast",
  overlay = TRUE,
  partial = FALSE,
  band = FALSE
) |>
  print() |>
  expect_silent()
visreg(fit, "Wind", by = "Heat", partial = FALSE) |> print() |> expect_silent()
visreg(fit, "Wind", by = "Heat", band = FALSE) |> print() |> expect_silent()
visreg(fit, "Wind", by = "Heat", partial = FALSE, band = FALSE) |> print() |> expect_silent()

v <- visreg(fit, "Wind", by = "Heat", plot = FALSE)
expect_equal(round(head(v$fit$visreg_fit[v$fit$Heat == "Cool"], 3), 3), c(24.791, 24.661, 24.530))
expect_equal(round(head(v$fit$visreg_fit[v$fit$Heat == "Mild"], 3), 3), c(60.327, 59.678, 59.029))
expect_equal(round(head(v$fit$visreg_fit[v$fit$Heat == "Hot"], 3), 3), c(102.206, 101.329, 100.451))

# A plot on top of the returned plot; an additive fit here, so there's no
# 'main effect in an interaction model' warning to work around
fit_add <- lm(Ozone ~ Wind + Heat + Solar.R + Mon, data = airquality)
(visreg(fit_add, "Wind") +
  ggplot2::geom_smooth(color = "red", method = "loess", formula = y ~ x)) |>
  print() |>
  expect_silent()
visreg(fit_add, "Wind", line = list(color = "green")) |> print() |> expect_silent()
visreg(fit_add, "Heat", line = list(color = "green")) |> print() |> expect_silent()

# Print conditions
expect_warning(visreg(fit, "Wind") |> print())
expect_stdout(visreg(fit, "Wind", print_cond = TRUE), pattern = "Conditions used")
expect_stdout(visreg(fit, "Wind", by = "Heat", print_cond = TRUE), pattern = "Conditions used")

# Factor on x axis
visreg(fit, "Heat", by = "Wind") |> print() |> expect_silent()
visreg(fit, "Heat", by = "Wind", breaks = 9) |> print() |> expect_silent()
visreg(fit, "Heat", by = "Wind", breaks = c(0, 10, 20)) |> print() |> expect_silent()
visreg(fit, "Heat", by = "Wind", strip_names = c("A", "B", "C")) |> print() |> expect_silent()
expect_error(visreg(fit, "Heat", by = "Wind", plot = FALSE) |> plot(strip_names = c("A", "B")))
visreg(fit, "Wind", by = "Heat", overlay = TRUE, strip_names = LETTERS[1:3]) |>
  print() |>
  expect_silent()
visreg(fit, "Heat", by = "Wind", overlay = TRUE) |> print() |> expect_silent()
visreg(fit, "Heat", by = "Wind", overlay = TRUE, breaks = c(0, 10, 20)) |>
  print() |>
  expect_silent()
visreg(fit, "Heat", by = "Wind", overlay = TRUE, partial = FALSE, band = FALSE) |>
  print() |>
  expect_silent()
visreg(fit, "Heat", by = "Wind", breaks = 4) |> print() |> expect_silent()
visreg(fit_add, "Heat", by = "Mon") |> print() |> expect_silent()

# Breaks: Numeric
fit <- lm(Ozone ~ Wind * Solar.R, data = airquality)
visreg(fit, "Wind", by = "Solar.R") |> print() |> expect_silent()
visreg(fit, "Wind", by = "Solar.R", breaks = 4) |> print() |> expect_silent()
visreg(fit, "Wind", by = "Solar.R", breaks = 9) |> print() |> expect_silent()
visreg(fit, "Wind", by = "Solar.R", overlay = TRUE, breaks = 9) |> print() |> expect_silent()

# Breaks: categorical
fit <- lm(Ozone ~ Wind * Heat, data = airquality)
visreg(fit, "Wind", "Heat", breaks = c("Hot", "Cool")) |> print() |> expect_silent()

# Numeric variables with few unique values
airquality$Hotness <- as.numeric(cut(airquality$Temp, 2, labels = c("Cold", "Hot")))
fit <- lm(Ozone ~ Solar.R + Wind * Hotness, data = airquality)
visreg(fit, "Wind", by = "Hotness") |> print() |> expect_silent()
visreg(fit, "Wind", by = "Hotness", overlay = TRUE) |> print() |> expect_silent()

# Aesthetic options
fit <- lm(Ozone ~ Wind * Heat + Solar.R + Mon, data = airquality)
visreg(fit, "Wind", by = "Heat", xlab = "XXX", ylab = "YYY") |> print() |> expect_silent()
visreg(
  fit,
  "Wind",
  by = "Heat",
  line = list(color = "blue", linewidth = 5),
  points = list(color = "red", size = 3),
  fill = list(fill = "yellow", color = "green")
) |>
  print() |>
  expect_silent()
visreg(fit, "Heat", by = "Wind", xlab = "XXX", ylab = "YYY") |> print() |> expect_silent()
visreg(
  fit,
  "Heat",
  by = "Wind",
  line = list(color = "blue", linewidth = 5),
  points = list(color = "red", size = 3),
  fill = list(fill = "yellow", color = "green")
) |>
  print() |>
  expect_silent()

# Rug
fit <- lm(Ozone ~ Wind * Heat, data = airquality)
visreg(fit, "Wind", by = "Heat", rug = TRUE) |> print() |> expect_silent()
visreg(fit, "Heat", by = "Wind", rug = TRUE) |> print() |> expect_silent()
visreg(fit, "Wind", by = "Heat", rug = TRUE, overlay = TRUE) |> print() |> expect_silent()
visreg(fit, "Heat", by = "Wind", rug = TRUE, overlay = TRUE) |> print() |> expect_silent()
visreg(fit, "Wind", by = "Heat", rug = 2, overlay = TRUE) |> print() |> expect_silent()
visreg(fit, "Heat", by = "Wind", rug = 2, overlay = TRUE) |> print() |> expect_silent()
