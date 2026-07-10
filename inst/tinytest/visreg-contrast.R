# Tests of contrast plots

# Basic functionality
ozone <- airquality[complete.cases(airquality), ]
ozone$Heat <- cut(ozone$Temp, 3, labels = c("Cool", "Mild", "Hot"))
fit <- lm(Ozone ~ Wind * Heat, data = ozone)
visreg(fit, "Wind", type = "contrast", by = "Heat") |> print() |> expect_silent()
visreg(fit, "Wind", type = "contrast", cond = list(Heat = "Cool"), print_cond = FALSE) |>
  print() |>
  expect_silent()
visreg(fit, "Wind", type = "contrast", cond = list(Heat = "Hot"), print_cond = FALSE) |>
  print() |>
  expect_silent()

v <- visreg(fit, "Wind", type = "contrast", by = "Heat", plot = FALSE)
expect_equal(round(head(v$fit$visreg_fit[v$fit$Heat == "Cool"], 3), 3), c(8.006, 7.814, 7.621))
expect_equal(round(head(v$fit$visreg_fit[v$fit$Heat == "Mild"], 3), 3), c(29.185, 28.482, 27.779))
expect_equal(round(head(v$fit$visreg_fit[v$fit$Heat == "Hot"], 3), 3), c(38.806, 37.871, 36.936))

fit <- lm(Ozone ~ Wind + Heat, data = ozone)
visreg(fit, "Wind", type = "contrast", cond = list(Heat = "Cool")) |> print() |> expect_silent()
visreg(fit, "Wind", type = "contrast", cond = list(Heat = "Hot")) |> print() |> expect_silent()
visreg(fit, "Wind", type = "contrast", cond = list(Wind = 5)) |> print() |> expect_silent()
visreg(fit, "Wind", type = "contrast", cond = list(Wind = 20)) |> print() |> expect_silent()
visreg(fit, "Heat", type = "contrast") |> print() |> expect_silent()
visreg(fit, "Heat", type = "contrast", cond = list(Heat = "Hot")) |> print() |> expect_silent()

# An invalid level for `cond` on the contrast reference falls back to the
# reference level with a warning
visreg(fit, "Heat", type = "contrast", cond = list(Heat = "nope")) |> print() |> expect_warning()

# Real example; overparameterized model
CO2$Plant <- factor(as.character(CO2$Plant))
fit <- lm(uptake ~ ., CO2)
visreg(fit, "conc", type = "contrast") |> print() |> expect_warning()
visreg(fit, "Plant", type = "contrast") |> print() |> expect_warning()

# Toy example
x1 <- rep(1:10, 2)
x2 <- factor(rep(c("A", "B"), rep(10, 2)))
y <- x1 * rep(c(1, -1), rep(10, 2)) + rnorm(20, sd = 0.5)
fit <- lm(y ~ x1 * x2)
visreg(fit, "x1", by = "x2") |> print() |> expect_silent()
visreg(fit, "x1", cond = list(x2 = "B"), print_cond = FALSE) |> print() |> expect_silent()
visreg(fit, "x1", by = "x2", type = "contrast") |> print() |> expect_silent()
visreg(fit, "x1", cond = list(x2 = "B"), type = "contrast", print_cond = FALSE) |>
  print() |>
  expect_silent()

x2 <- relevel(x2, "B")
fit <- lm(y ~ x1 * x2)
visreg(fit, "x1", by = "x2", type = "contrast") |> print() |> expect_silent()
