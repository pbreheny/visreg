data("birthwt", package = "MASS")
birthwt$race <- factor(birthwt$race, labels = c("White", "Black", "Other"))
birthwt$smoke <- factor(birthwt$smoke, labels = c("Nonsmoker", "Smoker"))

## Basic
fit <- glm(low ~ age + race + smoke + lwt, data = birthwt, family = "binomial")
visreg(fit, "age") |> print() |> expect_silent()
visreg(fit, "lwt") |> print() |> expect_silent()
visreg(fit, "race") |> print() |> expect_silent()
visreg(fit, "race", cond = list(smoke = "Smoker")) |> print() |> expect_silent()
visreg(fit, "smoke") |> print() |> expect_silent()

v <- visreg(fit, "age", plot = FALSE)
expect_equal(round(head(v$fit$visreg_fit), 3), c(-1.498, -1.505, -1.512, -1.519, -1.526, -1.533))
expect_equal(round(head(v$fit$visreg_lwr), 3), c(-2.453, -2.446, -2.440, -2.434, -2.428, -2.423))
expect_equal(round(head(v$fit$visreg_upr), 3), c(-0.543, -0.563, -0.584, -0.603, -0.623, -0.642))

v <- visreg(fit, "race", plot = FALSE)
expect_equal(round(v$fit$visreg_fit, 3), c(-1.700, -0.468, -0.757))
expect_equal(round(v$fit$visreg_lwr, 3), c(-2.421, -1.398, -1.289))
expect_equal(round(v$fit$visreg_upr, 3), c(-0.979, 0.461, -0.225))

## Transformation of X
fit <- glm(low ~ age + I(age^2) + race + smoke + lwt, data = birthwt, family = "binomial")
visreg(fit, "age") |> print() |> expect_silent()
visreg(fit, "age", type = "contrast") |> print() |> expect_silent()

## Plot on response scale
fit <- glm(low ~ age + race + smoke + lwt, data = birthwt, family = "binomial")
visreg(fit, "age", scale = "response") |> print() |> expect_silent()
visreg(fit, "lwt", scale = "response") |> print() |> expect_silent()

v <- visreg(fit, "age", scale = "response", plot = FALSE)
expect_equal(round(head(v$fit$visreg_fit), 3), c(0.183, 0.182, 0.181, 0.180, 0.179, 0.178))

## scale="response" + type="contrast" warns (transforming a contrast is not
## guaranteed to be meaningful)
visreg(fit, "age", scale = "response", type = "contrast") |> print() |> expect_warning()

## Cond
visreg(fit, "lwt", scale = "response", cond = list(smoke = "Smoker")) |> print() |> expect_silent()
visreg(fit, "lwt", scale = "response", by = "smoke") |> print() |> expect_silent()

## Rug
fit <- glm(low ~ age + race + smoke + lwt, data = birthwt, family = "binomial")
visreg(fit, "age", scale = "response", rug = 2, jitter = TRUE) |> print() |> expect_silent()
visreg(fit, "lwt", scale = "response", rug = 2) |> print() |> expect_silent()
visreg(fit, "race", scale = "response", rug = 2) |> print() |> expect_silent()
fit <- glm(low ~ age * race + smoke + lwt, data = birthwt, family = "binomial")
visreg(fit, "age", by = "race", scale = "response", rug = 2, jitter = TRUE) |>
  print() |>
  expect_silent()
visreg(fit, "race", by = "age", scale = "response", rug = 2) |> print() |> expect_silent()
visreg(fit, "age", by = "race", scale = "response", rug = 2, jitter = TRUE, overlay = TRUE) |>
  print() |>
  expect_silent()
visreg(fit, "race", by = "age", scale = "response", rug = 2, overlay = TRUE) |>
  print() |>
  expect_silent()
