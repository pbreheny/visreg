data("birthwt", package = "MASS")
birthwt$race <- factor(birthwt$race, labels = c("White", "Black", "Other"))
birthwt$smoke <- factor(birthwt$smoke, labels = c("Nonsmoker", "Smoker"))

## Basic
fit <- glm(low ~ age + race + smoke + lwt, data = birthwt, family = "binomial")
visreg2d(fit, "age", "lwt") |> expect_silent()
visreg2d(fit, "age", "lwt", scale = "response") |> expect_silent()

v <- visreg2d(fit, "age", "lwt", plot = FALSE)
expect_equal(round(v$z[1, 1], 3), -0.984)
expect_equal(round(v$z[50, 50], 3), -2.397)
v <- visreg2d(fit, "age", "lwt", scale = "response", plot = FALSE)
expect_equal(round(v$z[1, 1], 3), 0.272)
expect_equal(round(v$z[50, 50], 3), 0.083)

## Transformation of X
fit <- glm(low ~ age + race + smoke + log(lwt), data = birthwt, family = "binomial")
visreg2d(fit, "age", "lwt") |> expect_silent()
visreg2d(fit, "age", "lwt", type = "contrast") |> expect_silent()
visreg2d(fit, "age", "lwt", scale = "response") |> expect_silent()

## Cond
visreg2d(fit, "age", "lwt", scale = "response", cond = list(smoke = "Smoker")) |> expect_silent()

## Factors
visreg2d(fit, "age", "race", cond = list(smoke = "Smoker"), ylab = "") |> expect_silent()
visreg2d(fit, "race", "age", cond = list(smoke = "Smoker"), xlab = "") |> expect_silent()
