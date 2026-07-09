suppressPackageStartupMessages(library(lme4))
suppressPackageStartupMessages(library(locfit))
suppressPackageStartupMessages(library(mgcv))
suppressPackageStartupMessages(library(survival))
suppressPackageStartupMessages(library(splines))

fit <- lm(Ozone ~ Solar.R + poly(Wind, 4) + poly(Temp, 4), airquality)
visreg(fit, "Wind") |> print() |> expect_silent()
visreg(fit, "Temp") |> print() |> expect_silent()

v <- visreg(fit, "Wind", plot = FALSE)
expect_equal(round(head(v$fit$visreg_fit), 3), c(99.205, 96.486, 93.833, 91.245, 88.721, 86.260))

fit <- lm(Ozone ~ I(Solar.R / 2) + Wind + Temp, data = airquality)
visreg(fit, "Solar.R") |> print() |> expect_silent()
fit <- lm(Ozone ~ I(Solar.R^2) + Wind + Temp, data = airquality)
visreg(fit, "Solar.R") |> print() |> expect_silent()
fit <- lm(Ozone ~ I(Solar.R + 2) + Wind + Temp, data = airquality)
visreg(fit, "Solar.R") |> print() |> expect_silent()
fit <- lm(Ozone ~ I(Solar.R * 2) + Wind + Temp, data = airquality)
visreg(fit, "Solar.R") |> print() |> expect_silent()
fit <- lm(Ozone ~ I(Solar.R / Wind) + Temp, data = airquality)
visreg(fit, "Solar.R") |> print() |> expect_silent()

# lme4
data(Orthodont, package = "nlme")
Orthodont$nsex <- as.numeric(Orthodont$Sex == "Male")
fit <- lmer(
  distance ~ age +
    (age | Subject) +
    (0 + nsex | Subject) +
    (0 + nsex:age | Subject),
  data = Orthodont
) |>
  suppressMessages()
visreg(fit, "age") |> print() |> expect_silent()

data(ethanol, package = "locfit")
# Namespace-qualified: mgcv also exports an unrelated `lp()`, and since mgcv
# is loaded after locfit, an unqualified `lp()` resolves to the wrong one.
fit <- locfit(NOx ~ locfit::lp(E, nn = 0.5), data = ethanol)
visreg(fit, "E") |> print() |> expect_silent()

v <- visreg(fit, "E", plot = FALSE)
expect_equal(round(head(v$fit$visreg_fit), 3), c(0.487, 0.503, 0.521, 0.542, 0.564, 0.589))

fit <- locfit(NOx ~ locfit::lp(E, C, nn = 0.5, scale = 0), data = ethanol)
visreg(fit, "E", by = "C") |> print() |> expect_silent()

# splines
fit <- lm(Ozone ~ Solar.R + bs(Wind, 4) + ns(Temp, 4), airquality)
visreg(fit, "Wind") |> print() |> expect_silent()
visreg(fit, "Temp") |> print() |> expect_silent()

v <- visreg(fit, "Wind", plot = FALSE)
expect_equal(round(head(v$fit$visreg_fit), 3), c(99.162, 96.451, 93.800, 91.206, 88.671, 86.192))

# survival
fit <- coxph(Surv(time, status) ~ ph.ecog + pspline(age, 4), cancer)
visreg(fit, "age") |> print() |> expect_silent()
visreg(fit, "age", type = "contrast") |> print() |> expect_silent()

# mgcv
set.seed(2)
capture.output(dat <- gamSim(2, n = 200, dist = "normal", scale = 0.1)$data) |>
  invisible()
fit <- gam(y ~ s(x) + s(z), data = dat)
visreg(fit, "x") |> print() |> expect_silent()

v <- visreg(fit, "x", plot = FALSE)
expect_equal(round(head(v$fit$visreg_fit), 3), c(0.384, 0.391, 0.399, 0.406, 0.413, 0.421))

fit <- gam(y ~ s(x, z), data = dat)
visreg(fit, "x") |> print() |> expect_silent()
visreg(fit, "z") |> print() |> expect_silent()
fit <- gam(y ~ te(x, z, k = 7), data = dat, method = "REML")
visreg(fit, "x", by = "z") |> print() |> expect_silent()
fit <- gam(y ~ ti(x, z, k = 7), data = dat, method = "REML")
visreg(fit, "x", by = "z") |> print() |> expect_silent()
visreg2d(fit, "x", "z") |> expect_silent()
