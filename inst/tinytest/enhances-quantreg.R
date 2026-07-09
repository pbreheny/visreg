suppressPackageStartupMessages(library(quantreg))

# Basic usage
fit <- rq(Ozone ~ ., data = airquality)
visreg(fit, 'Wind') |> print() |> expect_silent()

v <- visreg(fit, 'Wind', plot = FALSE)
expect_equal(round(head(v$fit$visreg_fit), 3), c(64.357, 63.809, 63.262, 62.715, 62.168, 61.620))

fit <- rq(Ozone ~ ., data = airquality, tau = 0.1)
visreg(fit, 'Wind') |> print() |> expect_silent()

# Multiple quantile overlay; predict.rq() doesn't provide se.fit for the
# multi-tau matrix form, so the band is always NA -- band = FALSE avoids the
# resulting geom_ribbon "missing values" warning
fit <- rq(Ozone ~ Wind + Temp, tau = c(.25, .5, .75), data = airquality)
v <- visreg(fit, "Wind", plot = FALSE, collapse = TRUE)
plot(v, overlay = TRUE, band = FALSE) |> print() |> expect_silent()

# If you want confidence bands, fit each quantile separately instead
fit1 <- rq(Ozone ~ Wind + Temp, tau = .25, data = airquality)
fit2 <- rq(Ozone ~ Wind + Temp, tau = .5, data = airquality)
fit3 <- rq(Ozone ~ Wind + Temp, tau = .75, data = airquality)
v <- visreg_list(
  visreg(fit1, "Wind", plot = FALSE),
  visreg(fit2, "Wind", plot = FALSE),
  visreg(fit3, "Wind", plot = FALSE),
  collapse = TRUE
)
plot(v) |> print() |> expect_silent()
v <- visreg_list(
  visreg(fit1, "Wind", plot = FALSE),
  visreg(fit2, "Wind", plot = FALSE),
  visreg(fit3, "Wind", plot = FALSE),
  labels = paste("tau", c(.25, .5, .75), sep = "="),
  collapse = TRUE
)
plot(v, ylab = "Ozone") |> print() |> expect_silent()
