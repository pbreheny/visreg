library(tinytest)
suppressPackageStartupMessages(library(betareg))
data("GasolineYield", package = "betareg")
fit <- betareg(yield ~ batch + temp, data = GasolineYield)
# predict.betareg() doesn't support se.fit; the contrast-type band is fine since
# its SEs come from vcov() instead.
v <- visreg(fit, "temp", plot = FALSE)
plot(v, band = FALSE) |> print() |> expect_silent()
visreg(fit, "temp", type = "contrast") |> print() |> expect_silent()

expect_equal(round(head(v$fit$visreg_fit), 3), c(-2.184, -2.157, -2.131, -2.105, -2.079, -2.053))

fit <- betareg(yield ~ gravity + temp, data = GasolineYield, link = "logit")
visreg(fit, "temp", band = FALSE) |> print() |> expect_silent()
