suppressPackageStartupMessages(library(betareg))
data("GasolineYield", package = "betareg")
fit <- betareg(yield ~ batch + temp, data = GasolineYield)
# predict.betareg() only supports type='variance', not se.fit, so the
# conditional-type band is always NA -- band = FALSE avoids the resulting
# geom_ribbon "missing values" warning; the contrast-type band is fine since
# its SEs come from vcov() instead.
# Note: band must be set via plot(v, band = FALSE) rather than
# visreg(fit, ..., band = FALSE) directly -- visreg_pred() forwards all `...`
# through to predict(), and predict.betareg() errors on unknown arguments.
v <- visreg(fit, "temp", plot = FALSE)
plot(v, band = FALSE) |> print() |> expect_silent()
visreg(fit, "temp", type = "contrast") |> print() |> expect_silent()

expect_equal(round(head(v$fit$visreg_fit), 3), c(-2.184, -2.157, -2.131, -2.105, -2.079, -2.053))

fit <- betareg(yield ~ gravity + temp, data = GasolineYield, link = "logit")
v <- visreg(fit, "temp", plot = FALSE)
plot(v, band = FALSE) |> print() |> expect_silent()
