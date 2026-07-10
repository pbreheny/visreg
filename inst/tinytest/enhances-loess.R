fit <- loess(Ozone ~ Wind, data = airquality)
visreg(fit, "Wind") |> print() |> expect_silent()

v <- visreg(fit, "Wind", plot = FALSE)
expect_equal(
  round(head(v$fit$visreg_fit), 3),
  c(129.325, 126.034, 122.781, 119.567, 116.389, 113.246)
)
