suppressPackageStartupMessages(library(nnet))

airquality$Heat <- cut(airquality$Temp, 3, labels = c("Cool", "Mild", "Hot"))
capture.output(fit <- multinom(Heat ~ Wind + Ozone, airquality)) |> invisible()
# predict.multinom() has no se.fit, so the band is always NA -- band = FALSE
# avoids the resulting geom_ribbon "missing values" warning, same situation
# as gamlss/polr
visreg(
  fit,
  "Ozone",
  collapse = TRUE,
  overlay = TRUE,
  ylab = "Probability",
  partial = FALSE,
  rug = 2,
  band = FALSE
) |>
  print() |>
  expect_silent()
v <- visreg(fit, "Ozone", plot = FALSE)
plot(v[[2]], ylab = "Probability", partial = FALSE, rug = 2, band = FALSE) |>
  print() |>
  expect_silent()
