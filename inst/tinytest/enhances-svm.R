library(e1071)

# predict.svm() has no se.fit, so the band is always NA -- band = FALSE
# avoids the resulting geom_ribbon "missing values" warning, same situation
# as gamlss/polr
fit <- svm(Ozone ~ ., airquality)
visreg(fit, "Temp", band = FALSE) |> print() |> expect_silent()

v <- visreg(fit, "Temp", band = FALSE, plot = FALSE)
expect_equal(round(head(v$fit$visreg_fit), 3), c(23.354, 22.892, 22.428, 21.960, 21.492, 21.023))

fit <- svm(Species ~ ., data = iris, probability = TRUE)
visreg(
  fit,
  "Petal.Length",
  collapse = TRUE,
  partial = FALSE,
  rug = FALSE,
  overlay = TRUE,
  band = FALSE
) |>
  print() |>
  expect_silent()
