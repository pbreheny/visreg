suppressPackageStartupMessages(library(MASS))

# rlm
fit <- rlm(Ozone ~ ., data = airquality)
visreg(fit, "Wind") |> print() |> expect_silent()

v <- suppressWarnings(visreg(fit, "Wind", plot = FALSE))
expect_equal(round(head(v$fit$visreg_fit), 3), c(64.205, 63.714, 63.223, 62.732, 62.241, 61.750))

# polr; predict.polr(type='probs') has no se.fit, so the band is always NA --
# band = FALSE avoids the resulting geom_tile "missing values" warning, same
# situation as gamlss
fit <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
visreg(fit, "Infl", partial = FALSE, rug = FALSE, band = FALSE, collapse = TRUE) |>
  suppressMessages() |>
  print() |>
  expect_silent()
visreg(fit, "Infl", partial = FALSE, rug = FALSE, band = FALSE, collapse = TRUE, overlay = TRUE) |>
  suppressMessages() |>
  print() |>
  expect_silent()
# contrast-type SEs come from vcov(), not predict(se.fit=), so the band is
# meaningful here even though it wasn't for the conditional-type calls above
visreg(fit, "Infl", type = "contrast", partial = FALSE, rug = FALSE) |>
  suppressMessages() |>
  print() |>
  expect_silent()
