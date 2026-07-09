suppressPackageStartupMessages(library(gamlss))
fit <- gamlss(
  Ozone ~ Solar.R + pb(Wind) + cs(Temp),
  data = na.omit(airquality),
  control = gamlss.control(trace = FALSE)
)

# gamlss prints out annoying "new prediction" messages, hence capture.output;
# predict.gamlss() doesn't support se.fit for new data, so visreg falls back
# to NA confidence bounds -- band = FALSE avoids the resulting (correct)
# geom_ribbon "missing values" warning about an all-NA band
capture.output(visreg(fit, 'Temp', band = FALSE) |> print()) |> invisible() |> expect_silent()
capture.output(visreg2d(fit, 'Temp', 'Wind')) |> invisible() |> expect_silent()

v <- suppressWarnings(visreg(fit, 'Temp', plot = FALSE))
expect_true(all(is.na(v$fit$visreg_lwr)))
expect_false(any(is.na(v$fit$visreg_fit)))
