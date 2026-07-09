# Subsets
fit1 <- lm(Ozone ~ Wind + Temp, airquality, subset = (Month <= 7))
fit2 <- lm(Ozone ~ Wind + Temp, airquality, subset = (Month > 7))
v <- visreg_list(
  visreg(fit1, 'Wind', plot = FALSE),
  visreg(fit2, 'Wind', plot = FALSE)
)
plot(v) |> print() |> expect_silent()
v <- visreg_list(
  visreg(fit1, 'Wind', plot = FALSE),
  visreg(fit2, 'Wind', plot = FALSE),
  labels = c('May-Jun', 'Jul-Aug'),
  collapse = TRUE
)
plot(v) |> print() |> expect_silent()
plot(v, overlay = TRUE) |> print() |> expect_silent()

expect_equal(
  round(head(v$fit$visreg_fit[v$fit$visreg_collapse == "May-Jun"]), 3),
  c(56.756, 56.358, 55.960, 55.562, 55.165, 54.767)
)
expect_equal(
  round(head(v$fit$visreg_fit[v$fit$visreg_collapse == "Jul-Aug"]), 3),
  c(71.541, 70.992, 70.443, 69.894, 69.345, 68.796)
)

# Models
fit1 <- lm(Ozone ~ Wind + Solar.R, airquality)
fit2 <- lm(Ozone ~ Wind + Temp, airquality)
v <- visreg_list(
  visreg(fit1, 'Wind', plot = FALSE),
  visreg(fit2, 'Wind', plot = FALSE)
)
plot(v) |> print() |> expect_silent()
v <- visreg_list(
  visreg(fit1, 'Wind', plot = FALSE),
  visreg(fit2, 'Wind', plot = FALSE),
  labels = c('Adj for Solar', 'Adj for Temp'),
  collapse = TRUE
)
plot(v) |> print() |> expect_silent()
plot(v, overlay = TRUE) |> print() |> expect_silent()
