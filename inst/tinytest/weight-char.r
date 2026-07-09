set.seed(3)
my_data <- data.frame(
  x = runif(100),
  y = rnorm(100),
  g = letters[1:10] |> rep(10),
  w = runif(100)
)
fit <- glm(y ~ x + g, data = my_data, weights = w)
visreg(fit, "x") |> print() |> expect_silent()

v <- visreg(fit, "x", plot = FALSE)
expect_equal(round(head(v$fit$visreg_fit), 3), c(0.278, 0.277, 0.276, 0.275, 0.274, 0.273))

fit <- glm(y ~ x + g, data = my_data)
visreg(fit, "x") |> print() |> expect_silent()
