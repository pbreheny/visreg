my_data <- data.frame(
  x = runif(100),
  y = rnorm(100),
  g = letters[1:10] |> rep(10),
  w = runif(100)
)
fit <- glm(y ~ x + g, data = my_data, weights = w)
visreg(fit, "x")

fit <- glm(y ~ x + g, data = my_data)
