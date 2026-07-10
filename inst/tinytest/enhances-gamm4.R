suppressPackageStartupMessages(library(gamm4))

# Make up some data
set.seed(4)
x <- runif(100)
id <- factor(sample.int(20, 100, replace = TRUE))
eta <- x^2 * 3 + as.numeric(id) / 20
y <- rpois(100, exp(eta))
df <- data.frame(y = y, x = x, id = id)

fit <- gamm4(y ~ s(x), family = poisson, random = ~ (1 | id), data = df)
visreg(fit$gam, "x") |> print() |> expect_silent()
visreg(fit$gam, "x", type = "contrast") |> print() |> expect_silent()

v <- visreg(fit$gam, "x", plot = FALSE)
expect_equal(round(head(v$fit$visreg_fit), 3), c(0.494, 0.514, 0.535, 0.555, 0.575, 0.595))
