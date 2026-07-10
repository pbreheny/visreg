suppressPackageStartupMessages(library(nlme))

# Make up some data
set.seed(7)
n <- 10
j <- 3
alpha <- rnorm(n)
ID <- rep(1:n, j)
x <- runif(n * j)
y <- rnorm(n * j, mean = x + alpha[ID], sd = 0.5)
df <- data.frame(y = y, x = x, ID = factor(ID))

# predict.lme() has no se.fit, so the conditional-type band is always NA --
# band = FALSE avoids the resulting geom_ribbon "missing values" warning; the
# contrast-type band is fine since its SEs come from vcov() instead

# Fit and plot
fit <- lme(y ~ x, random = ~ 1 | ID, data = df)
visreg(fit, "x", band = FALSE) |> print() |> expect_silent()
visreg(fit, "x", type = "contrast") |> print() |> expect_silent()
visreg(fit, "x", by = "ID", band = FALSE) |> print() |> expect_silent()
lattice::xyplot(y ~ x | ID, data = df, pch = 19) |> print() |> expect_silent() # Note: Not the same as the above!
# Random effects eliminated in visreg
visreg(fit, "x", by = "ID", level = 1, band = FALSE) |> print() |> expect_silent() # Add the random effects back in

v <- visreg(fit, "x", band = FALSE, plot = FALSE)
expect_equal(round(head(v$fit$visreg_fit), 3), c(-0.087, -0.072, -0.058, -0.043, -0.028, -0.014))

# Works with random intercept
vf1 <- varIdent(c(Female = 0.5), form = ~ 1 | Sex)
fit <- lme(distance ~ age + Sex, data = Orthodont, random = ~1, weights = vf1)
visreg(fit, "age", band = FALSE) |> print() |> expect_silent()
