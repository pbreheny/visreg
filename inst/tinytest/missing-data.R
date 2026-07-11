# Tests missing / subsetted data in various / mixed locations
ozone <- airquality
fit <- lm(Ozone ~ Solar.R, data = ozone)
visreg(fit, "Solar.R") |> print() |> expect_silent()

x <- rnorm(nrow(ozone))
x[sample(1:nrow(ozone), 20)] <- NA
fit <- lm(Ozone ~ Solar.R + x, data = ozone)
visreg(fit, "Solar.R") |> print() |> expect_silent()
visreg(fit, "x") |> print() |> expect_silent()

fit <- lm(Ozone ~ I(Solar.R^2) + Wind, data = ozone)
visreg(fit, "Solar.R") |> print() |> expect_silent()

fit <- lm(log(Ozone) ~ Solar.R + Wind + I(Temp^2), data = ozone)
visreg(fit, "Wind") |> print() |> expect_silent()

y <- rnorm(100)
x <- rnorm(100)
x[c(10, 20)] <- NA
fit <- lm(y ~ x)
visreg(fit, "x") |> print() |> expect_silent()

# Subset
fit_full <- lm(Ozone ~ Wind, data = airquality)
fit_sub <- lm(Ozone ~ Wind, data = airquality, subset = (Ozone < 150))
visreg(fit_full) |> print() |> expect_silent()
visreg(fit_sub) |> print() |> expect_silent()
visreg(fit_full, type = "contrast") |> print() |> expect_silent()
visreg(fit_sub, type = "contrast") |> print() |> expect_silent()

v_full <- visreg(fit_full, plot = FALSE)
v_sub <- visreg(fit_sub, plot = FALSE)
expect_equal(nrow(v_full$res), 116)
expect_equal(nrow(v_sub$res), 115)
expect_true(!isTRUE(all.equal(v_full$fit$visreg_fit, v_sub$fit$visreg_fit)))

# A bunch of mixed types in various locations
a <- rep(LETTERS[1:4], 25)
b <- rep(c(TRUE, FALSE), 50)
df <- data.frame(c = rnorm(100), d = factor(rep(1:10, 10)), y = rnorm(100))
fit <- lm(y ~ a + b + c + d, df)
visreg(fit) |> print() |> expect_silent()

# Data not in global scope
my_fun <- function(form) {
  dat <- data.frame(x = rnorm(100), y = rnorm(100), z = rnorm(100))
  fit <- lm(form, data = dat)
  visreg(fit) |> print() |> expect_silent()
  visreg2d(fit, "x", "y") |> expect_silent()
}
my_fun(z ~ x + y)

# Data reachable only via the fitting function's closure environment, not
# from the caller's frame -- exercises setup_frame()'s environment(fit$terms)
# fallback for locating a model's data set
make_fit <- function() {
  dat <- data.frame(x = rnorm(20), y = rnorm(20))
  lm(y ~ x, data = dat)
}
call_visreg <- function(fit) visreg(fit, "x", plot = FALSE)
v <- call_visreg(make_fit())
expect_equal(nrow(v$res), 20)

# Data unreachable from anywhere (removed after fitting): setup_frame() must
# error rather than silently misbehaving
dat <- data.frame(x = rnorm(20), y = rnorm(20))
fit_orphan <- lm(y ~ x, data = dat)
rm(dat)
expect_error(visreg(fit_orphan, "x"))

# Data in a package namespace
fit <- lm(accel ~ times, data = MASS::mcycle)
visreg(fit, "times") |> print() |> expect_silent()

# Missing factor levels
x <- factor(rep(LETTERS[1:5], rep(5, 5)))
y <- rnorm(length(x))
y[11:15] <- NA
fit <- lm(y ~ x)
visreg(fit) |> print() |> expect_silent()
visreg(fit, type = "contrast") |> print() |> expect_silent()

# data options
data("birthwt", package = "MASS")
tmp <- birthwt
fit <- lm(bwt ~ age + race, tmp)
rm(tmp)
visreg(fit, "age", data = birthwt) |> print() |> expect_silent()
visreg2d(fit, "age", "race", data = birthwt) |> expect_silent()
fit$data <- birthwt
visreg(fit, "age") |> print() |> expect_silent()
