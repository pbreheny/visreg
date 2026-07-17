library(tinytest)
library(lme4)

# Make up some data
set.seed(6)
n <- 10
j <- 3
alpha <- rnorm(n)
id <- rep(1:n, j)
x <- runif(n * j)
y <- rnorm(n * j, mean = x + alpha[id], sd = 0.5)
df <- data.frame(y = y, x = x, id = factor(id))

fit <- lmer(y ~ x + (1 | id), data = df)
visreg(fit, "x") |> print() |> expect_silent()
visreg(fit, "x", by = "id") |> print() |> expect_silent()
visreg(fit, "x", by = "id", predict = list(re.form = ~ (1 | id))) |> print() |> expect_silent() # Adds random effects back in

v <- visreg(fit, "x", plot = FALSE)
expect_equal(round(head(v$fit$visreg_fit), 3), c(0.307, 0.314, 0.321, 0.327, 0.334, 0.341))

visreg(fit, "x", by = "id", overlay = TRUE, strip_names = TRUE) |> print() |> expect_silent()
v <- visreg(fit, "x", by = "id", predict = list(re.form = ~ 1 | id))
plot(v, overlay = TRUE, strip_names = FALSE) |> print() |> expect_silent()
plot(v, overlay = TRUE, strip_names = TRUE) |> print() |> expect_silent()
plot(v, overlay = TRUE, strip_names = LETTERS[1:10]) |> print() |> expect_silent()

# type='contrast' on a merMod fit emits a deprecation warning with Matrix 1.8.0.
# To be safe, suppress that warning here in the first call
visreg(fit, "x", type = "contrast") |> print() |> suppressWarnings() |> expect_silent()
visreg(fit, "x", by = "id", type = "contrast") |> print() |> expect_silent()

# Works with a cbind() formula
y1 <- sample(101:200, n * j, replace = TRUE)
y2 <- sample(0:100, n * j, replace = TRUE)
df <- data.frame(y1 = y1, y2 = y2, x = x, id = factor(id))

fit <- glmer(cbind(y1, y2) ~ x + (1 | id), data = df, family = "binomial")
visreg(fit, "x") |> print() |> expect_silent()
