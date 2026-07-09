suppressPackageStartupMessages(library(glmmTMB))
data(Owls, package = "glmmTMB")
Owls$NCalls <- Owls$SiblingNegotiation
Owls <- transform(Owls, Nest = reorder(Nest, NegPerChick), FT = FoodTreatment, AnyCalls = NCalls > 0)

# A regular model
fit <- glmmTMB(
  NCalls ~ FT + ArrivalTime + offset(log(BroodSize)) + (1 | Nest),
  data = Owls,
  family = poisson
)
visreg(fit, 'FT') |> print() |> expect_silent()
# type='contrast' on a merMod-family fit routes through lme4::nobars(), which
# emits a one-time-per-session upstream notice (moved to the reformulas
# package); suppress that specific notice rather than asserting full silence,
# since whether it fires depends on what ran earlier in the session.
visreg(fit, 'FT', type = 'contrast') |> print() |> suppressWarnings() |> expect_silent()
visreg(fit, 'ArrivalTime') |> print() |> expect_silent()
visreg(fit, 'ArrivalTime', type = 'contrast') |> print() |> suppressWarnings() |> expect_silent()

v <- visreg(fit, 'FT', plot = FALSE)
expect_equal(round(v$fit$visreg_fit, 3), c(2.133, 1.548))
v <- visreg(fit, 'ArrivalTime', plot = FALSE)
expect_equal(round(head(v$fit$visreg_fit), 3), c(2.473, 2.464, 2.454, 2.444, 2.435, 2.425))

# Now with zero inflation
fit <- glmmTMB(
  NCalls ~ FT + ArrivalTime + offset(log(BroodSize)) + (1 | Nest),
  data = Owls,
  ziformula = ~SexParent,
  family = poisson
)
visreg(fit, 'FT') |> print() |> expect_silent()
# type='contrast' on a merMod-family fit routes through lme4::nobars(), which
# emits a one-time-per-session upstream notice (moved to the reformulas
# package); suppress that specific notice rather than asserting full silence,
# since whether it fires depends on what ran earlier in the session.
visreg(fit, 'FT', type = 'contrast') |> print() |> suppressWarnings() |> expect_silent()
visreg(fit, 'ArrivalTime') |> print() |> expect_silent()
visreg(fit, 'ArrivalTime', type = 'contrast') |> print() |> suppressWarnings() |> expect_silent()

# Autocorrelation
n <- 25
x <- MASS::mvrnorm(mu = rep(0, n), Sigma = .7^as.matrix(dist(1:n)))
y <- x + rnorm(n)
times <- factor(1:n, levels = 1:n)
group <- factor(rep(1, n))
dat <- data.frame(y, times, group)
fit <- glmmTMB(y ~ ar1(times + 0 | group), data = dat)
visreg(fit, 'times') |> print() |> expect_silent()
