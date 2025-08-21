library(glmmTMB)
Owls <- transform(
  Owls,
  Nest = reorder(Nest, NegPerChick),
  NCalls = SiblingNegotiation,
  FT = FoodTreatment,
  AnyCalls = NCalls > 0
)

# A regular model
fit <- glmmTMB(
  NCalls ~ FT + ArrivalTime + offset(log(BroodSize)) + (1|Nest),
  data = Owls, family = poisson)
visreg(fit, 'FT')
visreg(fit, 'FT', type='contrast')
visreg(fit, 'ArrivalTime')
visreg(fit, 'ArrivalTime', type='contrast')

# Now with zero inflation
fit <- glmmTMB(
  NCalls ~ FT + ArrivalTime + offset(log(BroodSize)) + (1|Nest),
  data = Owls, ziformula = ~SexParent, family = poisson)
visreg(fit, 'FT')
visreg(fit, 'FT', type='contrast')
visreg(fit, 'ArrivalTime')
visreg(fit, 'ArrivalTime', type='contrast')

# Autocorrelation
n <- 25
x <- MASS::mvrnorm(mu = rep(0,n), Sigma = .7 ^ as.matrix(dist(1:n)) )
y <- x + rnorm(n)
times <- factor(1:n, levels=1:n)
group <- factor(rep(1,n))
dat <- data.frame(y, times, group)
fit <- glmmTMB(y ~ ar1(times + 0 | group), data=dat)
visreg(fit, 'times')
