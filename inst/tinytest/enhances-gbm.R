suppressPackageStartupMessages(library(gbm))

# predict.gbm() has no se.fit, so the band is always NA -- band = FALSE
# avoids the resulting geom_ribbon "missing values" warning, same situation
# as gamlss/polr

# Continuous outcome
set.seed(5)
Data <- airquality[complete.cases(airquality), ]
fit <- gbm(Ozone ~ ., data = Data, distribution = 'gaussian')
visreg(fit, 'Wind', band = FALSE) |> print() |> expect_warning()
visreg(fit, 'Wind', partial = FALSE, rug = FALSE, band = FALSE) |> print() |> expect_silent()

v <- suppressWarnings(visreg(fit, 'Wind', partial = FALSE, rug = FALSE, band = FALSE, plot = FALSE))
expect_equal(round(head(v$fit$visreg_fit), 3), rep(73.112, 6))

# Supply our own residuals() function
registerS3method('residuals', 'gbm', function(fit) Data$Ozone - fit$fit)
visreg(fit, 'Wind', band = FALSE) |> print() |> expect_silent()
fit <- gbm(Ozone ~ ., data = Data, n.trees = 10000, distribution = 'gaussian')
visreg(fit, 'Wind', band = FALSE) |> print() |> expect_silent()

# Binary outcome
data("birthwt", package = "MASS")
Data <- birthwt[, 1:9]
Data$race <- factor(Data$race, labels = c("White", "Black", "Other"))
Data$smoke <- factor(Data$smoke, labels = c("Nonsmoker", "Smoker"))
fit <- gbm(
  low ~ age + lwt,
  data = Data,
  n.trees = 10000,
  distribution = 'bernoulli'
)
registerS3method('residuals', 'gbm', function(fit) Data$low)
visreg(fit, 'lwt', rug = 2, band = FALSE) |> print() |> expect_silent()

v <- visreg(fit, 'lwt', rug = 2, band = FALSE, plot = FALSE)
expect_equal(round(head(v$fit$visreg_fit), 3), rep(0.215, 6))
