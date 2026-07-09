suppressPackageStartupMessages(library(randomForest))

# predict.randomForest() has no se.fit, so the band is always NA -- band =
# FALSE avoids the resulting geom_ribbon "missing values" warning, same
# situation as gamlss/polr

# Continuous outcome
set.seed(8)
fit <- randomForest(Ozone ~ ., data = airquality, na.action = na.omit)
visreg(fit, "Temp", band = FALSE) |> print() |> expect_silent()

v <- visreg(fit, "Temp", band = FALSE, plot = FALSE)
expect_equal(round(head(v$fit$visreg_fit), 3), c(28.991, 28.991, 29.028, 29.038, 29.011, 29.011))

# Binary outcome
mtcars$am <- factor(mtcars$am)
fit <- randomForest(am ~ ., mtcars)
visreg(fit, "wt", band = FALSE) |> print() |> expect_silent()
