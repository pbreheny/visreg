suppressPackageStartupMessages(library(randomForest))

# Continuous outcome
fit <- randomForest(Ozone ~ ., data=airquality, na.action=na.omit)
visreg(fit, "Temp")

# Binary outcome
mtcars$am <- factor(mtcars$am)
fit <- randomForest(am ~ ., mtcars)
visreg(fit, "wt")
