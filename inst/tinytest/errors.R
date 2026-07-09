# Cross-cutting input-validation / error-path tests that don't belong to any
# one model family.

# xvar not a column in the model
fit <- lm(Ozone ~ Wind, data = airquality)
expect_error(visreg(fit, "NotAVar"))

# Intercept-only model has no predictors
fit <- lm(Ozone ~ 1, data = airquality)
expect_error(visreg(fit))

# Cannot specify 'by' together with multiple xvars
fit <- lm(Ozone ~ Wind + Temp + Solar.R, data = airquality)
expect_error(visreg(fit, c("Wind", "Temp"), by = "Solar.R"))

# type='effect' is a deprecated alias for type='contrast'
expect_warning(visreg(fit, "Wind", type = "effect", plot = FALSE))
expect_warning(visreg2d(fit, x = "Wind", y = "Temp", type = "effect", plot = FALSE))

# visreg2d requires both x and y
expect_error(visreg2d(fit, x = "Wind"))

# scale='response' + type='contrast': trans is set internally from the link,
# triggering the same "transforming a contrast" warning a user-supplied
# trans would
data("birthwt", package = "MASS")
birthwt$race <- factor(birthwt$race, labels = c("White", "Black", "Other"))
fit_glm <- glm(low ~ age + race + lwt, data = birthwt, family = "binomial")
expect_warning(visreg(fit_glm, "age", scale = "response", type = "contrast", plot = FALSE))
expect_warning(visreg2d(fit_glm, x = "age", y = "lwt", scale = "response", type = "contrast", plot = FALSE))

# by= breaks that don't match the levels of a factor by-variable
airquality$Heat <- cut(airquality$Temp, 3, labels = c("Cool", "Mild", "Hot"))
fit_int <- lm(Ozone ~ Wind * Heat, data = airquality)
expect_error(visreg(fit_int, "Wind", by = "Heat", breaks = "NotALevel", plot = FALSE))

# An invalid factor level for a contrast reference falls back to the
# reference level with a warning
fit_add <- lm(Ozone ~ Wind + Heat, data = airquality)
expect_warning(visreg(fit_add, "Heat", type = "contrast", cond = list(Heat = "nope"), plot = FALSE))
# A numeric index for a factor's contrast reference is also accepted
expect_warning(visreg(fit_add, "Heat", type = "contrast", cond = list(Heat = 2), plot = FALSE))

# visreg_list()'s labels must match the number of elements being combined
fit1 <- lm(Ozone ~ Wind, airquality)
fit2 <- lm(Ozone ~ Temp, airquality)
expect_error(visreg_list(
  visreg(fit1, "Wind", plot = FALSE),
  visreg(fit2, "Temp", plot = FALSE),
  labels = c("only one"),
  collapse = TRUE
))

# setup_frame()'s data-resolution fallbacks and failure mode

## Response stored directly as a Surv object column (rather than inline
## Surv(time, status) in the formula) is detected and triggers a model update
suppressPackageStartupMessages(library(survival))
ovarian2 <- ovarian
ovarian2$S <- Surv(ovarian2$futime, ovarian2$fustat)
fit_surv <- coxph(S ~ age, data = ovarian2)
v <- visreg(fit_surv, "age", plot = FALSE)
expect_true(nrow(v$res) > 0)

## S4 model whose data is reachable via neither the call site nor fit@frame:
## setup_frame() must error rather than misbehave (constructed directly,
## since no supported S4 model class lacks both @frame and @data)
setClass("FakeS4Fit", representation(call = "ANY"))
fake_fit <- new("FakeS4Fit", call = quote(fakefit(y ~ x, data = NoSuchData)))
expect_error(visreg:::setup_frame(fake_fit, "x", globalenv(), NULL))

## Non-S4 model: data unreachable from either the call site or the fitting
## function's closure environment
Data <- data.frame(x = rnorm(20), y = rnorm(20))
fit_orphan <- lm(y ~ x, data = Data)
rm(Data)
expect_error(visreg(fit_orphan, "x"))

# se.mlm(): response names taken from the literal cbind() call when the
# fitted coefficient matrix itself has no column names
fit_mlm <- lm(cbind(unname(Sepal.Length), unname(Sepal.Width)) ~ Species + Petal.Width, iris)
expect_true(is.null(colnames(coef(fit_mlm))))
v <- visreg:::se.mlm(fit_mlm)
expect_equal(colnames(v), c("unname(Sepal.Length)", "unname(Sepal.Width)"))

# se.mlm(): a partially-empty response name (only some cbind() columns
# explicitly named) gets a placeholder
Y <- with(iris, cbind(Sepal.Length, Sepal.Width))
colnames(Y) <- c("", "y2")
fit_mlm2 <- lm(Y ~ Species + Petal.Width, iris)
v <- visreg:::se.mlm(fit_mlm2)
expect_equal(colnames(v), c("Y1", "y2"))

# compute_response(): residuals recomputed against a data set that has since
# grown relative to what the model was fit on
Data <- data.frame(x = rnorm(50), y = rnorm(50))
fit_stale <- lm(y ~ x, data = Data)
Data <- rbind(Data, data.frame(x = rnorm(5), y = rnorm(5)))
expect_warning(visreg(fit_stale, "x", plot = FALSE))
