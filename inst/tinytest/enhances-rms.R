suppressPackageStartupMessages(library(rms))
data("birthwt", package = "MASS")
birthwt$race <- factor(birthwt$race, labels = c("White", "Black", "Other"))
# options(datadist=) looks up "dd" in the global environment specifically,
# not wherever this file happens to be sourced from
assign("dd", rms::datadist(birthwt), envir = .GlobalEnv)
options(datadist = "dd")

fit <- lrm(low ~ age + race + lwt, data = birthwt, x = TRUE, y = TRUE)
visreg(fit, "age") |> print() |> expect_silent()
visreg(fit, "age", scale = "response") |> print() |> expect_silent()

v <- visreg(fit, "age", plot = FALSE)
expect_equal(round(head(v$fit$visreg_fit), 3), c(-0.787, -0.795, -0.803, -0.811, -0.819, -0.827))
v <- visreg(fit, "age", scale = "response", plot = FALSE)
expect_equal(round(head(v$fit$visreg_fit), 3), c(0.313, 0.311, 0.309, 0.308, 0.306, 0.304))
