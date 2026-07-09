# Setup
library(survival)
library(splines)
ovarian$rx <- factor(ovarian$rx)

# Basic
fit <- coxph(Surv(futime, fustat) ~ age + rx, data = ovarian)
visreg(fit, "age") |> print() |> expect_silent()
visreg(fit, "age", type = "contrast") |> print() |> expect_silent()
visreg(fit, "rx") |> print() |> expect_silent()

v <- visreg(fit, "age", plot = FALSE)
expect_equal(round(head(v$fit$visreg_fit), 3), c(-2.545, -2.492, -2.440, -2.387, -2.335, -2.282))
expect_equal(round(head(v$fit$visreg_lwr), 3), c(-4.107, -4.022, -3.938, -3.853, -3.768, -3.684))
expect_equal(round(head(v$fit$visreg_upr), 3), c(-0.982, -0.962, -0.942, -0.922, -0.901, -0.881))

# Interaction
fit <- coxph(Surv(futime, fustat) ~ age * rx, data = ovarian)
visreg(fit, "age", cond = list(rx = "1"), print_cond = FALSE) |> print() |> expect_silent()
visreg(fit, "age", cond = list(rx = "2"), print_cond = FALSE) |> print() |> expect_silent()
visreg(fit, "age", by = "rx") |> print() |> expect_silent()
visreg2d(fit, x = "age", y = "rx") |> expect_silent()

# Splines
fit <- coxph(Surv(futime, fustat) ~ ns(age, 4) + rx, data = ovarian)
visreg(fit, "age") |> print() |> expect_silent()
visreg(fit, "rx") |> print() |> expect_silent()
visreg(fit, "age", type = "contrast") |> print() |> expect_silent()
visreg(fit, "rx", type = "contrast") |> print() |> expect_silent()

# Strata
ovarian$Group <- factor(ovarian$rx)
fit <- coxph(Surv(futime, fustat) ~ age + strata(Group), data = ovarian)
visreg(fit, "age") |> print() |> expect_silent()
visreg(fit, "age", type = 'contrast') |> print() |> expect_silent()

# Logical
ovarian$rx <- ovarian$rx == 2
fit <- coxph(Surv(futime, fustat) ~ age + rx, data = ovarian)
visreg(fit, "age") |> print() |> expect_silent()
