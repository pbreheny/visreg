library(tinytest)
library(visreg)

# data = <call expression> must not crash data lookup
set.seed(1)
df <- data.frame(y = rnorm(60), x = rnorm(60))
d2 <- subset(df, x > -1)

v_sub <- visreg(lm(y ~ x, data = subset(df, x > -1)), "x", plot = FALSE)
v_brk <- visreg(lm(y ~ x, data = df[df$x > -1, ]), "x", plot = FALSE)
v_ref <- visreg(lm(y ~ x, data = d2), "x", plot = FALSE)

expect_equal(v_sub$res, v_ref$res)
expect_equal(v_sub$fit, v_ref$fit)
expect_equal(v_brk$res, v_ref$res)
expect_equal(v_brk$fit, v_ref$fit)
