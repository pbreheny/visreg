airquality$Heat <- cut(airquality$Temp, 3, labels = c("Cool", "Mild", "Hot"))
fit <- lm(Ozone ~ Solar.R + Wind * Heat, data = airquality)
v <- visreg(fit, "Wind", by = "Heat", plot = FALSE)

vv <- subset(v, Heat %in% c("Cool", "Hot"))
expect_equal(sort(unique(as.character(vv$fit$Heat))), c("Cool", "Hot"))
expect_equal(sort(unique(as.character(vv$res$Heat))), c("Cool", "Hot"))
plot(vv) |> print() |> expect_silent()

vv <- subset(v, Wind < 15)
expect_true(all(vv$fit$Wind < 15))
expect_true(all(vv$res$Wind < 15))
plot(vv) |> print() |> expect_silent()
