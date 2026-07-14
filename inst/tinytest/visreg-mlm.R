suppressPackageStartupMessages(library(rgl))

fit <- lm(cbind(Sepal.Length, Sepal.Width, Petal.Length) ~ Species + Petal.Width, iris)
visreg(fit, "Species") |> print() |> expect_silent()
visreg(fit, "Species", type = "contrast") |> print() |> expect_silent()
visreg(fit, "Petal.Width") |> print() |> expect_silent()
visreg(fit, "Petal.Width", type = "contrast") |> print() |> expect_silent()
visreg(fit, "Petal.Width", by = "Species") |> print() |> expect_silent()
visreg(fit, "Species", by = "Petal.Width") |> print() |> expect_silent()

v <- visreg(fit, "Species", plot = FALSE)
expect_equal(round(v[[1]]$fit$visreg_fit, 3), c(5.972, 5.912, 5.922)) # Sepal.Length
expect_equal(round(v[[2]]$fit$visreg_fit, 3), c(4.251, 2.750, 2.407)) # Sepal.Width
expect_equal(round(v[[3]]$fit$visreg_fit, 3), c(2.536, 4.234, 4.812)) # Petal.Length

# Works with no names?
y <- with(iris, cbind(Sepal.Length, Sepal.Width, Petal.Length))
dimnames(y) <- NULL
fit <- lm(y ~ Species + Petal.Width, iris)
visreg:::se_mlm(fit) |> expect_silent()
visreg(fit, "Species") |> print() |> expect_silent()

# Rug
visreg(fit, "Petal.Width", rug = TRUE, jitter = TRUE) |> print() |> expect_silent()
visreg(fit, "Petal.Width", rug = TRUE, jitter = TRUE, type = "contrast") |>
  print() |>
  expect_silent()
visreg(fit, "Species", rug = TRUE) |> print() |> expect_silent()
visreg(fit, "Species", rug = TRUE, type = "contrast") |> print() |> expect_silent()
visreg(fit, "Petal.Width", by = "Species", rug = TRUE) |> print() |> expect_silent()
visreg(fit, "Petal.Width", by = "Species", rug = TRUE, overlay = TRUE, jitter = TRUE) |>
  print() |>
  expect_silent()
visreg(fit, "Species", by = "Petal.Width", rug = TRUE, overlay = TRUE) |> print() |> expect_silent()

visreg2d(fit, "Species", "Petal.Width") |> expect_silent()

fit <- lm(cbind(Sepal.Length, Sepal.Width) ~ Petal.Length * Petal.Width, iris)
visreg(fit, "Petal.Width", by = "Petal.Length") |> print() |> expect_silent()
p <- visreg2d(fit, "Petal.Width", "Petal.Length")
gridExtra::marrangeGrob(p, nrow = 2, ncol = 1, top = "") |> print() |> expect_silent()
p <- visreg2d(fit, "Petal.Width", "Petal.Length", type = "contrast")
gridExtra::marrangeGrob(p, nrow = 2, ncol = 1, top = "") |> print() |> expect_silent()

v <- visreg2d(fit, "Petal.Width", "Petal.Length", type = "contrast", plot = FALSE)
expect_equal(round(v[[1]]$z[1, 1], 3), -0.687)

visreg2d(fit, "Petal.Width", "Petal.Length", plot = FALSE) |> persp() |> expect_silent()
visreg2d(fit, "Petal.Width", "Petal.Length", plot = FALSE) |> rgl::persp3d() |> expect_silent()
