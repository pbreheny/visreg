fit <- lm(cbind(Sepal.Length, Sepal.Width, Petal.Length) ~ Species + Petal.Width, iris)
par(mfrow=c(3,1), mar=c(5, 5, 0.5, 0.5))
visreg(fit, "Species")
visreg(fit, "Species", type="contrast")
visreg(fit, "Petal.Width")
visreg(fit, "Petal.Width", type="contrast")
visreg(fit, "Petal.Width", by="Species")
visreg(fit, "Species", by="Petal.Width")

# Works with no names?
Y <- with(iris, cbind(Sepal.Length, Sepal.Width, Petal.Length))
dimnames(Y) <- NULL
fit <- lm(Y ~ Species + Petal.Width, iris)
visreg:::se.mlm(fit)
visreg(fit, "Species")

## Rug
par(mfrow=c(3,1), mar=c(5, 5, 0.5, 0.5))
visreg(fit, "Petal.Width", rug=TRUE, jitter=TRUE)
visreg(fit, "Petal.Width", rug=TRUE, jitter=TRUE, type="contrast")
visreg(fit, "Species", rug=TRUE)
visreg(fit, "Species", rug=TRUE, type="contrast")
visreg(fit, "Petal.Width", by="Species", rug=TRUE)
visreg(fit, "Petal.Width", by="Species", rug=TRUE, overlay=TRUE, jitter=TRUE)
visreg(fit, "Species", by="Petal.Width", rug=TRUE, overlay=TRUE)

par(mfrow=c(1,1), mar=c(5,5,2,2))
visreg2d(fit, "Species", "Petal.Width")

fit <- lm(cbind(Sepal.Length, Sepal.Width) ~ Petal.Length*Petal.Width, iris)
visreg(fit, "Petal.Width", by="Petal.Length")
visreg2d(fit, "Petal.Width", "Petal.Length")

visreg2d(fit, "Petal.Width", "Petal.Length", plot.type="persp")
visreg2d(fit, "Petal.Width", "Petal.Length", plot.type="persp", type="contrast")
#visreg2d(fit, "Petal.Width", "Petal.Length", plot.type="rgl")
