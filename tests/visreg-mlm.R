require(visreg)
fit <- lm(cbind(Sepal.Length, Sepal.Width, Petal.Length) ~ Species + Petal.Width, iris)
par(mfrow=c(3,1), mar=c(5, 5, 0.5, 0.5))
visreg(fit, "Species")
visreg(fit, "Petal.Width")
visreg(fit, "Species", type="effect")
visreg(fit, "Petal.Width", type="effect")
visreg(fit, "Petal.Width", by="Species")
visreg(fit, "Species", by="Petal.Width")

## Rug
visreg(fit, "Petal.Width", rug=TRUE, jitter=TRUE)
visreg(fit, "Petal.Width", rug=TRUE, jitter=TRUE, type="effect")
visreg(fit, "Species", rug=TRUE)
visreg(fit, "Species", rug=TRUE, type="effect")
visreg(fit, "Petal.Width", by="Species", rug=TRUE)
visreg(fit, "Petal.Width", by="Species", rug=TRUE, overlay=TRUE, jitter=TRUE)
visreg(fit, "Species", by="Petal.Width", rug=TRUE, overlay=TRUE)

visreg2d(fit, "Species", "Petal.Width")

fit <- lm(cbind(Sepal.Length, Sepal.Width) ~ Petal.Length*Petal.Width, iris)
visreg(fit, "Petal.Width", by="Petal.Length")
visreg2d(fit, "Petal.Width", "Petal.Length")
visreg2d(fit, "Petal.Width", "Petal.Length", plot.type="persp")
visreg2d(fit, "Petal.Width", "Petal.Length", plot.type="persp", type="effect")
visreg2d(fit, "Petal.Width", "Petal.Length", plot.type="rgl") ## Doesn't really work

