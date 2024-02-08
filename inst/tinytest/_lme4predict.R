library(lme4, quietly=TRUE)
data(Milk, package="nlme")
ctrl <- lmerControl(optCtrl=list(xtol_rel=1e-6)) # Warning otherwise
fit <- lmer(protein ~ Diet + Time + (Time|Cow), Milk, control=ctrl)
predict(fit, Milk[1,])
f <- function(object, newdata, se.fit=FALSE, ...) {
  if (se.fit) merTools::predictInterval(object, newdata) |> as.matrix()
  else lme4:::predict.merMod(object, newdata, ...)
}
registerS3method('predict', 'lmerMod', f)
predict(fit, Milk[1,])
predict(fit, Milk[1,], se.fit=TRUE)

visreg(fit, 'Diet')
