if(require(MASS)) {
  # rlm
  fit <- rlm(Ozone ~ ., data=airquality)
  visreg(fit, 'Wind')

  # polr
  fit <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
  visreg(fit, 'Infl', partial=FALSE, collapse=TRUE)
  visreg(fit, 'Infl', partial=FALSE, collapse=TRUE, gg=TRUE)
  visreg(fit, 'Infl', partial=FALSE, collapse=TRUE, overlay=TRUE)
  visreg(fit, 'Infl', type='contrast', partial=FALSE)
}
