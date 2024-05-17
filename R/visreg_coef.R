visreg_coef <- function(fit) {
  if (inherits(fit, "lme")) {
    nlme::fixed.effects(fit)
  } else if (inherits(fit, "merMod")) {
    fit@beta
  } else if (inherits(fit, "glmmTMB")) {
    glmmTMB::fixef(fit)$cond
  } else if (inherits(fit, 'betareg')) {
    b <- coef(fit)
    b[-length(b)]
  } else {
    coef(fit)
  }
}
