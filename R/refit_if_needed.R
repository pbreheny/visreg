refit_if_needed <- function(fit, dat) {
  if (!attr(dat, "needs_update")) {
    return(fit)
  }
  if (inherits(fit, "coxph")) {
    update(fit, formula = formula(fit), data = dat, model = TRUE)
  } else {
    update(fit, formula = formula(fit), data = dat)
  }
}
