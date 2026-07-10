# Variables that appear together with xvar in a higher-order (interaction) term of fit's formula
interacting_vars <- function(fit, xvar) {
  tt <- try(terms(as.formula(formula(fit))), silent = TRUE)
  if (inherits(tt, "try-error")) return(character(0))
  fac <- attr(tt, "factors")
  ord <- attr(tt, "order")
  if (is.null(fac) || is.null(ord) || !(xvar %in% rownames(fac))) return(character(0))
  hi_order_terms <- which(ord > 1 & fac[xvar, ] > 0)
  if (length(hi_order_terms) == 0) return(character(0))
  vars <- rownames(fac)[apply(fac[, hi_order_terms, drop = FALSE] > 0, 1, any)]
  setdiff(vars, xvar)
}
