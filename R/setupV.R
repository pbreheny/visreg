## Each element of v should be a list of x and y for plotting
## v is a list of these elements, one element for each xvar or condition
setupV <- function(fit, f, xvar, nn, cond, type, trans, xtrans, alpha, jitter, ...) {
  if (length(xvar) > 1 & length(cond) > 1) stop("Cannot specify 'by' and multiple x variables simultaneously")
  J <- max(length(xvar), length(cond))
  v <- vector("list", J)
  for (j in 1:J) {
    cond.j <- if (length(cond) > 1) cond[[j]] else cond[[1]]
    name <- if (length(xvar) > 1) xvar[j] else xvar
    v[[j]] <- getXY(fit, f, name, nn, cond.j, type, trans, xtrans, alpha, jitter, ...)
  }
  if (length(cond)==1) names(v) <- xvar
  attr(v, "lev") <- attr(cond, "lev")
  attr(v, "hasInteraction") <- (max(attr(terms(formula(fit)), "order")) > 1)
  v
}
