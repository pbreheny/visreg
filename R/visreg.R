visreg <- function(fit, xvar, by, breaks=3, type=c("conditional", "contrast", "effect"), trans=I,
                   scale=c("linear","response"), xtrans, alpha=.05, nn=101, cond=list(), jitter=FALSE, collapse=FALSE,
                   plot=TRUE, ...) {
  ## Setup
  type <- match.arg(type)
  scale <- match.arg(scale)
  if (scale=="response") {
    if (class(fit)[1]=="lrm") {
      trans <- binomial()$linkinv
    } else {
      trans <- family(fit)$linkinv
    }
  }
  if (type=="effect") {
    warning("Please note that type='effect' is deprecated and may not be supported in future versions of visreg.  Use type='contrast' instead.")
    type <- "contrast"
  }

  f <- setupF(fit, xvar, parent.frame())
  xvar <- attr(f, "xvar")
  if (attr(f, "needsUpdate")) fit <- update(fit, data=f)
  cond <- setupCond(cond, f, by, breaks)

  ## Calculate v
  yName <- makeYName(fit, scale, trans, type)
  v <- setupV(fit, f, xvar, nn, cond, type, trans, xtrans, alpha, jitter, by, yName, ...)
  if (collapse) v <- collapse.visregList(v)

  ## Plot/return
  if (plot) plot(v, ...)
  invisible(v)
}
