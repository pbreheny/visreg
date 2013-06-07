visreg <- function(fit, xvar, by, overlay=FALSE, breaks=3, type=c("conditional", "contrast", "effect"), trans=I, scale=c("linear","response"), xtrans, 
                   alpha=.05, nn=101, cond=list(), print.cond=missing(by) & (max(attr(terms(formula(fit)), "order"))>1), whitespace=0.2, 
                   partial=(!rug), band=TRUE, rug=FALSE, jitter=FALSE, strip.names=is.numeric(attr(v, "lev")), legend=TRUE, ask=TRUE, line.par=NULL, fill.par=NULL, points.par=NULL, ...) {
  ## Setup
  type <- match.arg(type)
  scale <- match.arg(scale)
  if (scale=="response") trans <- family(fit)$linkinv
  if (type=="effect") {
    warning("Please note that type='effect' is deprecated and may not be supported in future versions of visreg.  Use type='contrast' instead.")
    type <- "contrast"
  }
  
  f <- setupF(fit, xvar, parent.frame())
  xvar <- attr(f, "xvar")
  if (attr(f, "needsUpdate")) fit <- update(fit, data=f)
  cond <- setupCond(cond, f, by, breaks)

  ## Calculate v
  v <- setupV(fit, f, xvar, nn, cond, type, trans, xtrans, alpha, jitter, ...)
  attr(v, "yNameClass") <- if (scale=="response" | (class(fit)[1] %in% c("lm", "mlm") & identical(trans,I))) {if (type=="contrast") 1 else 2} else 3
  if (!missing(by)) {
    attr(v, "by") <- by
    v <- subsetV(v, f, by)
  }
  
  ## Plot
  if (missing(by)) {
    visregPlot(v, partial, band, rug, ask, whitespace, print.cond, line.par, fill.par, points.par, ...)
  } else if (overlay) {
    visregOverlayPlot(v, partial, band, rug, ask, whitespace, strip.names, line.par, fill.par, points.par, ...)
    if (print.cond) printCond(v)
  } else {
    visregLatticePlot(v, partial, band, rug, ask, whitespace, legend, line.par, fill.par, points.par, ...)
    if (print.cond) printCond(v)
  }
  
  invisible(v)
}
