visreg <- function(fit, xvar, by, overlay=FALSE, breaks=3, type=c("conditional","effect"), trans=I, scale=c("linear","response"), 
                   xtrans, alpha=.05, nn=101, cond=list(), print.cond=missing(by) & (max(attr(terms(formula(fit)), "order"))>1), whitespace=0.2, 
                   partial=TRUE, band=TRUE, jitter=FALSE, strip.names=is.numeric(attr(v, "lev")), legend=TRUE, line.par=NULL, fill.par=NULL, points.par=NULL, ...) {
  ## Setup
  type <- match.arg(type)
  scale <- match.arg(scale)
  if (scale=="response") trans <- family(fit)$linkinv
  
  f <- setupF(fit, xvar)
  xvar <- attr(f, "xvar")
  if (attr(f, "needsUpdate")) fit <- update(fit, data=f)
  cond <- setupCond(cond, f, by, breaks)
  
  ## Calculate v
  v <- setupV(fit, f, xvar, nn, cond, type, trans, xtrans, alpha, jitter)
  attr(v, "yNameClass") <- if (scale=="response" | (class(fit)[1] %in% c("lm", "mlm") & identical(trans,I))) {if (type=="effect") 1 else 2} else 3
  if (!missing(by)) {
    attr(v, "by") <- by
    v <- subsetV(v, f, by)
  }
  
  ## Plot
  if (missing(by)) {
    visregPlot(v, partial, band, whitespace, print.cond, line.par, fill.par, points.par, ...)
  } else if (overlay) {
    visregOverlayPlot(v, partial, band, whitespace, strip.names, line.par, fill.par, points.par, ...)
    if (print.cond) printCond(v)
  } else {
    visregLatticePlot(v, partial, band, whitespace, legend, line.par, fill.par, points.par, ...)
    if (print.cond) printCond(v)
  }
  
  invisible(v)
}
