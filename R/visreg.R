visreg <- function(fit, xvar, by, breaks=4, type=c("conditional","effect"), trans=I, scale=c("linear","response"), xtrans, alpha=.05, nn=101, cond=list(), whitespace=0.2, partial=TRUE, jitter=FALSE, strip.names=is.numeric(attr(v, "lev")), line.par=NULL, fill.par=NULL, points.par=NULL, ...) {
  ## Setup
  type <- match.arg(type)
  scale <- match.arg(scale)
  if (!missing(by) & !missing(cond)) stop("Cannot specify 'by' and 'cond' simultaneously")
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
    visregPlot(v, partial, whitespace, line.par, fill.par, points.par, ...)
  } else {
    visregLatticePlot(v, partial, whitespace, strip.names, line.par, fill.par, points.par, ...)
  }
  
  invisible(v)
}
