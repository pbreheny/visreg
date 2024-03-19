getXY <- function(fit, f, name, nn, cond, type, trans, alpha, jitter, ...) {
  if (type=="conditional") {
    x <- setupD(fit, f, name, nn, cond, ...)
    y <- Response(fit, x, trans, alpha, ...)
  } else if (type=="contrast") {
    x <- setupX(fit, f, name, nn, cond, ...)
    y <- Terms(fit, f, x, trans, alpha, ...)
    x <- setupD(fit, f, name, nn, cond, ...)
  }
  
  if (jitter && is.numeric(x$x)) x$x <- jitter(x$x)
  dots <- list(...)
  if ('xtrans' %in% names(dots)) {
    x$xx <- dots$xtrans(x$xx)
    x$x <- dots$xtrans(x$x)
  }
  list(x=x, y=y)
}
