getXY <- function(fit, f, name, nn, cond, type, trans, xtrans, alpha, jitter, ...) {
  if (type=="conditional") {
    x <- setupD(fit, f, name, nn, cond, ...)
    y <- Response(fit, x, trans, alpha, ...)
  } else if (type=="contrast") {
    x <- setupX(fit, f, name, nn, cond, ...)
    y <- Terms(fit, f, x, trans, alpha, ...)
    x <- setupD(fit, f, name, nn, cond, ...)
  }
  
  if (jitter) x$x <- jitter(x$x)
  if (!missing(xtrans)) {
    x$xx <- xtrans(x$xx)
    x$x <- xtrans(x$x)
  }
  list(x=x,y=y)
}
