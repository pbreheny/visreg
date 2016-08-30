subset.visreg <- function(x, sub, ...) {
  x$fit <- x$fit[eval(match.call()$sub, x$fit),]
  x$res <- x$res[eval(match.call()$sub, x$res),]
  x
}
