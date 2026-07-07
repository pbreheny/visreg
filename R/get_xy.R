get_xy <- function(fit, f, name, nn, cond, type, trans, alpha, jitter, ...) {
  if (type == "conditional") {
    x <- setup_data(fit, f, name, nn, cond, ...)
    y <- compute_response(fit, x, trans, alpha, ...)
  } else if (type == "contrast") {
    x <- setup_contrast_data(fit, f, name, nn, cond, ...)
    y <- compute_terms(fit, f, x, trans, alpha, ...)
    x <- setup_data(fit, f, name, nn, cond, ...)
  }

  if (jitter && is.numeric(x$x)) {
    x$x <- jitter(x$x)
  }
  dots <- list(...)
  if ("xtrans" %in% names(dots)) {
    x$xx <- dots$xtrans(x$xx)
    x$x <- dots$xtrans(x$x)
  }
  list(x = x, y = y)
}
