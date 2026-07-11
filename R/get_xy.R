get_xy <- function(fit, f, name, nn, cond, type, trans, alpha, jitter, predict) {
  if (type == "conditional") {
    x <- setup_data(fit, f, name, nn, cond)
    y <- compute_response(fit, x, trans, alpha, predict)
  } else if (type == "contrast") {
    x <- setup_contrast_data(fit, f, name, nn, cond)
    y <- compute_terms(fit, f, x, trans, alpha)
    x <- setup_data(fit, f, name, nn, cond)
  }

  if (jitter && is.numeric(x$x_res)) {
    x$x_res <- jitter(x$x_res)
  }
  list(x = x, y = y)
}
