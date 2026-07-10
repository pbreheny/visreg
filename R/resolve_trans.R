resolve_trans <- function(fit, scale, trans, type) {
  if (scale == "response") {
    if (inherits(fit, "lrm")) {
      trans <- binomial()$linkinv
    } else if (inherits(fit, "betareg")) {
      trans <- fit$link$mean$linkinv
    } else {
      trans <- family(fit)$linkinv
    }
  }
  if (!identical(trans, I) && type == "contrast") {
    warning(
      "You are attempting to transform a contrast. The resulting plot is not guaranteed",
      "to be meaningful.",
      call. = FALSE
    )
  }
  trans
}
