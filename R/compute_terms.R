compute_terms <- function(fit, f, x, trans, alpha, ...) {
  b <- visreg_coef(fit)

  if (inherits(fit, "mlm")) {
    summ <- summary(fit)
    n_y <- length(summ)
    yy <- se <- matrix(NA, nrow = nrow(x$matrix_fit), ncol = n_y)
    r <- rr <- matrix(NA, nrow = nrow(x$matrix_res), ncol = n_y)
    for (i in 1:n_y) {
      v <- summ[[i]]$sigma^2 * summ[[i]]$cov.unscaled
      se[, i] <- Matrix::rowSums(x$matrix_fit * (x$matrix_fit %*% v)) |> sqrt()
      ind <- is.finite(b[, i])
      yy[, i] <- x$matrix_fit %*% b[ind, i]
      rr[, i] <- visreg_resid(fit)[, i]
      r[, i] <- x$matrix_res %*% b[ind, i] + rr[, i]
    }
  } else {
    if (inherits(fit, "glmmTMB")) {
      v <- vcov(fit)$cond
    } else {
      v <- vcov(fit)
    }
    dg <- if (inherits(v, "Matrix")) Matrix::diag(v) else diag(v)
    if (inherits(fit, "polr")) {
      keep <- grep("|", colnames(v), fixed = TRUE, invert = TRUE)
      v <- v[keep, keep, drop = FALSE]
    } else if (inherits(fit, "betareg")) {
      v <- v[-nrow(v), -ncol(v), drop = FALSE]
    }
    if (anyNA(dg)) {
      keep <- which(!is.na(dg))
      v <- v[keep, keep, drop = FALSE]
    }
    se <- Matrix::rowSums(x$matrix_fit * (x$matrix_fit %*% v)) |> sqrt()
    yy <- drop(x$matrix_fit %*% b[is.finite(b)])
    rr <- visreg_resid(fit)
    if (is.null(rr)) {
      rr <- rep(NA, nrow(x$matrix_res))
    }
    r <- drop(x$matrix_res %*% b[is.finite(b)]) + rr
    if (nrow(x$matrix_res) != length(rr)) {
      warning(
        "Residuals do not match data; have you changed the original data set? If so, visreg is",
        "probably not displaying the residuals for the data set that was actually used to fit",
        "the model.",
        call. = FALSE
      )
    }
  }
  if (!all(is.finite(b))) {
    warning("prediction from a rank-deficient fit may be misleading", call. = FALSE)
  }
  m <- ifelse(
    class(fit)[1] == "lm" || class(fit)[1] == "mlm",
    qt(1 - alpha / 2, fit$df.residual),
    qnorm(1 - alpha / 2)
  )
  lwr <- yy - m * se
  upr <- yy + m * se
  if (inherits(fit, "mlm")) {
    val <- list(
      fit = matrix(as.double(trans(yy)), ncol = n_y),
      lwr = matrix(as.double(trans(lwr)), ncol = n_y),
      upr = matrix(as.double(trans(upr)), ncol = n_y),
      r = matrix(as.double(trans(r)), ncol = n_y)
    )
    val$name <- colnames(val$fit) <- colnames(fit$fitted.values)
  } else {
    val <- list(
      fit = as.double(trans(yy)),
      lwr = as.double(trans(lwr)),
      upr = as.double(trans(upr)),
      r = as.double(trans(r)),
      name = as.character(formula(fit)[2])
    )
  }
  val$pos <- rr > 0
  val$n_y <- if (inherits(fit, "mlm")) n_y else 1
  val
}
