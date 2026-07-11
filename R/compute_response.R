compute_response <- function(fit, x, trans, alpha, predict = list()) {
  # Calculate partial residuals
  res <- visreg_resid(fit)
  n_res <- if (is.matrix(res)) nrow(res) else length(res)
  if (n_res > 0 && nrow(x$frame_res) != n_res) {
    warning(
      "Residuals do not match data; have you changed the original data set?  If so, visreg is",
      "probably not displaying the residuals for the data set that was actually used to fit",
      "the model.",
      call. = FALSE
    )
  }
  y <- visreg_pred(fit, x$frame_res, predict = predict)
  if (is.null(res)) {
    r <- NULL
  } else {
    r <- y + res
  }

  # Calculate predictions
  p <- visreg_pred(fit, x$frame_fit, se_fit = TRUE, predict = predict)

  # Format output
  if (inherits(p, "svystat")) {
    p <- list(fit = as.double(p), se.fit = sqrt(attr(p, "var")))
  } else if (inherits(fit, "rq")) {
    p <- list(fit = as.double(p[, 1]), se.fit = as.double(p[, 3] - p[, 2]) / (2 * qnorm(0.975)))
  } else if (inherits(fit, "rms")) {
    p$fit <- p$linear.predictors
  } else if (is.double(p)) {
    p <- list(fit = p, se.fit = NA)
  }
  m <- ifelse(class(fit)[1] == "lm", qt(1 - alpha / 2, fit$df.residual), qnorm(1 - alpha / 2))
  upr <- p$fit + m * p$se.fit
  lwr <- p$fit - m * p$se.fit
  if (is.matrix(p$fit)) {
    if (length(r) == 0) {
      r_mat <- matrix(NA, nrow(x$frame_res), ncol = ncol(p$fit))
    } else {
      r_mat <- matrix(trans(r), ncol = ncol(p$fit))
    }
    val <- list(
      fit = matrix(trans(p$fit), ncol = ncol(p$fit)),
      lwr = matrix(trans(lwr), ncol = ncol(p$fit)),
      upr = matrix(trans(upr), ncol = ncol(p$fit)),
      r = r_mat
    )
    val$name <- colnames(val$fit) <- colnames(p$fit)
  } else {
    if (length(r) == 0) {
      r <- rep(NA_real_, nrow(x$frame_res))
    }
    val <- list(
      fit = as.double(trans(p$fit)),
      lwr = as.double(trans(lwr)),
      upr = as.double(trans(upr)),
      r = as.double(trans(r)),
      name = as.character(formula(fit)[2])
    )
  }
  val$pos <- res > 0
  if (length(val$pos) == 0) {
    if (is.matrix(p$fit)) {
      val$pos <- matrix(NA, nrow(x$frame_res), ncol(p$fit))
    } else {
      val$pos <- rep(NA_real_, nrow(x$frame_res))
    }
  }
  val$n_y <- if (is.matrix(p$fit)) ncol(p$fit) else 1
  val
}
