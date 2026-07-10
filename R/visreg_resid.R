visreg_resid <- function(fit) {
  if (inherits(fit, "randomForest")) {
    if (fit$type == "regression") {
      rr <- fit$y - fit$predicted
    }
    if (fit$type == "classification") {
      p_mat <- predict(fit, type = "prob")
      rr <- (fit$y == colnames(p_mat)[2]) - p_mat[, 2]
    }
  } else if (inherits(fit, "coxph")) {
    rr <- residuals(fit, type = "deviance")
  } else if (inherits(fit, "gamlss")) {
    rr <- residuals(fit, what = "mu")
  } else if (inherits(fit, "glmmTMB")) {
    rr <- residuals(fit, type = "pearson")
  } else if (inherits(fit, "betareg")) {
    rr <- residuals(fit, type = "deviance")
  } else {
    rr <- residuals(fit)
  }
  if (!is.matrix(rr) && length(rr) > 0) {
    rr <- rr[!is.na(rr)]
  }
  rr
}
