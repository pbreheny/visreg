make_y_name <- function(fit, scale, trans, type) {
  if (scale == "response" || (class(fit)[1] %in% c("lm", "aov") & identical(trans, I))) {
    if (type == "contrast") {
      y_name <- as.expression(substitute(list(Delta) * x, list(x = as.character(formula(fit)[2]))))
    } else {
      y_name <- as.character(formula(fit)[2])
    }
  } else if (inherits(fit, "mlm")) {
    if (type == "contrast") {
      y_name <- sapply(colnames(fit$residuals), function(y) {
        as.expression(substitute(list(Delta) * x, list(x = y)))
      })
    } else {
      y_name <- colnames(fit$residuals)
    }
  } else if (inherits(fit, "randomForest")) {
    if (fit$type == "regression") {
      y_name <- as.character(formula(fit)[2])
    }
    if (fit$type == "classification") {
      y_name <- paste0("Pr(", as.character(formula(fit)[2]), ")")
    }
  } else {
    y_name <- NULL
  }
  y_name
}
