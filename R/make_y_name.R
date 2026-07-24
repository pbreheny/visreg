make_y_name <- function(fit, scale, trans, type, var_labels = NULL) {
  label_or <- function(name) {
    lab <- if (!is.null(var_labels)) unname(var_labels[name]) else NA_character_
    if (is.na(lab)) name else lab
  }
  if (scale == "response" || (class(fit)[1] %in% c("lm", "aov") & identical(trans, I))) {
    if (type == "contrast") {
      y_name <- as.expression(substitute(
        list(Delta) * x,
        list(x = label_or(as.character(formula(fit)[2])))
      ))
    } else {
      y_name <- label_or(as.character(formula(fit)[2]))
    }
  } else if (inherits(fit, "mlm")) {
    if (type == "contrast") {
      y_name <- sapply(colnames(fit$residuals), function(y) {
        as.expression(substitute(list(Delta) * x, list(x = label_or(y))))
      })
    } else {
      y_name <- vapply(colnames(fit$residuals), label_or, character(1))
    }
  } else if (inherits(fit, "randomForest")) {
    if (fit$type == "regression") {
      y_name <- label_or(as.character(formula(fit)[2]))
    }
    if (fit$type == "classification") {
      y_name <- paste0("Pr(", label_or(as.character(formula(fit)[2])), ")")
    }
  } else {
    y_name <- NULL
  }
  y_name
}
