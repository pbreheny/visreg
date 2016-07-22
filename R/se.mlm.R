se.mlm <- function(object, newdata) {
  coef <- coef(object)
  ny <- ncol(coef)
  effects <- object$effects
  resid <- object$residuals
  fitted <- object$fitted.values
  ynames <- colnames(coef)
  if (is.null(ynames)) {
    lhs <- object$terms[[2L]]
    if (mode(lhs) == "call" && lhs[[1L]] == "cbind")
      ynames <- as.character(lhs)[-1L]
    else ynames <- paste0("Y", seq_len(ny))
  }
  ind <- ynames == ""
  if (any(ind))
    ynames[ind] <- paste0("Y", seq_len(ny))[ind]
  value <- NULL
  cl <- oldClass(object)
  class(object) <- cl[match("mlm", cl):length(cl)][-1L]
  object$call$formula <- formula(object)
  for (i in seq(ny)) {
    object$coefficients <- coef[, i]
    names(object$coefficients) <- rownames(coef)
    object$residuals <- resid[, i]
    object$fitted.values <- fitted[, i]
    object$effects <- effects[, i]
    object$call$formula[[2L]] <- object$terms[[2L]] <- as.name(ynames[i])
    value <- cbind(value, predict(object, newdata=newdata, se.fit=TRUE)$se.fit)
  }
  colnames(value) <- ynames
  value
}
