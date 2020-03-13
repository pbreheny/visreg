visregPred <- function(fit, Data, se.fit=FALSE, ...) {
  predict.args <- list(object=fit, newdata=Data)
  if (inherits(fit, "lme")) predict.args$level <- 0
  if (inherits(fit, "merMod")) predict.args$re.form <- NA
  if (inherits(fit, "rq")) predict.args$interval <- "confidence"
  if (inherits(fit, "svm")) predict.args$probability <- TRUE
  if (inherits(fit, "multinom") | inherits(fit, "polr")) predict.args$type <- "probs"
  if (inherits(fit, "gbm")) predict.args$n.trees <- length(fit$trees)
  if (inherits(fit, "betareg")) predict.args$type <- "link"
  dots <- list(...)
  if (length(dots)) predict.args[names(dots)] <- dots

  if (se.fit) {
    if (inherits(fit, "mlm")) {
      p <- list(fit = suppressWarnings(do.call("predict", predict.args)), se.fit = se.mlm(fit, newdata=Data))
    } else if (inherits(fit, "randomForest") && fit$type=="classification") {
      predict.args$type <- "prob"
      P <- suppressWarnings(do.call("predict", predict.args))
      p <- list(fit=P[,2], se.fit=NA)
    } else if (inherits(fit, "loess")) {
      predict.args$se <- TRUE
      p <- suppressWarnings(do.call("predict", predict.args))
    } else {
      predict.args$se.fit <- TRUE
      p <- suppressWarnings(do.call("predict", predict.args))
    }
  } else {
    if (inherits(fit, "randomForest") && fit$type=="classification") {
      p <- predict(fit, type="prob")[,2]
    } else if (inherits(fit, 'rq')) {
      p <- suppressWarnings(do.call("predict", predict.args))[,1]
    } else {
      p <- suppressWarnings(do.call("predict", predict.args))
    }
  }
  if (inherits(fit, "svm") && fit$type < 3) p <- attr(p, "probabilities")
  p
}
