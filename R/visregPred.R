visregPred <- function(fit, Data, se.fit=FALSE, ...) {
  predict.args <- list(object=fit, newdata=Data)
  if ("lme" %in% class(fit)) predict.args$level <- 0
  if (inherits(fit, "merMod")) predict.args$re.form <- NA
  if ("rq" %in% class(fit)) predict.args$interval <- "confidence"
  if ("svm" %in% class(fit)) predict.args$probability <- TRUE
  if ("multinom" %in% class(fit)) predict.args$type <- "probs"
  if ("gbm" %in% class(fit)) predict.args$n.trees <- length(fit$trees)
  dots <- list(...)
  if (length(dots)) predict.args[names(dots)] <- dots

  if (se.fit) {
    if (class(fit)[1]=="mlm") {
      p <- list(fit = suppressWarnings(do.call("predict", predict.args)), se.fit = se.mlm(fit, newdata=Data))
    } else if ("randomForest" %in% class(fit) && fit$type=="classification") {
      predict.args$type <- "prob"
      P <- suppressWarnings(do.call("predict", predict.args))
      p <- list(fit=P[,2], se.fit=NA)
    } else if (class(fit)[1] %in% c("loess")) {
      predict.args$se <- TRUE
      p <- suppressWarnings(do.call("predict", predict.args))
    } else {
      predict.args$se.fit <- TRUE
      p <- suppressWarnings(do.call("predict", predict.args))
    }
  } else {
    if ("randomForest" %in% class(fit) && fit$type=="classification") {
      p <- predict(fit, type="prob")[,2]
    } else if ('rq' %in% class(fit)) {
      p <- suppressWarnings(do.call("predict", predict.args))[,1]
    } else {
      p <- suppressWarnings(do.call("predict", predict.args))
    }
  }
  if ("svm" %in% class(fit) && fit$type < 3) p <- attr(p, "probabilities")
  p
}
