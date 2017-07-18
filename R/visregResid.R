visregResid <- function(fit) {
  if ("randomForest" %in% class(fit)) {
    if (fit$type=="regression") rr <- fit$y - fit$predicted
    if (fit$type=="classification") {
      P <- predict(fit, type="prob")
      rr <- (fit$y==colnames(P)[2]) - P[,2]
    }
#   } else if ("svm" %in% class(fit)) {
#     rr <- fit$y - fit$fitted
  } else if ('coxph' %in% class(fit)) {
    rr <- residuals(fit, type='deviance')
  } else {
    rr <- residuals(fit)
  }
  if (!is.matrix(rr) & length(rr)>0) rr <- rr[!is.na(rr)]
  rr
}
