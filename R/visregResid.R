visregResid <- function(fit) {
  if (inherits(fit, "randomForest")) {
    if (fit$type=="regression") rr <- fit$y - fit$predicted
    if (fit$type=="classification") {
      P <- predict(fit, type="prob")
      rr <- (fit$y==colnames(P)[2]) - P[,2]
    }
  } else if (inherits(fit, 'coxph')) {
    rr <- residuals(fit, type='deviance')
  } else if (inherits(fit, 'gamlss')) {
    rr <- residuals(fit, what='mu')
  } else {
    rr <- residuals(fit)
  }
  if (!is.matrix(rr) & length(rr)>0) rr <- rr[!is.na(rr)]
  rr
}
