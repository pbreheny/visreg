Terms <- function(fit,f,x,trans,alpha) {
  if (class(fit)[1]=="mlm") {
    summ <- summary.mlm(fit)
    n.y <- length(summ)
    yy <- SE <- matrix(NA, nrow=nrow(x$XX), ncol=n.y)
    r <- matrix(NA, nrow=nrow(x$X), ncol=n.y)
    for (i in 1:n.y) {
      V <- summ[[i]]$sigma^2 * summ[[i]]$cov.unscaled
      SE[,i] <- sqrt(apply(x$XX * (x$XX %*% V),1,sum))
      ind <- is.finite(coef(fit)[,i])
      yy[,i] <- x$XX%*%coef(fit)[ind,i]
      r[,i] <- x$X%*%coef(fit)[ind,i] + residuals(fit)[,i]
    }
  } else {
    V <- vcov(fit)
    SE <- sqrt(apply(x$XX * (x$XX %*% V),1,sum))
    yy <- x$XX%*%coef(fit)[is.finite(coef(fit))]
    rr <- if (class(fit)[1] %in% c("lm", "glm")) rstudent(fit) else residuals(fit)
    r <- x$X%*%coef(fit)[is.finite(coef(fit))] + rr
  }
  m <- ifelse(class(fit)=="coxph" || family(fit)$family %in% c("binomial","poisson"), qnorm(1-alpha/2), qt(1-alpha/2,fit$df.residual))
  lwr <- yy - m*SE
  upr <- yy + m*SE
  if (class(fit)[1]=="mlm") {
    val <- list(fit=matrix(as.numeric(trans(yy)), ncol=n.y), lwr=matrix(as.numeric(trans(lwr)), ncol=n.y), upr=matrix(as.numeric(trans(upr)), ncol=n.y), r=matrix(as.numeric(trans(r)), ncol=n.y))
    colnames(val$fit) <- colnames(fit$fitted.values)
  } else {
    val <- list(fit=as.numeric(trans(yy)), lwr=as.numeric(trans(lwr)), upr=as.numeric(trans(upr)), r=as.numeric(trans(r)))
  }
  val
}
