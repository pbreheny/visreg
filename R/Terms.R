Terms <- function(fit, f, x, trans, alpha, ...) {
  b <- if ("lme" %in% class(fit)) fixed.effects(fit) else coef(fit)
  if (class(fit)[1]=="mlm") {
    summ <- summary(fit)
    n.y <- length(summ)
    yy <- SE <- matrix(NA, nrow=nrow(x$XX), ncol=n.y)
    r <- rr <- matrix(NA, nrow=nrow(x$X), ncol=n.y)
    if (nrow(x$X) != nrow(rr)) warning("Residuals do not match data; have you changed the original data set?  If so, visreg is probably not displaying the residuals for the data set that was actually used to fit the model.")
    for (i in 1:n.y) {
      V <- summ[[i]]$sigma^2 * summ[[i]]$cov.unscaled
      SE[,i] <- sqrt(apply(x$XX * (x$XX %*% V),1,sum))
      ind <- is.finite(b[,i])
      yy[,i] <- x$XX%*%b[ind,i]
      rr[,i] <- residuals(fit)[,i]
      r[,i] <- x$X%*%b[ind,i] + rr[,i]
    }
  } else {
    V <- vcov(fit)
    SE <- sqrt(apply(x$XX * (x$XX %*% V),1,sum))
    yy <- x$XX%*%b[is.finite(b)]
    rr <- residuals(fit)
    if (nrow(x$X) != length(rr)) warning("Residuals do not match data; have you changed the original data set?  If so, visreg is probably not displaying the residuals for the data set that was actually used to fit the model.")
    r <- x$X%*%b[is.finite(b)] + rr
  }
  if (!all(is.finite(b))) warning("prediction from a rank-deficient fit may be misleading")
  m <- ifelse(class(fit)=="coxph" || "lme" %in% class(fit) || family(fit)$family %in% c("binomial","poisson"), qnorm(1-alpha/2), qt(1-alpha/2,fit$df.residual))
  lwr <- yy - m*SE
  upr <- yy + m*SE
  if (class(fit)[1]=="mlm") {
    val <- list(fit=matrix(as.numeric(trans(yy)), ncol=n.y), lwr=matrix(as.numeric(trans(lwr)), ncol=n.y), upr=matrix(as.numeric(trans(upr)), ncol=n.y), r=matrix(as.numeric(trans(r)), ncol=n.y))
    val$name <- colnames(val$fit) <- colnames(fit$fitted.values)
  } else {
    val <- list(fit=as.numeric(trans(yy)), lwr=as.numeric(trans(lwr)), upr=as.numeric(trans(upr)), r=as.numeric(trans(r)), name=as.character(formula(fit)[2]))
  }
  val$pos <- rr>0
  val$n <- if (class(fit)[1]=="mlm") n.y else 1
  val
}
