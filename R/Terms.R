Terms <- function(fit, f, x, trans, alpha, ...) {
  if (inherits(fit, "lme")) {
    b <- nlme::fixed.effects(fit)
  } else if (inherits(fit, "merMod")) {
    b <- fit@beta
  } else {
    b <- coef(fit)
  }

  if (inherits(fit, "mlm")) {
    summ <- summary(fit)
    n.y <- length(summ)
    yy <- SE <- matrix(NA, nrow=nrow(x$XX), ncol=n.y)
    r <- rr <- matrix(NA, nrow=nrow(x$X), ncol=n.y)
    for (i in 1:n.y) {
      V <- summ[[i]]$sigma^2 * summ[[i]]$cov.unscaled
      SE[,i] <- sqrt(apply(x$XX * (x$XX %*% V), 1, sum))
      ind <- is.finite(b[,i])
      yy[,i] <- x$XX%*%b[ind,i]
      rr[,i] <- visregResid(fit)[,i]
      r[,i] <- x$X%*%b[ind,i] + rr[,i]
    }
  } else {
    V <- vcov(fit)
    dg <- if (inherits(V, 'Matrix')) Matrix::diag(V) else diag(V)
    if (inherits(fit, 'polr')) {
      remove <- grep("|", colnames(V), fixed=TRUE)
      V <- V[-remove,-remove, drop=FALSE]
    }
    if (any(is.na(dg))) {
      remove <- which(is.na(dg))
      V <- V[-remove,-remove, drop=FALSE]
    }
    SE <- sqrt(apply(x$XX * (x$XX %*% V), 1, sum))
    yy <- drop(x$XX%*%b[is.finite(b)])
    rr <- visregResid(fit)
    if (is.null(rr)) rr <- rep(NA, nrow(x$X))
    r <- drop(x$X%*%b[is.finite(b)]) + rr
    if (nrow(x$X) != length(rr)) warning("Residuals do not match data; have you changed the original data set?  If so, visreg is probably not displaying the residuals for the data set that was actually used to fit the model.")
  }
  if (!all(is.finite(b))) warning("prediction from a rank-deficient fit may be misleading")
  m <- ifelse(identical(class(fit), "lm") || identical(class(fit), "mlm"), qt(1-alpha/2, fit$df.residual), qnorm(1-alpha/2))
  lwr <- yy - m*SE
  upr <- yy + m*SE
  if (inherits(fit, "mlm")) {
    val <- list(fit=matrix(as.double(trans(yy)), ncol=n.y), lwr=matrix(as.double(trans(lwr)), ncol=n.y), upr=matrix(as.double(trans(upr)), ncol=n.y), r=matrix(as.double(trans(r)), ncol=n.y))
    val$name <- colnames(val$fit) <- colnames(fit$fitted.values)
  } else {
    val <- list(fit=as.double(trans(yy)), lwr=as.double(trans(lwr)), upr=as.double(trans(upr)), r=as.double(trans(r)), name=as.character(formula(fit)[2]))
  }
  val$pos <- rr>0
  val$n <- if (inherits(fit, "mlm")) n.y else 1
  val
}
