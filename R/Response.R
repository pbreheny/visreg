Response <- function(fit, x, trans, alpha, ...) {

  ## Calculate partial residuals
  rr <- visregResid(fit)
  nr <- if (is.matrix(rr)) nrow(rr) else length(rr)
  if (nr>0 && nrow(x$D) != nr) warning("Residuals do not match data; have you changed the original data set?  If so, visreg is probably not displaying the residuals for the data set that was actually used to fit the model.")
  y <- visregPred(fit, x$D, ...)
  if (is.null(rr)) {
    r <- NULL
  } else {
    r <- y + rr
  }

  # Calculate predictions
  p <- visregPred(fit, x$DD, se.fit=TRUE, ...)

  ## Format output
  if (inherits(p, "svystat")) {
    p <- list(fit=as.double(p), se.fit=sqrt(attr(p,"var")))
  } else if (inherits(fit, "rq")) {
    p <- list(fit=as.double(p[,1]), se.fit=as.double(p[,3]-p[,2])/(2*qnorm(.975)))
  } else if (inherits(fit, "rms")) {
    p$fit <- p$linear.predictors
  } else if (is.double(p)) {
    p <- list(fit=p, se.fit=NA)
  }
  m <- ifelse(identical(class(fit), "lm"), qt(1-alpha/2, fit$df.residual), qnorm(1-alpha/2))
  upr <- p$fit + m*p$se.fit
  lwr <- p$fit - m*p$se.fit
  if (is.matrix(p$fit)) {
    if (length(r)==0) {
      R <- matrix(NA, nrow(x$D), ncol=ncol(p$fit))
    } else {
      R <- matrix(trans(r), ncol=ncol(p$fit))
    }
    val <- list(fit=matrix(trans(p$fit), ncol=ncol(p$fit)), lwr=matrix(trans(lwr), ncol=ncol(p$fit)), upr=matrix(trans(upr), ncol=ncol(p$fit)), r=R)
    val$name <- colnames(val$fit) <- colnames(p$fit)
  } else {
    if (length(r)==0) r <- rep(NA_real_, nrow(x$D))
    val <- list(fit=as.double(trans(p$fit)), lwr=as.double(trans(lwr)), upr=as.double(trans(upr)), r=as.double(trans(r)), name=as.character(formula(fit)[2]))
  }
  val$pos <- rr>0
  if (length(val$pos)==0) {
    if (is.matrix(p$fit)) {
      val$pos <- matrix(NA, nrow(x$D), ncol(p$fit))
    } else {
      val$pos <- rep(NA_real_, nrow(x$D))
    }
  }
  val$n <- if (is.matrix(p$fit)) ncol(p$fit) else 1
  val
}
