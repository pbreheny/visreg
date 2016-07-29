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
  if (is.numeric(p)) p <- list(fit=p, se.fit=NA)
  if (class(p)=="svystat") p <- list(fit=as.numeric(p), se.fit=sqrt(attr(p,"var")))
  if ("rq" %in% class(fit)) p <- list(fit=as.numeric(p$fit[,1]), se.fit=as.numeric(p$fit[,3]-p$fit[,2])/(2*qnorm(.975)))
  if ("rms" %in% class(fit)) p$fit <- p$linear.predictors
  m <- ifelse(identical(class(fit),"lm"), qt(1-alpha/2,fit$df.residual), qnorm(1-alpha/2))
  upr <- p$fit + m*p$se.fit
  lwr <- p$fit - m*p$se.fit
  if (length(r)==0) r <- as.numeric(rep(NA, nrow(x$D)))
  if (is.matrix(p$fit)) {
    val <- list(fit=matrix(trans(p$fit), ncol=ncol(p$fit)), lwr=matrix(trans(lwr), ncol=ncol(p$fit)), upr=matrix(trans(upr), ncol=ncol(p$fit)), r=matrix(trans(r), ncol=ncol(p$fit)))
    val$name <- colnames(val$fit) <- colnames(p$fit)
  } else {
    val <- list(fit=as.numeric(trans(p$fit)), lwr=as.numeric(trans(lwr)), upr=as.numeric(trans(upr)), r=as.numeric(trans(r)), name=as.character(formula(fit)[2]))
  }
  val$pos <- rr>0
  if (length(val$pos)==0) {
    if (is.matrix(p$fit)) {
      val$pos <- matrix(NA, nrow(x$D), ncol(p$fit))
    } else {
      val$pos <- as.numeric(rep(NA, nrow(x$D)))
    }
  }
  val$n <- if (is.matrix(p$fit)) ncol(p$fit) else 1
  val
}
