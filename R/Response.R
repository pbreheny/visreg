Response <- function(fit, x, trans, alpha)
{
  rr <- residuals(fit)
  r <- predict(fit, newdata=x$D) + rr
  if (class(fit)[1]=="mlm") {
    p <- list(fit = predict(fit, newdata=x$DD), se.fit = se.mlm(fit, newdata=x$DD))
  } else p <- predict(fit, newdata=x$DD, se.fit=TRUE)
  if (is.numeric(p)) p <- list(fit=p, se.fit=NA)
  m <- ifelse(identical(class(fit),"lm"),qt(1-alpha/2,fit$df.residual),qnorm(1-alpha/2))
  upr <- p$fit + m*p$se.fit
  lwr <- p$fit - m*p$se.fit
  if (class(fit)[1]=="mlm") {
    val <- list(fit=matrix(trans(p$fit), ncol=ncol(p$fit)), lwr=matrix(trans(lwr), ncol=ncol(p$fit)), upr=matrix(trans(upr), ncol=ncol(p$fit)), r=matrix(trans(r), ncol=ncol(p$fit)))
    colnames(val$fit) <- colnames(p$fit)
  } else {
    val <- list(fit=as.numeric(trans(p$fit)), lwr=as.numeric(trans(lwr)), upr=as.numeric(trans(upr)), r=as.numeric(trans(r)))
  }
  val
}
