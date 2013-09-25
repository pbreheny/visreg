Response <- function(fit, x, trans, alpha, ...) {
  level <- if ("level" %in% names(list(...))) list(...)$level else 0
  rr <- residuals(fit)
  nr <- if (is.matrix(rr)) nrow(rr) else length(rr)
  if (nrow(x$D) != nr) warning("Residuals do not match data; have you changed the original data set?  If so, visreg is probably not displaying the residuals for the data set that was actually used to fit the model.")
  r <- predict(fit, newdata=x$D, level=level) + rr
  if (class(fit)[1]=="mlm") {
    p <- list(fit = predict(fit, newdata=x$DD), se.fit = se.mlm(fit, newdata=x$DD))
  } else p <- predict(fit, newdata=x$DD, se=TRUE, level=level)
  if (class(p)=="svystat") p <- list(fit=as.numeric(p), se.fit=sqrt(attr(p,"var")))
  if (is.numeric(p)) p <- list(fit=p, se.fit=NA)
  m <- ifelse(identical(class(fit),"lm"),qt(1-alpha/2,fit$df.residual),qnorm(1-alpha/2))
  upr <- p$fit + m*p$se.fit
  lwr <- p$fit - m*p$se.fit
  if (class(fit)[1]=="mlm") {
    val <- list(fit=matrix(trans(p$fit), ncol=ncol(p$fit)), lwr=matrix(trans(lwr), ncol=ncol(p$fit)), upr=matrix(trans(upr), ncol=ncol(p$fit)), r=matrix(trans(r), ncol=ncol(p$fit)))
    val$name <- colnames(val$fit) <- colnames(p$fit)
  } else {
    val <- list(fit=as.numeric(trans(p$fit)), lwr=as.numeric(trans(lwr)), upr=as.numeric(trans(upr)), r=as.numeric(trans(r)), name=as.character(formula(fit)[2]))
  }
  val$pos <- rr>0
  val$n <- if (class(fit)[1]=="mlm") ncol(p$fit) else 1
  val
}
