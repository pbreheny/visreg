Response <- function(fit, x, trans, alpha)
{
  r <- predict(fit, newdata=x$D) + fit$residuals
  if (class(fit)[1]=="mlm") {
    p <- list(fit = predict(fit, newdata=x$DD), se.fit = se.mlm(fit, newdata=x$DD))
  } else p <- predict(fit, newdata=x$DD, se.fit=TRUE)
  if (is.numeric(p)) p <- list(fit=p, se.fit=NA)
  m <- ifelse(identical(class(fit),"lm"),qt(1-alpha/2,fit$df.residual),qnorm(1-alpha/2))
  upr <- p$fit + m*p$se.fit
  lwr <- p$fit - m*p$se.fit    
  list(fit=trans(p$fit), lwr=trans(lwr), upr=trans(upr), r=trans(r))
}
