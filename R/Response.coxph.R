Response.coxph <- function(fit, x, trans, alpha)
{
  attach(x)
  r <- predict(fit, newdata=D) + fit$residuals
  p <- predict(fit, newdata=DD, se.fit=TRUE)
  detach(x)
  m <- ifelse(identical(class(fit),"lm"),qt(1-alpha/2,fit$df.residual),qnorm(1-alpha/2))
  upr <- p$fit + m*p$se.fit
  lwr <- p$fit - m*p$se.fit    
  list(fit=trans(p$fit), lwr=trans(lwr), upr=trans(upr), r=trans(r))
}
