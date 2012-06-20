Response <- function(fit,x,trans,alpha)
  {
    r <- predict(fit,newdata=x$D)+residuals(fit)
    p <- predict(fit,newdata=x$DD,se.fit=TRUE)
    m <- ifelse(family(fit)$family %in% c("binomial","poisson"),qnorm(1-alpha/2),qt(1-alpha/2,fit$df.residual))
    upr <- p$fit + m*p$se.fit
    lwr <- p$fit - m*p$se.fit
    return(list(fit=trans(p$fit),lwr=trans(lwr),upr=trans(upr),r=trans(r)))
  }
