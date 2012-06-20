Response <- function(fit,x,trans,alpha)
  {
    r <- predict(fit,newdata=x$D)+residuals(fit)
    p <- predict(fit,newdata=x$DD,se.fit=TRUE)
    m <- ifelse(identical(class(fit),"lm"),qt(1-alpha/2,fit$df.residual),qnorm(1-alpha/2))
    upr <- p$fit + m*p$se.fit
    lwr <- p$fit - m*p$se.fit    
    return(list(fit=trans(p$fit),lwr=trans(lwr),upr=trans(upr),r=trans(r)))
  }
