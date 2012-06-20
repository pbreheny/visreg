Terms <- function(fit,x,trans,alpha)
  {
    fit <- update(fit,data=centerFrame(fit,attr(x$x,"name")))
    V <- vcov(fit)
    SE <- sqrt(apply(x$XX * (x$XX %*% V),1,sum))
    yy <- x$XX%*%coef(fit)
    m <- ifelse(family(fit)$family %in% c("binomial","poisson"),qnorm(1-alpha/2),qt(1-alpha/2,fit$df.residual))
    upr <- yy + m*SE
    lwr <- yy - m*SE
    r <- x$X%*%coef(fit) + residuals(fit)
    return(list(fit=trans(yy),lwr=trans(lwr),upr=trans(upr),r=trans(r)))
  }
