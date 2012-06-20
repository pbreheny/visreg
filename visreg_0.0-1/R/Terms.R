Terms <- function(fit,XD,alpha)
  {
    fit <- update(fit,data=centerFrame(fit,attr(XD$x,"name")))
    V <- vcov(fit)
    SE <- sqrt(apply(XD$XX * (XD$XX %*% V),1,sum))
    y <- XD$XX%*%coef(fit)
    lwr <- y-qt(1-alpha/2,fit$df.residual)*SE
    upr <- y+qt(1-alpha/2,fit$df.residual)*SE
    r <- XD$X%*%coef(fit) + residuals(fit)
    return(list(fit=y,lwr=lwr,upr=upr,r=r))
  }
