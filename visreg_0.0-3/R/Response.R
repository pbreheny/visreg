Response <- function(fit,XD,trans,alpha)
  {
    r <- predict(fit,newdata=XD$D)+residuals(fit)
    p <- predict(fit,newdata=XD$DD,interval="confidence",level=1-alpha)
    return(list(fit=trans(p[,"fit"]),lwr=trans(p[,"lwr"]),upr=trans(p[,"upr"]),r=trans(r)))
  }
