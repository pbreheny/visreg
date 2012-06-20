Response <- function(fit,XD,alpha)
  {
    p <- predict(fit,newdata=XD$DD,interval="confidence",level=1-alpha)
    r <- predict(fit,newdata=XD$D)+residuals(fit)
    return(list(fit=p[,"fit"],lwr=p[,"lwr"],upr=p[,"upr"],r=r))
  }
