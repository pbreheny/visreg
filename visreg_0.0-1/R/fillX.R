fillX <- function(fit,fill,exclude)
  {
    X <- model.matrix(fit)
    include <- setdiff(colnames(X),exclude)
    if (fill=="mean") val <- apply(X[,include],2,mean)
    else if (fill=="zero")
      {
        val <- rep(0,length(include))
        names(val) <- include
      }
    return(val)
  }
