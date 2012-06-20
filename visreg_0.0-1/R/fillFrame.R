fillFrame <- function(fit,x,fill)
  {
    name <- attr(x,"name")
    X2 <- fillX(fit,fill,exclude=name)
    ## To-do: Add X3 for cond
    
    newdf <- data.frame(x,as.list(X2))
    names(newdf)[1] <- name
    return(newdf)
  }
