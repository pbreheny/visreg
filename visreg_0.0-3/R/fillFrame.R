fillFrame <- function(f,x,fill)
  {
    name <- attr(x,"name")
    fillFunction <- switch(fill,mean=mean,zero=function(x){0},median=median)
    x2 <- lapply(f[,setdiff(names(f),name)],fillFunction)
    ## To-do: Add x3 for cond
    
    newdf <- data.frame(x,x2,check.names=FALSE)
    names(newdf)[1] <- name
    return(newdf)
  }
