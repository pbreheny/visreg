fillFrame <- function(f,x,cond) {
  ## x  = data frame of x variable(s) being changed
  ## x2 = variables being filled by median
  ## x3 = variables specified by cond
  if (missing(cond)) cond <- NULL
  for (j in 1:ncol(f)) {
    if (is.factor(f[,j]) && !is.element(names(f)[j],names(cond)) && !is.element(names(f)[j],names(x))) {
      mode = names(sort(-table(f[j])))[1]
      eval(parse(text=c('cond=c(cond,list(',names(f)[j],'=factor(mode,levels=droplevels(levels(f[,names(f)[j]]))))')))
    }
  }
  x2 <- lapply(as.data.frame(f[,setdiff(names(f),c(names(x),names(cond)))]),median)
  names(x2) <- setdiff(names(f),c(names(x),names(cond)))
  x3 <- cond
  
  if (length(x2)>0 & length(x3)>0) newdf <- data.frame(x,x2,x3,check.names=FALSE)
  else if (length(x2)>0 & length(x3)==0) newdf <- data.frame(x,x2,check.names=FALSE)
  else if (length(x2)==0 & length(x3)>0) newdf <- data.frame(x,x3,check.names=FALSE)
  else newdf <- x
  
  ##names(newdf)[1] <- name
  return(newdf)
}
