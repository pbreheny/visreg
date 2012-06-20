fillFrame <- function(f,x,fill,cond)
  {
    name <- attr(x,"name")
    fillFunction <- switch(fill,mean=mean,zero=function(x){0},median=median)

    for(i in 1:length(names(f)))
      {
        if(is.factor(f[,names(f)[i]]))
          {
            mode = names(sort(-table(f[i])))[1]
            if(fill=='zero')
              {
                eval(parse(text=c('cond=c(cond,list(',names(f)[i],'=factor(levels(f[,names(f)[i]])[1],levels=levels(f[,names(f)[i]]))))')))
              }
            else
              {
                eval(parse(text=c('cond=c(cond,list(',names(f)[i],'=factor(mode,levels=levels(f[,names(f)[i]]))))')))
              }
          }
      }
    x2 <- lapply(as.data.frame(f[,setdiff(names(f),c(name,names(cond)))]),fillFunction)
    names(x2) <- setdiff(names(f),c(name,names(cond)))
    x3 <- cond
    
    if (length(x3)>0)
      {
        newdf <- data.frame(x,x2,x3,check.names=FALSE)
      }
    else if (length(x3)==0)
      {
        newdf <- data.frame(x,x2,check.names=FALSE)
      }

    names(newdf)[1] <- name
    return(newdf)
  }
