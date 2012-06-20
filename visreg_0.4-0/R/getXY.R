getXY <- function(fit,f,name,fill,nn,cond,type,trans,xtrans,alpha,jitter)
  {
    if (type=="conditional")
      {
        x <- setupD(fit,f,name,fill,nn,cond)
        y <- Response(fit,x,trans,alpha)
      }
    else if (type=="effect")
      {
        x <- setupX(fit,f,name,fill,nn,cond)
        y <- Terms(fit,f,x,trans,alpha)
      }
    if (jitter) x$x <- jitter(x$x)
    if (!missing(xtrans))
      {
        x$xx <- xtrans(x$xx)
        x$x <- xtrans(x$x)
      }
    return(list(x=x,y=y))
  }
