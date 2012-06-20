visregPlot <- function(fit,f,name,fill,nn,cond,type,trans,xtrans,alpha,jitter,partial,whitespace,...)
  {
    xy <- getXY(fit,f,name,fill,nn,cond,type,trans,xtrans,alpha,jitter)
    x <- xy$x
    y <- xy$y

    if (is.factor(x$x)) xlim <- c(0,1)
    else xlim <- range(x$xx)
    plot.args <- list(x=1, y=1, ylim=range(c(y$r,y$lwr,y$upr)), xlab=name, ylab=as.character(formula(fit)[2]), type="n", xlim=xlim,xaxt=ifelse(is.factor(f[,name]),'n','s'))
    new.args <- list(...)
    if (length(new.args)) plot.args[names(new.args)] <- new.args
    do.call("plot", plot.args)

    if(is.factor(f[,name])) factorPlot(x,y,partial,whitespace)
    else
      {
        polygon(c(x$xx,rev(x$xx)),c(y$lwr,rev(y$upr)),col="gray85",border=F)
        lines(x$xx,y$fit,lwd=2)
        if (partial) points(x$x,y$r,pch=19,cex=0.4)
      }
    return(xy)
  }
