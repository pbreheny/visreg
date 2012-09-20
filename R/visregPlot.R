visregPlot <- function(fit, f, name, nn, cond, type, trans, xtrans, alpha, jitter, partial, whitespace, line.par, fill.par, points.par, ...)
{
  xy <- getXY(fit,f,name,nn,cond,type,trans,xtrans,alpha,jitter)
  x <- xy$x
  y <- xy$y
  
  if (class(fit)[1] == "mlm") {
    Y <- y
    n.outcomes <- ncol(Y$fit)
  } else n.outcomes <- 1
  
  for (j in 1:n.outcomes) {
    if (class(fit)[1] == "mlm") {
      y <- list(fit=Y$fit[,j], lwr=Y$lwr[,j], upr=Y$upr[,j], r=Y$r[,j])
      yname <- colnames(Y$fit)[j]
    } else yname <- as.character(formula(fit)[2])
    
    if (is.factor(x$x)) xlim <- c(0,1)
    else xlim <- range(x$xx)
    if (type=="effect") ylab <- as.expression(substitute(list(Delta) * x,list(x=yname)))
    else ylab <- yname
    if (partial) ylim <- range(c(y$r,y$lwr,y$upr))
    else ylim <- range(c(y$lwr,y$upr))
    plot.args <- list(x=1, y=1, ylim=ylim, xlab=name, ylab=ylab, type="n", xlim=xlim,xaxt=ifelse(is.factor(f[,name]),'n','s'),las=1)
    new.args <- list(...)
    if (length(new.args)) plot.args[names(new.args)] <- new.args
    do.call("plot", plot.args)
    
    if(is.factor(f[,name])) factorPlot(x,y,partial,whitespace, line.par, fill.par, points.par)
    else
    {
      fill.args <- list(x=c(x$xx,rev(x$xx)), y=c(y$lwr,rev(y$upr)), col="gray85", border=F)
      if (length(fill.par)) fill.args[names(fill.par)] <- fill.par
      do.call("polygon", fill.args)
      line.args <- list(x=x$xx, y=y$fit, lwd=2)
      if (length(line.par)) line.args[names(line.par)] <- line.par
      do.call("lines", line.args)
      if (partial) {
        points.args <- list(x=x$x, y=y$r, pch=19, cex=0.4)
        if (length(points.par)) points.args[names(points.par)] <- points.par
        do.call("points", points.args)
      }
    }
  }
  return(xy)
}