visregPlot <- function(v, partial, rug, band, whitespace, line.par, fill.par, points.par, ...) {
  ## Setup
  x <- v$res[,v$meta$x]
  y <- v$res$visregRes
  xx <- v$fit[,v$meta$x]
  yy <- v$fit$visregFit
  lwr <- v$fit$visregLwr
  upr <- v$fit$visregUpr
  xlim <- if (is.factor(xx)) c(0,1) else range(xx)
  ylab <- switch(v$meta$yNameClass,
                 as.expression(substitute(list(Delta) * x, list(x=v$meta$y))),
                 v$meta$y,
                 paste("f(", v$meta$x, ")", sep=""))
  ylim <- if (partial) range(c(y, lwr, upr), na.rm=TRUE) else range(c(lwr, upr), na.rm=TRUE)
  plot.args <- list(x=1, y=1, ylim=ylim, xlab=v$meta$x, ylab=ylab, type="n", xlim=xlim, xaxt=ifelse(is.factor(xx),'n','s'), las=1)
  new.args <- list(...)
  if (length(new.args)) plot.args[names(new.args)] <- new.args
  do.call("plot", plot.args)

  if (is.factor(xx)) {
    ax <- TRUE
    if (("xaxt" %in% names(new.args)) && new.args$xaxt=="n") ax <- FALSE
    factorPlot(v, partial, band, rug, whitespace, line.par, fill.par, points.par, ax)
  } else {
    if (band) {
      fill.args <- list(x=c(xx,rev(xx)), y=c(lwr,rev(upr)), col="gray85", border=F)
      if (length(fill.par)) fill.args[names(fill.par)] <- fill.par
      do.call("polygon", fill.args)          
    }
    if (partial) {
      points.args <- list(x=x, y=y, pch=19, cex=0.4, col="gray50")
      if (length(points.par)) points.args[names(points.par)] <- points.par
      do.call("points", points.args)
    }
    line.args <- list(x=xx, y=yy, lwd=3, lty=1, col="#008DFFFF")
    if (length(line.par)) line.args[names(line.par)] <- line.par
    do.call("lines", line.args)
    if (rug==1) rug(x, side=1)
    if (rug==2) {
      rug(x[!v$res$visregPos], side=1)
      rug(x[v$res$visregPos], side=3)
    }
  }
}
