visregPlot <- function(v, partial, rug, band, whitespace, line.par, fill.par, points.par, ...) {
  ## Setup
  x <- v$res[,v$meta$x]
  y <- v$res$visregRes
  xx <- v$fit[,v$meta$x]
  yy <- v$fit$visregFit
  lwr <- v$fit$visregLwr
  upr <- v$fit$visregUpr
  xlim <- if (is.factor(xx)) c(0,1) else range(xx)
  ylab <- if (is.null(v$meta$yName)) paste("f(", v$meta$x, ")", sep="") else v$meta$yName
  if (partial && sum(!is.na(y))>0) {
    ylim <- range(c(y, lwr, upr), na.rm=TRUE)
  } else if (band) {
    ylim <- range(c(yy, lwr, upr), na.rm=TRUE)
  } else {
    ylim <- range(yy)
  }
  if (partial && sum(!is.na(y))==0) warning(paste0("The generic function residuals() is not set up for this type of model object.  To plot partial residuals, you will need to define your own residuals.", v$meta$class[1], " function."))
  plot.args <- list(x=1, y=1, ylim=ylim, xlab=v$meta$x, ylab=ylab, type="n", xlim=xlim, xaxt=ifelse(is.factor(xx),'n','s'), las=1)
  new.args <- list(...)
  if (length(new.args)) plot.args[names(new.args)] <- new.args
  do.call("plot", plot.args)

  if (is.factor(xx)) {
    factorPlot(v, partial, band, rug, whitespace, line.par, fill.par, points.par, ...)
  } else {
    if (band) {
      fill.args <- list(x=c(xx,rev(xx)), y=c(lwr,rev(upr)), col="gray85", border=F)
      if (length(fill.par)) fill.args[names(fill.par)] <- fill.par
      do.call("polygon", fill.args)
    }
    line.args <- list(x=xx, y=yy, lwd=3, lty=1, col="#008DFFFF")
    if (length(line.par)) line.args[names(line.par)] <- line.par
    do.call("lines", line.args)
    if (partial) {
      points.args <- list(x=x, y=y, pch=19, cex=0.4, col="gray50")
      if (length(points.par)) points.args[names(points.par)] <- points.par
      do.call("points", points.args)
    }
    if (rug==1) rug(x, side=1)
    if (rug==2) {
      rug(x[!v$res$visregPos], side=1)
      rug(x[v$res$visregPos], side=3)
    }
  }
}
