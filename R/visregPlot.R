visregPlot <- function(v, partial, band, rug, ask, whitespace, print.cond, line.par, fill.par, points.par, ...) {
  n.plots <- length(v) * v[[1]]$y$n
  if (ask & (n.plots > 1) & (prod(par("mfcol")) < n.plots) && dev.interactive()) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
  
  for (i in 1:length(v)) {
    x <- v[[i]]$x
    y <- v[[i]]$y
    if (y$n > 1) Y <- y
    
    for (j in 1:y$n) {
      if (y$n > 1) {
        y <- list(fit=Y$fit[,j], lwr=Y$lwr[,j], upr=Y$upr[,j], r=Y$r[,j])
        y$name <- colnames(Y$fit)[j]
      }
      
      xlim <- if (x$factor) c(0,1) else range(x$xx)
      ylab <- switch(attr(v, "yNameClass"),
                     as.expression(substitute(list(Delta) * x,list(x=y$name))),
                     y$name,
                     paste("f(", x$name, ")", sep=""))
      if (partial) ylim <- range(c(y$r,y$lwr,y$upr), na.rm=TRUE)
      else ylim <- range(c(y$lwr,y$upr), na.rm=TRUE)
      plot.args <- list(x=1, y=1, ylim=ylim, xlab=x$name, ylab=ylab, type="n", xlim=xlim, xaxt=ifelse(x$factor,'n','s'), las=1)
      new.args <- list(...)
      if (length(new.args)) plot.args[names(new.args)] <- new.args
      do.call("plot", plot.args)
      
      if (x$factor) {
        factorPlot(x, y, partial, band, rug, whitespace, line.par, fill.par, points.par)
      } else {
        if (band) {
          fill.args <- list(x=c(x$xx,rev(x$xx)), y=c(y$lwr,rev(y$upr)), col="gray85", border=F)
          if (length(fill.par)) fill.args[names(fill.par)] <- fill.par
          do.call("polygon", fill.args)          
        }
        line.args <- list(x=x$xx, y=y$fit, lwd=2)
        if (length(line.par)) line.args[names(line.par)] <- line.par
        do.call("lines", line.args)
        if (partial) {
          points.args <- list(x=x$x, y=y$r, pch=19, cex=0.4)
          if (length(points.par)) points.args[names(points.par)] <- points.par
          do.call("points", points.args)
        }
        if (rug==1) rug(x$x, side=1)
        if (rug==2) {
          rug(x$x[!y$pos], side=1)
          rug(x$x[y$pos], side=3)
        }
      }
    }
    if (print.cond) printCond(list(v[[i]]), attr(v, "hasInteraction"))
  }
}
