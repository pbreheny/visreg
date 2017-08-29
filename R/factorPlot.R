factorPlot <- function(v, partial, band, rug, w, line.par, fill.par, points.par, ...) {
  ## Setup
  x <- v$res[,v$meta$x]
  y <- v$res$visregRes
  K <- length(levels(x))
  len <- K*(1-w)+(K-1)*w
  xx <- v$fit[,v$meta$x]
  yy <- v$fit$visregFit
  lwr <- v$fit$visregLwr
  upr <- v$fit$visregUpr

  for(k in 1:K) {
    x1 <- (k-1)/len
    x2 <- (k-1)/len + (1-w)/len
    xx <- c(x1,x2)
    if (band) {
      fill.args <- list(x=c(xx,rev(xx)), y=c(rep(lwr[k],2),rev(rep(upr[k],2))), col="gray85", border=F)
      if (length(fill.par)) fill.args[names(fill.par)] <- fill.par
      do.call("polygon", fill.args)
    }
    line.args <- list(x=c(x1,x2), y=rep(yy[k],2), lwd=3, col="#008DFFFF")
    if (length(line.par)) line.args[names(line.par)] <- line.par
    do.call("lines", line.args)
    ind <- x==levels(x)[k]
    rx <- seq(x1,x2,len=sum(ind)+2)[c(-1,-(sum(ind)+2))]
    if (partial) {
      points.args <- list(x=rx, y=y[ind], pch=19, cex=0.4, col="gray50")
      if (length(points.par)) points.args[names(points.par)] <- points.par
      do.call("points", points.args)
    }
    do.call("lines", line.args)
    if (!all(is.na(v$res$visregPos))) {
      if (rug==1) rug(rx,col=line.args$col)
      if (rug==2) {
        browser()
        ind1 <- ind & !v$res$visregPos
        ind2 <- ind & v$res$visregPos
        rx1 <- seq(x1, x2, len=sum(ind1)+2)[c(-1,-(sum(ind1)+2))]
        rx2 <- seq(x1, x2, len=sum(ind2)+2)[c(-1,-(sum(ind2)+2))]
        rug(rx1, col=line.args$col)
        rug(rx2, side=3, col=line.args$col)
      }
    }
  }
}
