factorPlot <- function(x, y, partial, band, rug, w, line.par, fill.par, points.par, ax=TRUE) {
  K <- length(levels(x$x))
  len <- K*(1-w)+(K-1)*w
  for(k in 1:K) {
    x1 <- (k-1)/len
    x2 <- (k-1)/len + (1-w)/len
    xx <- c(x1,x2)
    if (band) {
      fill.args <- list(x=c(xx,rev(xx)), y=c(rep(y$lwr[k],2),rev(rep(y$upr[k],2))), col="gray85", border=F)
      if (length(fill.par)) fill.args[names(fill.par)] <- fill.par
      do.call("polygon", fill.args)      
    }
    ind <- x$x==levels(x$x)[k]
    rx <- seq(x1,x2,len=sum(ind)+2)[c(-1,-(sum(ind)+2))]
    if (partial) {
      points.args <- list(x=rx, y=y$r[ind], pch=19, cex=0.4, col="gray50")
      if (length(points.par)) points.args[names(points.par)] <- points.par
      do.call("points", points.args)
    }
    line.args <- list(x=c(x1,x2), y=rep(y$fit[k],2), lwd=3, col="#008DFFFF")
    if (length(line.par)) line.args[names(line.par)] <- line.par
    do.call("lines", line.args)
    if (rug==1) rug(rx)
    if (rug==2) {
      ind1 <- ind & !y$pos
      ind2 <- ind & y$pos
      rx1 <- seq(x1, x2, len=sum(ind1)+2)[c(-1,-(sum(ind1)+2))]
      rx2 <- seq(x1, x2, len=sum(ind2)+2)[c(-1,-(sum(ind2)+2))]
      rug(rx1)
      rug(rx2, side=3)
    }
  }
  if (ax) axis(side=1,at=(0:(K-1))/len+(1-w)/(2*len),labels=levels(x$x))
}
