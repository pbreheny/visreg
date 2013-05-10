factorPlot <- function(x, y, partial, band, w, line.par, fill.par, points.par, ax=TRUE) {
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
    line.args <- list(x=c(x1,x2), y=rep(y$fit[k],2), lwd=2)
    if (length(line.par)) line.args[names(line.par)] <- line.par
    do.call("lines", line.args)
    if (partial) {
      ind <- which(x$x==levels(x$x)[k])
      rx <- seq(x1,x2,len=length(ind)+2)[c(-1,-(length(ind)+2))]
      points.args <- list(x=rx, y=y$r[ind], pch=19, cex=0.4)
      if (length(points.par)) points.args[names(points.par)] <- points.par
      do.call("points", points.args)
    }
  }
  if (ax) axis(side=1,at=((0:(K-1))/len+(1-w)/(2*len)),labels=levels(x$x))
}
