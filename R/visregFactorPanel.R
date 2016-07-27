visregFactorPanel <- function(x, y, w, subscripts, lframe, lresids, partial, band, rug, fill.par, ...) {
  K <- length(levels(lframe$xx))
  len <- K*(1-w)+(K-1)*w

  for(k in 1:K) {
    x1 <- (k-1)/len
    x2 <- (k-1)/len + (1-w)/len
    xx <- c(x1,x2)

    if (band) {
      poly.args <- list(x=c(xx,rev(xx)), y=c(rep(lframe$lwr[subscripts][k],2),rev(rep(lframe$upr[subscripts][k],2))), subscripts=subscripts, col="gray85", border=F)
      if (length(fill.par)) poly.args[names(fill.par)] <- fill.par
      do.call("panel.polygon", poly.args)
    }
    panel.lines(xx,rep(lframe$fit[subscripts][k],2),subscripts=subscripts,...)
    ind <- (lresids$by==lframe$by[subscripts][1]) & (lresids$x==levels(lresids$x)[k])
    rx <- seq(x1, x2, len=sum(ind)+2)[c(-1,-(sum(ind)+2))]
    if (partial) panel.points(rx, lresids$r[ind])
    if (rug==1) panel.rug(rx)
    if (rug==2) {
      ind1 <- ind & !lresids$pos
      ind2 <- ind & lresids$pos
      rx1 <- seq(x1, x2, len=sum(ind1)+2)[c(-1,-(sum(ind1)+2))]
      rx2 <- seq(x1, x2, len=sum(ind2)+2)[c(-1,-(sum(ind2)+2))]
      panel.rug(rx1)
      panel.rug(rx2, regular=FALSE)
    }
  }

  panel.xyplot(0,0,subscripts=subscripts,...)
}
