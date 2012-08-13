visregFactorPanel <- function(x, y, w, subscripts, lframe, lresids, partial, fill.par, ...)
{
  K <- length(levels(lframe$xx))
  len <- K*(1-w)+(K-1)*w
  
  for(k in 1:K) {
    x1 <- (k-1)/len
    x2 <- (k-1)/len + (1-w)/len
    xx <- c(x1,x2)
    
    poly.args <- list(x=c(xx,rev(xx)), y=c(rep(lframe$lwr[subscripts][k],2),rev(rep(lframe$upr[subscripts][k],2))), subscripts=subscripts, col="gray85", border=F)
    if (length(fill.par)) poly.args[names(fill.par)] <- fill.par
    do.call("panel.polygon", poly.args)
    panel.lines(xx,rep(lframe$fit[subscripts][k],2),subscripts=subscripts,...)
    if (partial) {
      ind <- intersect(which(lresids$by==lframe$by[subscripts][1]),which(lresids$x==levels(lresids$x)[k]))
      rx <- seq(x1,x2,len=length(ind)+2)[c(-1,-(length(ind)+2))]
      panel.points(rx,lresids$r[ind])
    }
  }
  
  panel.xyplot(0,0,subscripts=subscripts,...)
}
