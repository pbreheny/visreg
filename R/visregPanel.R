visregPanel <- function(x, y, subscripts, lframe, lresids, partial, band, rug, fill.par, ...) {
  if (band) {
    poly.args <- list(x=c(lframe$xx[subscripts],rev(lframe$xx[subscripts])), y=c(lframe$lwr[subscripts],rev(lframe$upr[subscripts])), subscripts=subscripts, col="gray85", border=F)
    if (length(fill.par)) poly.args[names(fill.par)] <- fill.par
    do.call("panel.polygon", poly.args)
  }
  current.level <- lframe$by[subscripts][1]
  if (partial) panel.points(lresids$x[lresids$by==current.level],lresids$r[lresids$by==current.level])
  panel.xyplot(x,y,subscripts=subscripts,...)
  if (rug==1) panel.rug(lresids$x[lresids$by==current.level], lwd=1)
  if (rug==2) {
    panel.rug(lresids$x[lresids$by==current.level & !lresids$pos], lwd=1)
    panel.rug(lresids$x[lresids$by==current.level & lresids$pos], regular=FALSE, lwd=1)
  }
}
