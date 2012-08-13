visregPanel <- function(x, y, subscripts, lframe, lresids, partial, fill.par, ...)
{
  poly.args <- list(x=c(lframe$xx[subscripts],rev(lframe$xx[subscripts])), y=c(lframe$lwr[subscripts],rev(lframe$upr[subscripts])), subscripts=subscripts, col="gray85", border=F)
  if (length(fill.par)) poly.args[names(fill.par)] <- fill.par
  do.call("panel.polygon", poly.args)
  panel.xyplot(x,y,subscripts=subscripts,...)
  if(partial)
  {
    plotpoints <- (as.vector(rep(lframe$by[subscripts][1],length(lresids$by))) == as.vector(lresids$by))
    panel.points(lresids$x[plotpoints],lresids$r[plotpoints])
  }
}
