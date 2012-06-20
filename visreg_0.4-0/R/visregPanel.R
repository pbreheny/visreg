visregPanel <- function(x,y,subscripts,lframe,lresids,partial,...)
  {
    lpolygon(c(lframe$xx[subscripts],rev(lframe$xx[subscripts])), c(lframe$lwr[subscripts],rev(lframe$upr[subscripts])),col="grey85", border=F, subscripts=subscripts,...)
    panel.xyplot(x,y,subscripts=subscripts,...)
    if(partial)
      {
        plotpoints <- (as.vector(rep(lframe$by[subscripts][1],length(lresids$by))) == as.vector(lresids$by))
        lpoints(lresids$x[plotpoints],lresids$r[plotpoints])
      }
  }
