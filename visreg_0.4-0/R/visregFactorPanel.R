visregFactorPanel <- function(x,y,w,subscripts,lframe,lresids,partial,...)
  {
    K <- length(levels(lresids$x))
    len <- K*(1-w)+(K-1)*w
    for(k in 1:K)
      {
        x1 <- (k-1)/len
        x2 <- (k-1)/len + (1-w)/len
        xx <- c(x1,x2)
        lpolygon(c(xx,rev(xx)),c(rep(lframe$lwr[subscripts][k],2),rev(rep(lframe$upr[subscripts][k],2))),col="gray85",border=F, subscripts=subscripts,...)
        llines(xx,rep(lframe$fit[subscripts][k],2),lwd=2,subscripts=subscripts,...)
        if (partial)
          {
            ind <- intersect(which(lresids$by==lframe$by[subscripts][1]),which(lresids$x==levels(lresids$x)[k]))
            rx <- seq(x1,x2,len=length(ind)+2)[c(-1,-(length(ind)+2))]
            lpoints(rx,lresids$r[ind],pch=19,cex=0.4)
          }
      }
    panel.xyplot(0,0,subscripts=subscripts,...)
  }
