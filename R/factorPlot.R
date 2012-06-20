factorPlot <- function(x,y,partial,w)
  {
    K <- length(levels(x$x))
    len <- K*(1-w)+(K-1)*w
    for(k in 1:K)
      {
        x1 <- (k-1)/len
        x2 <- (k-1)/len + (1-w)/len
        xx <- c(x1,x2)
        polygon(c(xx,rev(xx)),c(rep(y$lwr[k],2),rev(rep(y$upr[k],2))),col="gray85",border=F)
        lines(c(x1,x2),rep(y$fit[k],2),lwd=2)
        if (partial)
          {
            ind <- which(x$x==levels(x$x)[k])
            rx <- seq(x1,x2,len=length(ind)+2)[c(-1,-(length(ind)+2))]
            points(rx,y$r[ind],pch=19,cex=0.4)
          }
      }
    axis(side=1,at=((0:(K-1))/len+(1-w)/(2*len)),labels=levels(x$x))
  }
