visregLatticePlot <- function(fit,f,name,nn,cond,type,trans,xtrans,alpha,jitter,partial,whitespace,by,strip.names,...)
  {
    lev <- attr(cond,"lev")  
    if(is.factor(f[,name])) nn <- length(levels(f[,name]))
    lframe <- as.data.frame(matrix(0,nn*length(lev),5))
    names(lframe)<-c('xx','fit','upr','lwr','by')
    lresids <- as.data.frame(matrix(0,length(f[,name]),3))
    names(lresids)<-c('r','x','by')
    lby <- rep(f[,by],length(lev))

    for (i in 1:length(cond))
      {
        xy <- getXY(fit,f,name,nn,cond[[i]],type,trans,xtrans,alpha,jitter)
        x <- xy$x
        y <- xy$y

        lframe$xx[((i-1)*nn + 1):(i*nn)] <- x$xx
        lframe$fit[((i-1)*nn + 1):(i*nn)] <- y$fit
        lframe$upr[((i-1)*nn + 1):(i*nn)] <- y$upr
        lframe$lwr[((i-1)*nn + 1):(i*nn)] <- y$lwr
        lframe$by[((i-1)*nn + 1):(i*nn)] <- lev[i]
        if(is.numeric(f[,by])) rpoints <- (as.numeric(attr(cond,"new.by"))==i)
        else rpoints <- (as.numeric(f[,by]) == i)
        lresids$x[rpoints] <- x$x[rpoints]
        lresids$r[rpoints] <- y$r[rpoints]
        lresids$by[rpoints] <- lev[i]
      }
    lframe$by <- factor(lframe$by,levels=lev)
    lresids$by <- factor(lresids$by,levels=lev)
    if (is.factor(f[,name]))
      {
        lresids$x <- factor(levels(f[,name])[lresids$x],levels=levels(f[,name]))
        lframe$xx <- factor(levels(f[,name])[lframe$xx],levels=levels(f[,name]))
      }
    
    if (is.factor(x$x)) xlim <- c(0,1)
    else xlim <- range(x$xx)
    ylim <- range(c(lresids$r,lframe$lwr,lframe$upr))
    pad <- 0.02*diff(ylim)
    ylim[1] <- ylim[1]-pad
    ylim[2] <- ylim[2]+pad
    pad <- 0.02*diff(xlim)
    xlim[1] <- xlim[1]-pad
    xlim[2] <- xlim[2]+pad
    
    plot.args <- list(x=formula(lframe$fit~lframe$xx | lframe$by),type="l",ylim=ylim, xlab=name, ylab=as.character(formula(fit)[2]), lframe=lframe,lresids=lresids,partial=partial,xlim=xlim,strip=strip.custom(strip.names=strip.names,var.name=by))
    new.args <- list(...)
    if (length(new.args)) plot.args[names(new.args)] <- new.args
    if (is.null(dev.list()))
      {
        trellis.device()
    trellis.par.set(plot.symbol=list(pch=19,cex=0.4))
      }
    
    if(is.factor(f[,name])) 
      {
        K <- length(levels(x$x))
        len <- K*(1-whitespace)+(K-1)*whitespace
        scales <- list(x=list(at=((0:(K-1))/len+(1-whitespace)/(2*len)),labels=levels(x$x)))
        plot.args$scales <- scales
        plot.args$panel <- visregFactorPanel
        plot.args$w <- whitespace
        tp <- do.call("xyplot",plot.args)
        plot(tp)
      }
    else
      {
        plot.args$panel <- visregPanel
        tp <- do.call("xyplot",plot.args)
        plot(tp)
      }
    return(list(lframe,lresids))
  }
