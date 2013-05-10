visregLatticePlot <- function(v, partial, band, whitespace, strip.names, line.par, fill.par, points.par, ...) {
  lev <- attr(v, "lev")
  for (j in 1:v[[1]]$y$n) {
    if (j > 1 & interactive()) readline(prompt="Hit <Return> to see next plot:")
    lframe <- lresids <- NULL
    for (i in 1:length(v)) {
      x <- v[[i]]$x
      y <- v[[i]]$y
      if (y$n > 1) {
        Y <- y
        y <- list(fit=Y$fit[,j], lwr=Y$lwr[,j], upr=Y$upr[,j], r=Y$r[,j])
        y$name <- colnames(Y$fit)[j]
      }
      lframe.i <- data.frame(xx=x$xx, fit=y$fit, upr=y$upr, lwr=y$lwr, by=lev[i])
      lframe <- rbind(lframe, lframe.i)
      lresids.i <- data.frame(x=x$x, r=y$r, by=lev[i])
      lresids <- rbind(lresids, lresids.i)
    }
    lframe$by <- factor(lframe$by, levels=lev)
    lresids$by <- factor(lresids$by, levels=lev)
    if (x$factor) {
      if (partial) lresids$x <- factor(lresids$x, levels=levels(x$x))
      lframe$xx <- factor(lframe$xx, levels=levels(x$x))
    }
    
    if (x$factor) xlim <- c(0,1) else xlim <- range(x$xx)
    if (partial) {
      ylim <- range(c(lresids$r, lframe$lwr, lframe$upr))
    } else ylim <- range(c(lframe$lwr, lframe$upr))
    pad <- 0.04*diff(ylim)
    ylim[1] <- ylim[1]-pad
    ylim[2] <- ylim[2]+pad
    pad <- 0.04*diff(xlim)
    xlim[1] <- xlim[1]-pad
    xlim[2] <- xlim[2]+pad      
    ylab <- switch(attr(v, "yNameClass"),
                   as.expression(substitute(list(Delta) * x,list(x=y$name))),
                   y$name,
                   paste("f(", x$name, ")", sep=""))
    if (!partial) lresids=NULL
    plot.args <- list(x=formula(lframe$fit~lframe$xx | lframe$by), type="l", ylim=ylim, xlab=x$name, ylab=ylab, lframe=lframe, lresids=lresids, partial=partial, band=band, xlim=xlim, strip=strip.custom(strip.names=strip.names, var.name=attr(v, "by")), fill.par=fill.par)
    new.args <- list(...)
    if (length(new.args)) plot.args[names(new.args)] <- new.args
    if (is.null(dev.list())) trellis.device()
    opar <- trellis.par.get()
    if (length(line.par)) trellis.par.set(plot.line=line.par)
    plot.symbol <- list(pch=19)
    if (length(points.par)) plot.symbol[names(points.par)] <- points.par
    trellis.par.set(plot.symbol=plot.symbol)
    
    if (x$factor) {
      K <- length(levels(x$x))
      len <- K*(1-whitespace)+(K-1)*whitespace
      scales <- list(x=list(at=((0:(K-1))/len+(1-whitespace)/(2*len)),labels=levels(x$x)))
      plot.args$scales <- scales
      plot.args$panel <- visregFactorPanel
      plot.args$w <- whitespace
      tp <- do.call("xyplot", plot.args)
      plot(tp)
    } else {
      plot.args$panel <- visregPanel
      plot.args$newpage <- TRUE
      tp <- do.call("xyplot",plot.args)
      plot(tp)
    }
  }
  trellis.par.set(opar)
}
