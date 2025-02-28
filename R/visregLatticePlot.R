visregLatticePlot <- function(v, partial, band, rug, whitespace, strip.names, top, line.par, fill.par, points.par, ...) {
  ## Setup
  x <- v$res[, v$meta$x]
  y <- v$res$visregRes
  b <- v$res[, v$meta$by]
  xx <- v$fit[, v$meta$x]
  yy <- v$fit$visregFit
  bb <- v$fit[, v$meta$by]
  lwr <- v$fit$visregLwr
  upr <- v$fit$visregUpr

  if (is.factor(bb)) {
    b <- droplevels(b)
    bb <- droplevels(bb)
  }

  xlim <- if (is.factor(xx)) c(0, 1) else range(xx)
  if (partial) {
    ylim <- range(c(y, lwr, upr), na.rm=TRUE)
  } else {
    ylim <- range(c(yy, lwr, upr), na.rm=TRUE)
  }
  pad <- 0.05*diff(ylim)
  ylim[1] <- ylim[1]-pad
  ylim[2] <- ylim[2]+pad
  pad <- 0.04*diff(xlim)
  xlim[1] <- xlim[1]-pad
  xlim[2] <- xlim[2]+pad
  ylab <- if (is.null(v$meta$yName)) paste("f(", v$meta$x, ")", sep="") else v$meta$yName
  new.args <- list(...)
  if (identical(strip.names, FALSE)) {
    strip <- lattice::strip.custom(strip.names=FALSE, factor.levels=levels(as.factor(bb)), strip.levels=c(TRUE, TRUE), fg=lattice::trellis.par.get("strip.background")$col)
  } else if (identical(strip.names, TRUE)) {
    if (is.factor(v$fit[, v$meta$by])) {
      strip <- lattice::strip.custom(strip.names=TRUE, strip.levels=c(TRUE, TRUE), var.name=v$meta$by)
    } else {
      strip <- lattice::strip.custom(strip.names=FALSE, factor.levels=paste(v$meta$by, abbrNum(bb), sep=": "), strip.levels=c(TRUE, TRUE), fg=lattice::trellis.par.get("strip.background")$col)
    }
  } else {
    strip <- lattice::strip.custom(strip.names=FALSE, factor.levels=strip.names, strip.levels=c(TRUE, TRUE), fg=lattice::trellis.par.get("strip.background")$col)
  }
  lframe <- data.frame(fit=yy, lwr=lwr, upr=upr, xx=xx, by=bb)
  lresids <- data.frame(r=y, x=x, by=b, pos=v$res$visregPos)
  plot.args <- list(x=formula(lframe$fit~lframe$xx | lframe$by), type="l", ylim=ylim, xlab=v$meta$x, ylab=ylab, lframe=lframe, lresids=lresids, partial=partial, band=band, rug=rug, xlim=xlim, strip=strip, top=top, fill.par=fill.par)
  if (length(new.args)) plot.args[names(new.args)] <- new.args
  if (is.null(dev.list())) lattice::trellis.device()
  opar <- lattice::trellis.par.get()

  # Plot
  line.args <- list(lwd=3, col="#008DFFFF")
  if (length(line.par)) line.args[names(line.par)] <- line.par
  lattice::trellis.par.set(plot.line=line.args)
  points.args <- list(cex=0.4, pch=19, col="gray50")
  if (length(points.par)) points.args[names(points.par)] <- points.par
  lattice::trellis.par.set(plot.symbol=points.args)
  FUN <- getFromNamespace('xyplot', 'lattice')
  if (is.factor(x)) {
    K <- length(levels(x))
    len <- K*(1-whitespace)+(K-1)*whitespace
    scales <- list(x=list(at=((0:(K-1))/len+(1-whitespace)/(2*len)), labels=levels(x)))
    if (is.null(plot.args$scales)) {
      plot.args$scales <- scales
    } else if (is.null(plot.args$scales$x)) {
      plot.args$scales$x <- scales$x
    } else {
      plot.args$scales$x <- c(plot.args$scales$x, scales)
    }
    plot.args$panel <- visregFactorPanel
    plot.args$w <- whitespace
    tp <- do.call(FUN, plot.args)
    plot(tp)
  } else {
    plot.args$panel <- visregPanel
    tp <- do.call(FUN, plot.args)
    plot(tp)
  }
  lattice::trellis.par.set(opar)
  return(tp)
}
