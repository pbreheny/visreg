visregOverlayPlot <- function(v, partial, band, rug, ask, whitespace, legend, strip.names, line.par, fill.par, points.par, ...) {
  ## Setup
  x <- v$res[,v$meta$x]
  y <- v$res$visregRes
  b <- v$res[,v$meta$by]
  xx <- v$fit[,v$meta$x]
  yy <- v$fit$visregFit
  bb <- v$fit[,v$meta$by]
  lev <- unique(bb)
  lwr <- v$fit$visregLwr
  upr <- v$fit$visregUpr
  xlim <- if (is.factor(xx)) c(0,1) else range(xx)
  if (partial) {
    ylim <- range(c(y, lwr, upr), na.rm=TRUE)
  } else {
    ylim <- range(c(yy, lwr, upr), na.rm=TRUE)
  }
  ylab <- if (is.null(v$meta$yName)) paste("f(", v$meta$x, ")", sep="") else v$meta$yName
  plot.args <- list(x=1, y=1, ylim=ylim, xlab=v$meta$x, ylab=ylab, type="n", xlim=xlim, xaxt=ifelse(is.factor(xx),'n','s'), las=1)
  new.args <- list(...)
  if (length(new.args)) plot.args[names(new.args)] <- new.args
  do.call("plot", plot.args)
  col <- pal(length(lev))
  acol <- pal(length(lev), alpha=0.5)
  line.args <- list(lwd=3, col=col, lty=1)
  if (length(line.par)) line.args[names(line.par)] <- line.par
  points.args <- list(pch=19, cex=0.4, col=col)
  if (length(points.par)) points.args[names(points.par)] <- points.par
  fill.args <- list(col=acol, border=F)
  if (length(fill.par)) fill.args[names(fill.par)] <- fill.par

  for (i in 1:length(lev)) {
    current.level <- lev[i]
    indfit <- v$fit[,v$meta$by]==current.level
    indres <- v$res[,v$meta$by]==current.level
    fun <- function(x) if (length(x)==length(lev)) x[i] else x
    line.args.i <- lapply(line.args, fun)
    points.args.i <- lapply(points.args, fun)
    fill.args.i <- lapply(fill.args, fun)
    if (is.factor(x)) {
      ax <- if (i==1) TRUE else FALSE
      if (("xaxt" %in% names(new.args)) && new.args$xaxt=="n") ax <- FALSE
      v.i <- v
      v.i$fit <- subset(v.i$fit, indfit)
      v.i$res <- subset(v.i$res, indres)
      factorPlot(v.i, partial, band, rug, whitespace, line.args.i, fill.args.i, points.args.i, ax=ax)
    } else {
      if (band) {
        fill.args.i$x <- c(xx[indfit],rev(xx[indfit]))
        fill.args.i$y <- c(lwr[indfit],rev(upr[indfit]))
        do.call("polygon", fill.args.i)
      }
      line.args.i$x <- xx[indfit]
      line.args.i$y <- yy[indfit]
      do.call("lines", line.args.i)
      if (partial) {
        points.args.i$x <- x[indres]
        points.args.i$y <- y[indres]
        do.call("points", points.args.i)
      }
      if (rug==1) rug(x[indres], side=1, col=line.args.i$col)
      if (rug==2) {
        rug(x[indres][!v$res$visregPos[indres]], side=1, col=line.args.i$col)
        rug(x[indres][v$res$visregPos[indres]], side=3, col=line.args.i$col)
      }
    }
  }
  if (legend) {
    if (identical(strip.names, FALSE)) {
      lgtext <- lev
    } else if (identical(strip.names, TRUE)) {
      lgtext <- paste(v$meta$by, lev, sep=" : ")
    } else {
      lgtext <- strip.names
    }
    toplegend(lgtext, col=line.args$col, lwd=line.args$lwd, lty=line.args$lty, ncol=min(length(lev), 5))
  }
}
