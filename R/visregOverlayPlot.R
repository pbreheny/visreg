visregOverlayPlot <- function(v, partial, band, rug, ask, whitespace, legend, strip.names, line.par, fill.par, points.par, ...) {
  # Setup
  x <- v$res[, v$meta$x]
  y <- v$res$visregRes
  b <- v$res[, v$meta$by]
  xx <- v$fit[, v$meta$x]
  yy <- v$fit$visregFit
  bb <- v$fit[, v$meta$by]
  lev <- unique(bb)
  lwr <- v$fit$visregLwr
  upr <- v$fit$visregUpr
  xlim <- if (is.factor(xx)) c(0, 1) else range(xx)
  if (partial) {
    ylim <- range(c(y, lwr, upr), na.rm=TRUE)
  } else {
    ylim <- range(c(yy, lwr, upr), na.rm=TRUE)
  }
  ylab <- if (is.null(v$meta$yName)) paste("f(", v$meta$x, ")", sep="") else v$meta$yName

  # Empty plot
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
  fun <- function(x, i) if (length(x)==length(lev)) x[i] else x
  if (is.factor(x) && !("xaxt" %in% names(new.args) && new.args$xaxt=="n")) factorAxis(x, whitespace, new.args)

  # Add bands
  if (band) {
    for (i in 1:length(lev)) {
      indfit <- v$fit[, v$meta$by] == lev[i]
      fill.args.i <- lapply(fill.args, fun, i)
      if (is.factor(x)) {
        v.i <- v
        v.i$fit <- subset(v.i$fit, indfit)
        fp_bands(v.i, whitespace, fill.args.i)
      } else {
        fill.args.i$x <- c(xx[indfit], rev(xx[indfit]))
        fill.args.i$y <- c(lwr[indfit], rev(upr[indfit]))
        do.call("polygon", fill.args.i)
      }
    }
  }
  
  # Add points
  if (partial) {
    for (i in 1:length(lev)) {
      indres <- v$res[, v$meta$by] == lev[i]
      points.args.i <- lapply(points.args, fun, i)
      if (is.factor(x)) {
        v.i <- v
        v.i$res <- subset(v.i$res, indres)
        fp_points(v.i, whitespace, points.args.i)
      } else {
        points.args.i$x <- x[indres]
        points.args.i$y <- y[indres]
        do.call("points", points.args.i)
      }
    }
  }

  # Add lines and rugs
  for (i in 1:length(lev)) {
    line.args.i <- lapply(line.args, fun, i)
    if (is.factor(x)) {
      indfit <- v$fit[, v$meta$by] == lev[i]
      v.i <- v
      v.i$fit <- subset(v.i$fit, indfit)
      fp_lines(v.i, whitespace, line.args.i)
      fp_rug(v.i, whitespace, rug, line.args.i)
    } else {
      indfit <- v$fit[, v$meta$by] == lev[i]
      indres <- v$res[, v$meta$by] == lev[i]
      line.args.i$x <- xx[indfit]
      line.args.i$y <- yy[indfit]
      do.call("lines", line.args.i)
      if (rug==1) rug(x[indres], side=1, col=line.args.i$col)
      if (rug==2) {
        rug(x[indres][!v$res$visregPos[indres]], side=1, col=line.args.i$col)
        rug(x[indres][v$res$visregPos[indres]], side=3, col=line.args.i$col)
      }
    }
  }

  # Add legend
  if (legend) {
    if (identical(strip.names, FALSE)) {
      if (is.double(lev)) {
        lgtext <- round(lev, 3)
      } else {
        lgtext <- lev
      }
    } else if (identical(strip.names, TRUE)) {
      if (is.double(lev)) {
        lgtext <- paste(v$meta$by, round(lev, 3), sep=" : ")
      } else {
        lgtext <- paste(v$meta$by, lev, sep=" : ")
      }
    } else {
      lgtext <- strip.names
    }
    toplegend(lgtext, col=line.args$col, lwd=line.args$lwd, lty=line.args$lty, ncol=min(length(lev), 5))
  }
}
