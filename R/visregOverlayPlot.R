visregOverlayPlot <- function(v, partial, band, rug, ask, whitespace, legend, line.par, fill.par, points.par, ...) {
  lev <- attr(v, "lev")
  if (ask & v[[1]]$y$n > 1 & prod(par("mfcol")) < v[[1]]$y$n & dev.interactive()) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
  for (j in 1:v[[1]]$y$n) {
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
    
    ylab <- switch(attr(v, "yNameClass"),
                   as.expression(substitute(list(Delta) * x,list(x=y$name))),
                   y$name,
                   paste("f(", x$name, ")", sep=""))

    plot.args <- list(x=1, y=1, ylim=ylim, xlab=x$name, ylab=ylab, type="n", xlim=xlim, xaxt=ifelse(x$factor,'n','s'), las=1)
    new.args <- list(...)
    if (length(new.args)) plot.args[names(new.args)] <- new.args
    do.call("plot", plot.args)
    col <- pal(length(v))
    acol <- pal(length(v), alpha=0.5)
    line.args <- list(lwd=3, col=col, lty=1)
    if (length(line.par)) line.args[names(line.par)] <- line.par
    points.args <- list(pch=19, cex=0.4, col=col)
    if (length(points.par)) points.args[names(points.par)] <- points.par
    fill.args <- list(col=acol, border=F)
    if (length(fill.par)) fill.args[names(fill.par)] <- fill.par
    
    for (i in 1:length(v)) {
      x <- v[[i]]$x
      y <- v[[i]]$y
      fun <- function(x) if (length(x)==length(v)) x[i] else x
      line.args.i <- lapply(line.args, fun)
      points.args.i <- lapply(points.args, fun)
      fill.args.i <- lapply(fill.args, fun)
      if (y$n > 1) {
        Y <- y
        y <- list(fit=Y$fit[,j], lwr=Y$lwr[,j], upr=Y$upr[,j], r=Y$r[,j])
        y$name <- colnames(Y$fit)[j]
      }
      if (x$factor) {
        ax <- if (i==1) TRUE else FALSE
        factorPlot(x, y, partial, band, rug, whitespace, line.args.i, fill.args.i, points.args.i, ax=ax)
      } else {
        if (band) {
          fill.args.i$x <- c(x$xx,rev(x$xx))
          fill.args.i$y <- c(y$lwr,rev(y$upr))
          do.call("polygon", fill.args.i)
        }
        line.args.i$x <- x$xx
        line.args.i$y <- y$fit
        do.call("lines", line.args.i)
        if (partial) {
          points.args.i$x <- x$x
          points.args.i$y <- y$r
          do.call("points", points.args.i)
        }
        if (rug==1) rug(x$x, side=1)
        if (rug==2) {
          rug(x$x[!y$pos], side=1)
          rug(x$x[y$pos], side=3)
        }
      }
    }
    toplegend(lev, col=line.args$col, lwd=line.args$lwd, lty=line.args$lty, ncol=min(length(lev), 5))
  }
}
