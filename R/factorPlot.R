factorPlot <- function(v, partial, band, rug, w, top, line.par, fill.par, points.par, ...) {
  if (band) fp_bands(v, w, fill.par)
  if (!partial) {
    fp_lines(v, w, line.par)
  } else {
    if (top=='line') {
      fp_points(v, w, points.par)
      fp_lines(v, w, line.par)
    } else {
      fp_lines(v, w, line.par)
      fp_points(v, w, points.par)
    }
  }
  fp_rug(v, w, rug, line.par)
}

fp_lines <- function(v, w, line.par) {
  xx <- v$fit[, v$meta$x]
  K <- length(levels(xx))
  len <- K*(1-w)+(K-1)*w
  yy <- v$fit$visregFit
  for(k in 1:K) {
    x1 <- (k-1)/len
    x2 <- (k-1)/len + (1-w)/len
    xx <- c(x1, x2)
    line.args <- list(x=c(x1, x2), y=rep(yy[k], 2), lwd=3, col="#008DFFFF")
    if (length(line.par)) line.args[names(line.par)] <- line.par
    do.call("lines", line.args)
  }
}

fp_bands <- function(v, w, fill.par) {
  xx <- v$fit[, v$meta$x]
  K <- length(levels(xx))
  len <- K*(1-w)+(K-1)*w
  lwr <- v$fit$visregLwr
  upr <- v$fit$visregUpr
  for(k in 1:K) {
    x1 <- (k-1)/len
    x2 <- (k-1)/len + (1-w)/len
    xx <- c(x1, x2)
    fill.args <- list(x=c(xx, rev(xx)), y=c(rep(lwr[k], 2), rev(rep(upr[k], 2))), col="gray85", border=F)
    if (length(fill.par)) fill.args[names(fill.par)] <- fill.par
    do.call("polygon", fill.args)
  }
}

fp_points <- function(v, w, points.par) {
  x <- v$res[, v$meta$x]
  y <- v$res$visregRes
  K <- length(levels(x))
  len <- K*(1-w)+(K-1)*w
  for(k in 1:K) {
    x1 <- (k-1)/len
    x2 <- (k-1)/len + (1-w)/len
    ind <- x==levels(x)[k]
    rx <- seq(x1, x2, len=sum(ind)+2)[c(-1, -(sum(ind)+2))]
    points.args <- list(x=rx, y=y[ind], pch=19, cex=0.4, col="gray50")
    if (length(points.par)) points.args[names(points.par)] <- points.par
    do.call("points", points.args)
  }
}

fp_rug <- function(v, w, rug, line.args) {
  x <- v$res[, v$meta$x]
  y <- v$res$visregRes
  K <- length(levels(x))
  len <- K*(1-w)+(K-1)*w
  for(k in 1:K) {
    x1 <- (k-1)/len
    x2 <- (k-1)/len + (1-w)/len
    ind <- x==levels(x)[k]
    rx <- seq(x1, x2, len=sum(ind)+2)[c(-1, -(sum(ind)+2))]
    if (!all(is.na(v$res$visregPos))) {
      if (rug==1) rug(rx, col=line.args$col)
      if (rug==2) {
        ind1 <- ind & !v$res$visregPos
        ind2 <- ind & v$res$visregPos
        rx1 <- seq(x1, x2, len=sum(ind1)+2)[c(-1,-(sum(ind1)+2))]
        rx2 <- seq(x1, x2, len=sum(ind2)+2)[c(-1,-(sum(ind2)+2))]
        rug(rx1, col=line.args$col)
        rug(rx2, side=3, col=line.args$col)
      }
    }
  }
}
