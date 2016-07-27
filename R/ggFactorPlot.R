ggFactorPlot <- function(v, partial, band, rug, w, strip.names, line.par, fill.par, points.par, ...) {
  b <- unique(v$fit[,v$meta$by])
  J <- length(b)
  K <- length(levels(v$fit[,v$meta$x]))
  len <- K*(1-w)+(K-1)*w

  # Initialize
  fillData <- lineData <- pointData <- NULL
  df0 <- rbind(v$fit, v$fit)
  df0$x <- rep(0:1, each=nrow(v$fit))
  df0$y <- rep(v$fit$visregFit, 2)
  p <- qplot(x, y, data=df0, geom="blank")

  # Plot bands and lines
  if (band) {
    for(k in 1:K) {
      x1 <- (k-1)/len
      x2 <- (k-1)/len + (1-w)/len
      xx <- c(x1, x2, x2, x1)
      fillData <- data.frame(x = rep(xx, J),
                             y = as.numeric(apply(v$fit[(1:J-1)*J + k, c("visregLwr", "visregUpr")], 1, function(x) rep(x, each=2))),
                             z = rep(b, each=4))
      names(fillData)[3] <- v$meta$by
      p <- p + geom_polygon(fill="gray", data=fillData)
    }
  }
  for(k in 1:K) {
    x1 <- (k-1)/len
    x2 <- (k-1)/len + (1-w)/len
    xx <- c(x1,x2)

    lineData <- data.frame(x = rep(xx, J),
                           y = rep(v$fit$visregFit[(1:J-1)*J + k], each=2),
                           z = rep(b, each=2))
    names(lineData)[3] <- v$meta$by
    p <- p + geom_line(data=lineData, col="#008DFFFF", size=2)
  }
  if (partial) {
    for (j in 1:J) {
      for (k in 1:K) {
        x1 <- (k-1)/len
        x2 <- (k-1)/len + (1-w)/len
        xx <- c(x1,x2)
        x <- v$res[,v$meta$x]
        z <- v$res[,v$meta$by]
        ind <- (x==levels(x)[k]) & (z==b[j])
        rx <- seq(x1, x2, len=sum(ind)+2)[c(-1,-(sum(ind)+2))]
        df <- data.frame(x = rx,
                         y = v$res$visregRes[ind],
                         z = b[j])
        pointData <- rbind(pointData, df)
      }
    }
    names(pointData)[3] <- v$meta$by
    p <- p + geom_point(data=pointData)
  }
  p
}
