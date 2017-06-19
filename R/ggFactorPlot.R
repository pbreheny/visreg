ggFactorPlot <- function(v, partial, band, rug, w, strip.names, line.par, fill.par, points.par, ...) {
  if ("by" %in% names(v$meta)) {
    b <- unique(v$fit[,v$meta$by])
    J <- length(b)
    facet <- TRUE
  } else {
    J <- 1
    facet <- FALSE
  }
  K <- length(levels(v$fit[,v$meta$x]))
  len <- K*(1-w)+(K-1)*w

  # Initialize
  fillData <- lineData <- pointData <- NULL
  df0 <- rbind(v$fit, v$fit)
  df0$x <- rep(0:1, each=nrow(v$fit))
  df0$y <- rep(v$fit$visregFit, 2)
  dots <- list(...)
  xlab <- if ("xlab" %in% names(dots)) dots$xlab else v$meta$x
  if ("ylab" %in% names(dots)) {
    ylab <- dots$ylab
  } else {
    ylab <- if (is.null(v$meta$yName)) paste("f(", v$meta$x, ")", sep="") else v$meta$yName
  }
  x <- y <- NULL
  p <- ggplot2::qplot(x, y, data=df0, xlab=xlab, ylab=ylab, geom="blank")
  p <- p + ggplot2::scale_x_continuous(breaks=(0:(K-1))/len+(1-w)/(2*len), labels = levels(v$fit[,v$meta$x]))

  # Bands
  if (band) {
    for(k in 1:K) {
      x1 <- (k-1)/len
      x2 <- (k-1)/len + (1-w)/len
      xx <- c(x1, x2, x2, x1)
      fillData <- data.frame(x = rep(xx, J),
                             y = as.numeric(apply(v$fit[(1:J-1)*J + k, c("visregLwr", "visregUpr")], 1, function(x) rep(x, each=2))))
      if (facet) {
        fillData$z <- rep(b, each=4)
        names(fillData)[3] <- v$meta$by
      }
      fill.args <- list(fill="gray85")
      if (length(fill.par)) fill.args[names(fill.par)] <- fill.par
      fill.args$data <- fillData
      p <- p + do.call("geom_polygon", fill.args, envir=asNamespace("ggplot2"))
    }
  }

  # Plot lines
  for(k in 1:K) {
    x1 <- (k-1)/len
    x2 <- (k-1)/len + (1-w)/len
    xx <- c(x1,x2)

    lineData <- data.frame(x = rep(xx, J),
                           y = rep(v$fit$visregFit[(1:J-1)*K + k], each=2))
    if (facet) {
      lineData$z <- rep(b, each=2)
      names(lineData)[3] <- v$meta$by
    }
    line.args <- list(size=1, col="#008DFFFF")
    if (length(line.par)) line.args[names(line.par)] <- line.par
    line.args$data <- lineData
    p <- p + do.call("geom_line", line.args, envir=asNamespace("ggplot2"))
  }

  # Plot points
  if (partial) {
    for (j in 1:J) {
      for (k in 1:K) {
        x1 <- (k-1)/len
        x2 <- (k-1)/len + (1-w)/len
        xx <- c(x1,x2)
        x <- v$res[,v$meta$x]
        z <- v$res[,v$meta$by]
        df <- NULL
        if ("by" %in% names(v$meta)) {
          ind <- (x==levels(x)[k]) & (z==b[j])
          if (any(ind)) {
            rx <- seq(x1, x2, len=sum(ind)+2)[c(-1,-(sum(ind)+2))]
            df <- data.frame(x = rx,
                             y = v$res$visregRes[ind],
                             z = b[j])
          }
        } else {
          if (any(ind)) {
            ind <- (x==levels(x)[k])
            rx <- seq(x1, x2, len=sum(ind)+2)[c(-1,-(sum(ind)+2))]
            df <- data.frame(x = rx,
                             y = v$res$visregRes[ind])
          }
        }
        pointData <- rbind(pointData, df)
      }
    }
    if (facet) names(pointData)[3] <- v$meta$by
    point.args <- list(size=0.8, col="gray50")
    if (length(points.par)) point.args[names(points.par)] <- points.par
    point.args$data <- pointData
    p <- p + do.call("geom_point", point.args, envir=asNamespace("ggplot2"))
  }

  # Facet
  if ("by" %in% names(v$meta)) {
    form <- as.formula(paste("~", v$meta$by))
    if (strip.names==TRUE) {
      p <- p + ggplot2::facet_grid(form, labeller=ggplot2::label_both)
    } else {
      p <- p + ggplot2::facet_grid(form)
    }
  }
  p
}
