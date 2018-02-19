ggFactorPlot <- function(v, partial, band, rug, w, strip.names, overlay, line.par, fill.par, points.par, ...) {
  if ("by" %in% names(v$meta)) {
    if (is.factor(v$fit[,v$meta$by])) {
      lev <- levels(v$fit[,v$meta$by])
      b <- vrb <- factor(lev, levels=lev)
      z <- vrz <- v$res[,v$meta$by]
    } else {
      lev <- abbrNum(v$fit[,v$meta$by])
      b <- factor(lev, levels=lev)
      vrz <- v$res[,v$meta$by]
      vrb <- unique(vrz)
    }
    J <- length(b)
    if (overlay) {
      facet <- FALSE
    } else {
      facet <- TRUE
    }
  } else {
    J <- 1
    facet <- FALSE
  }
  K <- length(levels(v$fit[,v$meta$x]))
  len <- K*(1-w)+(K-1)*w

  # Initialize gg object and aesthetic defaults
  fillData <- lineData <- pointData <- NULL
  df0 <- data.frame(visregFit = c(v$fit$visregFit, v$fit$visregFit))
  df0$visregGGX <- rep(0:1, each=nrow(v$fit))
  df0$visregGGY <- rep(v$fit$visregFit, 2)
  dots <- list(...)
  xlab <- if ("xlab" %in% names(dots)) dots$xlab else v$meta$x
  if ("ylab" %in% names(dots)) {
    ylab <- dots$ylab
  } else {
    ylab <- if (is.null(v$meta$yName)) paste("f(", v$meta$x, ")", sep="") else v$meta$yName
  }
  if ("by" %in% names(v$meta) & overlay){
    p <- ggplot2::ggplot(df0, ggplot2::aes_string('visregGGX', 'visregGGY', group = v$meta$by))
    fill.args <- list(mapping=ggplot2::aes_string(fill=v$meta$by))
    line.args <- list(mapping=ggplot2::aes_string(color=v$meta$by), size=1)
    point.args <- list(mapping=ggplot2::aes_string(color=v$meta$by), size=0.8)
    col <- pal(length(lev))
    acol <- pal(length(lev), alpha=0.3)
    aacol <- pal(length(lev), alpha=0.3/K)
    p <- p + ggplot2::scale_fill_manual(values=acol) +
      ggplot2::scale_color_manual(values=col) +
      ggplot2::guides(fill=ggplot2::guide_legend(override.aes = list(fill=aacol)))
  } else {
    p <- ggplot2::ggplot(pointData, ggplot2::aes_string('visregGGX', 'visregGGY'))
    fill.args <- list(fill="gray85")
    line.args <- list(size=1, col="#008DFFFF")
    point.args <- list(size=0.8, col="gray50")
    if (length(fill.par)) fill.args[names(fill.par)] <- fill.par
    if (length(line.par)) line.args[names(line.par)] <- line.par
    if (length(points.par)) point.args[names(points.par)] <- points.par
  }
  p <- p + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
    ggplot2::scale_x_continuous(breaks=(0:(K-1))/len+(1-w)/(2*len), labels = levels(v$fit[,v$meta$x]))

  # Bands
  if (band) {
    for(k in 1:K) {
      x1 <- (k-1)/len
      x2 <- (k-1)/len + (1-w)/len
      xx <- c(x1, x2, x2, x1)
      fillData <- data.frame(visregGGX = rep(xx, J),
                             visregGGY = as.numeric(apply(v$fit[(1:J-1)*K + k, c("visregLwr", "visregUpr")], 1, function(x) rep(x, each=2))))
      if ("by" %in% names(v$meta)) {
        fillData$visregGGZ <- rep(b, each=4)
        names(fillData)[3] <- v$meta$by
      }
      fill.args$data <- fillData
      p <- p + do.call("geom_polygon", fill.args, envir=asNamespace("ggplot2"))
    }
  }

  # Plot lines
  for(k in 1:K) {
    x1 <- (k-1)/len
    x2 <- (k-1)/len + (1-w)/len
    xx <- c(x1,x2)

    lineData <- data.frame(visregGGX = rep(xx, J),
                           visregGGY = rep(v$fit$visregFit[(1:J-1)*K + k], each=2))
    if ("by" %in% names(v$meta)) {
      lineData$visregGGZ <- rep(b, each=2)
      names(lineData)[3] <- v$meta$by
    }
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
        df <- NULL
        if ("by" %in% names(v$meta)) {
          ind <- (x==levels(x)[k]) & (vrz==vrb[j])
          if (any(ind)) {
            rx <- seq(x1, x2, len=sum(ind)+2)[c(-1,-(sum(ind)+2))]
            df <- data.frame(visregGGX = rx,
                             visregGGY = v$res$visregRes[ind],
                             visregGGZ = b[j])
          }
        } else {
          ind <- (x==levels(x)[k])
          if (any(ind)) {
            rx <- seq(x1, x2, len=sum(ind)+2)[c(-1,-(sum(ind)+2))]
            df <- data.frame(visregGGX = rx,
                             visregGGY = v$res$visregRes[ind])
          }
        }
        pointData <- rbind(pointData, df)
      }
    }
    if ("by" %in% names(v$meta)) names(pointData)[3] <- v$meta$by
    point.args$data <- pointData
    p <- p + do.call("geom_point", point.args, envir=asNamespace("ggplot2"))
  }

  # Facet
  if (facet) {
    form <- as.formula(paste("~", v$meta$by))
    if (strip.names==TRUE) {
      p <- p + ggplot2::facet_grid(form, labeller=ggplot2::label_both)
    } else {
      p <- p + ggplot2::facet_grid(form)
    }
  }
  p
}
