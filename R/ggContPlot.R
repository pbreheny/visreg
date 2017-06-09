ggContPlot <- function(v, partial, band, rug, whitespace, strip.names, line.par, fill.par, points.par, ...) {

  # Setup data frames
  xx <- v$fit[,v$meta$x]
  fillData <- data.frame(x = c(xx, rev(xx)),
                         y = c(v$fit$visregLwr, rev(v$fit$visregUpr)))
  lineData <- data.frame(x = xx,
                         y = v$fit$visregFit)
  pointData <- data.frame(x = v$res[,v$meta$x],
                          y = v$res$visregRes)
  if ("by" %in% names(v$meta)) {
    bb <- v$fit[,v$meta$by]
    if (is.factor(bb)) {
      fillData$z <- factor(c(bb, rev(bb)), labels=levels(bb))
    } else {
      fillData$z <- c(bb, rev(bb))
    }
    lineData$z <- bb
    pointData$z <- v$res[,v$meta$by]
    names(fillData)[3] <- names(lineData)[3] <- names(pointData)[3] <- v$meta$by
  }

  # Plotting defaults
  dots <- list(...)
  xlab <- if ("xlab" %in% names(dots)) dots$xlab else v$meta$x
  if ("ylab" %in% names(dots)) {
    ylab <- dots$ylab
  } else {
    ylab <- if (is.null(v$meta$yName)) paste("f(", v$meta$x, ")", sep="") else v$meta$yName
  }

  # Plot
  x <- y <- NULL
  p <- ggplot2::qplot(x, y, data=pointData, xlab=xlab, ylab=ylab, geom="blank")
  if (band) {
    fill.args <- list(fill="gray85")
    if (length(fill.par)) fill.args[names(fill.par)] <- fill.par
    fill.args$data <- fillData
    p <- p + do.call("geom_polygon", fill.args, envir=asNamespace("ggplot2"))
  }
  line.args <- list(size=1, col="#008DFFFF")
  if (length(line.par)) line.args[names(line.par)] <- line.par
  line.args$data <- lineData
  p <- p + do.call("geom_line", line.args, envir=asNamespace("ggplot2"))
  if (partial) {
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
  return(p)
}