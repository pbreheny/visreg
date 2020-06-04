ggContPlot <- function(v, partial, band, rug, whitespace, strip.names, overlay, top, line.par, fill.par, points.par, ...) {

  # Setup data frames
  xx <- v$fit[, v$meta$x]
  fillData <- data.frame(x = c(xx, rev(xx)),
                         y = c(v$fit$visregLwr, rev(v$fit$visregUpr)))
  lineData <- data.frame(x = xx,
                         y = v$fit$visregFit)
  pointData <- data.frame(x = v$res[, v$meta$x],
                          y = v$res$visregRes)
  if ("by" %in% names(v$meta)) {
    bb <- factor(v$fit[, v$meta$by])
    fillData$z <- factor(c(bb, rev(bb)), labels=levels(bb))
    lineData$z <- bb
    pointData$z <- factor(v$res[, v$meta$by])
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

  # Base gg object and aesthetic defaults
  if ("by" %in% names(v$meta) & overlay){
    p <- ggplot2::ggplot(pointData, ggplot2::aes_string('x', 'y', group=v$meta$by))
    fill.args <- list(mapping=ggplot2::aes_string(fill=v$meta$by))
    line.args <- list(mapping=ggplot2::aes_string(color=v$meta$by), size=1)
    point.args <- list(mapping=ggplot2::aes_string(color=v$meta$by), size=0.8)
    acol <- pal(length(levels(bb)), alpha=0.3)
    col <- pal(length(levels(bb)))
    if (length(fill.par)) fill.args[names(fill.par)] <- fill.par
    if (length(line.par)) line.args[names(line.par)] <- line.par
    if (length(points.par)) point.args[names(points.par)] <- points.par
    p <- p + ggplot2::scale_fill_manual(values=acol) + ggplot2::scale_color_manual(values=col)
  } else {
    p <- ggplot2::ggplot(pointData, ggplot2::aes_string('x', 'y'))
    fill.args <- list(fill="gray85")
    line.args <- list(size=1, col="#008DFFFF")
    point.args <- list(size=0.8, col="gray50")
    if (length(fill.par)) fill.args[names(fill.par)] <- fill.par
    if (length(line.par)) line.args[names(line.par)] <- line.par
    if (length(points.par)) point.args[names(points.par)] <- points.par
  }
  p <- p + ggplot2::xlab(xlab) + ggplot2::ylab(ylab)

  # Add geoms
  if (band) {
    fill.args$data <- fillData
    p <- p + do.call("geom_polygon", fill.args, envir=asNamespace("ggplot2"))
  }
  line.args$data <- lineData
  if (!partial) {
    p <- p + do.call("geom_line", line.args, envir=asNamespace("ggplot2"))
  } else {
    point.args$data <- pointData
    if (top == 'line') {
      p <- p + do.call("geom_point", point.args, envir=asNamespace("ggplot2"))
      p <- p + do.call("geom_line", line.args, envir=asNamespace("ggplot2"))
    } else {
      p <- p + do.call("geom_line", line.args, envir=asNamespace("ggplot2"))
      p <- p + do.call("geom_point", point.args, envir=asNamespace("ggplot2"))
    }
  }
  if (rug==1) {
    rug.args <- point.args
    rug.args$sides <- 'b'
    p <- p + do.call("geom_rug", point.args, envir=asNamespace("ggplot2"))
  }
  if (rug==2) {
    top.args <- bot.args <- point.args
    top.args$sides <- 't'
    bot.args$sides <- 'b'
    top.args$data <- pointData[v$res$visregPos,]
    bot.args$data <- pointData[!v$res$visregPos,]
    p <- p + do.call("geom_rug", top.args, envir=asNamespace("ggplot2"))
    p <- p + do.call("geom_rug", bot.args, envir=asNamespace("ggplot2"))
  }
  
  # Facet
  if ("by" %in% names(v$meta) & !overlay) {
    form <- as.formula(paste("~", v$meta$by))
    K <- length(levels(bb))
    if (identical(strip.names, TRUE)) {
      p <- p + ggplot2::facet_grid(form, labeller=ggplot2::label_both)
    } else if (identical(strip.names, FALSE)) {
      p <- p + ggplot2::facet_grid(form)
    } else if (is.character(strip.names) & length(strip.names) == K) {
      names(strip.names) <- levels(bb)
      args <- list(strip.names)
      names(args) <- v$meta$by
      lbl <- do.call(ggplot2::labeller, args)
      p <- p + ggplot2::facet_grid(form, labeller=lbl)
    } else {
      stop('strip.names must either be logical or a character vector with length equal to the number of facets', call.=FALSE)
    }
  }
  return(p)
}
