ggContPlot <- function(v, partial, band, rug, strip.names, overlay, top, line, fill, points, ...) {
  # Setup data frames
  xx <- v$fit[, v$meta$x]
  fillData <- data.frame(x = xx, ymin = v$fit$visregLwr, ymax = v$fit$visregUpr)
  lineData <- data.frame(x = xx, y = v$fit$visregFit)
  pointData <- data.frame(x = v$res[, v$meta$x], y = v$res$visregRes)
  if ("by" %in% names(v$meta)) {
    bb <- factor(v$fit[, v$meta$by])
    fillData[[v$meta$by]] <- bb
    lineData[[v$meta$by]] <- bb
    pointData[[v$meta$by]] <- factor(v$res[, v$meta$by])
  }

  # Plotting defaults
  dots <- list(...)
  xlab <- if ("xlab" %in% names(dots)) dots$xlab else v$meta$x
  if ("ylab" %in% names(dots)) {
    ylab <- dots$ylab
  } else {
    ylab <- if (is.null(v$meta$yName)) {
      paste("f(", v$meta$x, ")", sep = "")
    } else {
      v$meta$yName
    }
  }

  p <- ggplot2::ggplot() + ggplot2::xlab(xlab) + ggplot2::ylab(ylab)

  # Aesthetic defaults; each layer carries its own data + mapping, so none
  # of them rely on inheriting aesthetics from a base plot/data
  if ("by" %in% names(v$meta) & overlay) {
    fill.args <- list(
      mapping = ggplot2::aes(
        x = .data$x,
        ymin = .data$ymin,
        ymax = .data$ymax,
        fill = .data[[v$meta$by]]
      ),
      data = fillData
    )
    line.args <- list(
      mapping = ggplot2::aes(x = .data$x, y = .data$y, color = .data[[v$meta$by]]),
      data = lineData,
      linewidth = 1
    )
    point.args <- list(
      mapping = ggplot2::aes(x = .data$x, y = .data$y, color = .data[[v$meta$by]]),
      data = pointData,
      size = 0.8
    )
    acol <- pal(length(levels(bb)), alpha = 0.3)
    col <- pal(length(levels(bb)))
    if (length(fill)) {
      fill.args[names(fill)] <- fill
    }
    if (length(line)) {
      line.args[names(line)] <- line
    }
    if (length(points)) {
      point.args[names(points)] <- points
    }
    if (is.character(strip.names) & length(strip.names) == length(levels(bb))) {
      p <- p +
        ggplot2::scale_fill_manual(values = acol, labels = strip.names) +
        ggplot2::scale_color_manual(values = col, labels = strip.names)
    } else {
      p <- p + ggplot2::scale_fill_manual(values = acol) + ggplot2::scale_color_manual(values = col)
    }
  } else {
    fill.args <- list(
      mapping = ggplot2::aes(x = .data$x, ymin = .data$ymin, ymax = .data$ymax),
      data = fillData,
      fill = "gray85"
    )
    line.args <- list(
      mapping = ggplot2::aes(x = .data$x, y = .data$y),
      data = lineData,
      linewidth = 1,
      color = "#008DFFFF"
    )
    point.args <- list(
      mapping = ggplot2::aes(x = .data$x, y = .data$y),
      data = pointData,
      size = 0.8,
      color = "gray50"
    )
    if (length(fill)) {
      fill.args[names(fill)] <- fill
    }
    if (length(line)) {
      line.args[names(line)] <- line
    }
    if (length(points)) point.args[names(points)] <- points
  }

  # Add geoms
  if (band) {
    p <- p + do.call("geom_ribbon", fill.args, envir = asNamespace("ggplot2"))
  }
  if (!partial) {
    p <- p + do.call("geom_line", line.args, envir = asNamespace("ggplot2"))
  } else {
    if (top == "line") {
      p <- p + do.call("geom_point", point.args, envir = asNamespace("ggplot2"))
      p <- p + do.call("geom_line", line.args, envir = asNamespace("ggplot2"))
    } else {
      p <- p + do.call("geom_line", line.args, envir = asNamespace("ggplot2"))
      p <- p + do.call("geom_point", point.args, envir = asNamespace("ggplot2"))
    }
  }
  if (rug == 1) {
    rug.args <- point.args
    rug.args$sides <- "b"
    p <- p + do.call("geom_rug", rug.args, envir = asNamespace("ggplot2"))
  }
  if (rug == 2) {
    top.args <- bot.args <- point.args
    top.args$sides <- "t"
    bot.args$sides <- "b"
    top.args$data <- pointData[v$res$visregPos, ]
    bot.args$data <- pointData[!v$res$visregPos, ]
    p <- p + do.call("geom_rug", top.args, envir = asNamespace("ggplot2"))
    p <- p + do.call("geom_rug", bot.args, envir = asNamespace("ggplot2"))
  }

  # Facet
  if ("by" %in% names(v$meta) & !overlay) {
    form <- as.formula(paste("~", v$meta$by))
    K <- length(levels(bb))
    if (identical(strip.names, TRUE)) {
      p <- p + ggplot2::facet_grid(form, labeller = ggplot2::label_both)
    } else if (identical(strip.names, FALSE)) {
      p <- p + ggplot2::facet_grid(form)
    } else if (is.character(strip.names) & length(strip.names) == K) {
      names(strip.names) <- levels(bb)
      args <- list(strip.names)
      names(args) <- v$meta$by
      lbl <- do.call(ggplot2::labeller, args)
      p <- p + ggplot2::facet_grid(form, labeller = lbl)
    } else {
      stop(
        "strip.names must either be logical or a character vector with length equal to the number of facets",
        call. = FALSE
      )
    }
  }
  return(p)
}
