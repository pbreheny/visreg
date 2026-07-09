gg_cont_plot <- function(
  v,
  partial,
  band,
  rug,
  strip_names,
  overlay,
  top,
  line,
  fill,
  points,
  ...
) {
  # Setup data frames
  xx <- v$fit[, v$meta$x]
  fillData <- data.frame(x = xx, ymin = v$fit$visreg_lwr, ymax = v$fit$visreg_upr)
  lineData <- data.frame(x = xx, y = v$fit$visreg_fit)
  pointData <- data.frame(x = v$res[, v$meta$x], y = v$res$visreg_res)
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
    if (is.character(strip_names) & length(strip_names) == length(levels(bb))) {
      p <- p +
        ggplot2::scale_fill_manual(values = acol, labels = strip_names) +
        ggplot2::scale_color_manual(values = col, labels = strip_names)
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
  rug_args <- point.args
  if ("size" %in% names(rug_args)) {
    rug_args$linewidth <- rug_args$size
    rug_args$size <- NULL
  }  
  if (rug == 1) {
    rug_args$sides <- "b"
    p <- p + do.call("geom_rug", rug_args, envir = asNamespace("ggplot2"))
  }
  if (rug == 2) {
    top_args <- bot_args <- rug_args
    top_args$sides <- "t"
    bot_args$sides <- "b"
    top_args$data <- pointData[v$res$visreg_pos, ]
    bot_args$data <- pointData[!v$res$visreg_pos, ]
    p <- p + do.call("geom_rug", top_args, envir = asNamespace("ggplot2"))
    p <- p + do.call("geom_rug", bot_args, envir = asNamespace("ggplot2"))
  }

  # Facet
  if ("by" %in% names(v$meta) & !overlay) {
    form <- as.formula(paste("~", v$meta$by))
    K <- length(levels(bb))
    if (identical(strip_names, TRUE)) {
      p <- p + ggplot2::facet_grid(form, labeller = ggplot2::label_both)
    } else if (identical(strip_names, FALSE)) {
      p <- p + ggplot2::facet_grid(form)
    } else if (is.character(strip_names) & length(strip_names) == K) {
      names(strip_names) <- levels(bb)
      args <- list(strip_names)
      names(args) <- v$meta$by
      lbl <- do.call(ggplot2::labeller, args)
      p <- p + ggplot2::facet_grid(form, labeller = lbl)
    } else {
      stop(
        "strip_names must either be logical or a character vector with length equal to the number of facets",
        call. = FALSE
      )
    }
  }
  return(p)
}
