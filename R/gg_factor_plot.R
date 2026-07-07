gg_factor_plot <- function(
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
  hasBy <- "by" %in% names(v$meta)

  # Setup data frames
  fitData <- data.frame(
    x = v$fit[, v$meta$x],
    fit = v$fit$visreg_fit,
    ymin = v$fit$visreg_lwr,
    ymax = v$fit$visreg_upr
  )
  resData <- data.frame(x = v$res[, v$meta$x], y = v$res$visreg_res, pos = v$res$visreg_pos)

  if (hasBy) {
    rawBy <- v$fit[, v$meta$by]
    if (is.factor(rawBy)) {
      byLevels <- levels(rawBy)
      fitBy <- factor(rawBy, levels = byLevels)
      resBy <- factor(v$res[, v$meta$by], levels = byLevels)
    } else {
      uniqueRaw <- unique(rawBy)
      byLevels <- abbr_num(rawBy)
      fitBy <- factor(byLevels[match(rawBy, uniqueRaw)], levels = byLevels)
      resBy <- factor(byLevels[match(v$res[, v$meta$by], uniqueRaw)], levels = byLevels)
    }
    fitData[[v$meta$by]] <- fitBy
    resData[[v$meta$by]] <- resBy
  }

  # Plotting defaults
  dots <- list(...)
  xlab <- if ("xlab" %in% names(dots)) dots$xlab else v$meta$x
  ylab <- if ("ylab" %in% names(dots)) {
    dots$ylab
  } else if (is.null(v$meta$yName)) {
    paste("f(", v$meta$x, ")", sep = "")
  } else {
    v$meta$yName
  }

  p <- ggplot2::ggplot() + ggplot2::xlab(xlab) + ggplot2::ylab(ylab)

  # Band (geom_tile), line (geom_errorbar with ymin=ymax=fit), and jitter
  # (residuals) aesthetic defaults.  Band and line are kept as separate
  # layers, as with the continuous plot, so that the band never overplots
  # the residuals regardless of `top`.
  if (hasBy & overlay) {
    dodge <- ggplot2::position_dodge2(width = 0.7, padding = 0.1)
    fill.args <- list(
      mapping = ggplot2::aes(
        x = .data$x,
        y = (.data$ymin + .data$ymax) / 2,
        height = .data$ymax - .data$ymin,
        fill = .data[[v$meta$by]]
      ),
      data = fitData,
      position = dodge,
      width = 1
    )
    line.args <- list(
      mapping = ggplot2::aes(
        x = .data$x,
        ymin = .data$fit,
        ymax = .data$fit,
        color = .data[[v$meta$by]]
      ),
      data = fitData,
      position = dodge,
      width = 1,
      linewidth = 1
    )
    point.args <- list(
      mapping = ggplot2::aes(x = .data$x, y = .data$y, color = .data[[v$meta$by]]),
      data = resData,
      position = ggplot2::position_jitterdodge(jitter.width = 0.15, dodge.width = 0.7),
      size = 0.8
    )
    nBy <- length(byLevels)
    col <- pal(nBy)
    acol <- pal(nBy, alpha = 0.5)
    if (length(fill)) {
      fill.args[names(fill)] <- fill
    }
    if (length(line)) {
      line.args[names(line)] <- line
    }
    if (length(points)) {
      point.args[names(points)] <- points
    }
    if (is.character(strip_names) & length(strip_names) == nBy) {
      p <- p +
        ggplot2::scale_fill_manual(values = acol, labels = strip_names) +
        ggplot2::scale_color_manual(values = col, labels = strip_names)
    } else {
      p <- p + ggplot2::scale_fill_manual(values = acol) + ggplot2::scale_color_manual(values = col)
    }
  } else {
    fill.args <- list(
      mapping = ggplot2::aes(
        x = .data$x,
        y = (.data$ymin + .data$ymax) / 2,
        height = .data$ymax - .data$ymin
      ),
      data = fitData,
      fill = "gray85",
      width = 0.6
    )
    line.args <- list(
      mapping = ggplot2::aes(x = .data$x, ymin = .data$fit, ymax = .data$fit),
      data = fitData,
      width = 0.6,
      linewidth = 1,
      color = "#008DFFFF"
    )
    point.args <- list(
      mapping = ggplot2::aes(x = .data$x, y = .data$y),
      data = resData,
      position = ggplot2::position_jitter(width = 0.15, height = 0),
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

  # Add geoms: band is always the bottom-most layer so it never obscures
  # the residuals; only the relative order of the line and the points
  # depends on `top`
  if (band) {
    p <- p + do.call("geom_tile", fill.args, envir = asNamespace("ggplot2"))
  }
  if (!partial) {
    p <- p + do.call("geom_errorbar", line.args, envir = asNamespace("ggplot2"))
  } else if (top == "line") {
    p <- p + do.call("geom_jitter", point.args, envir = asNamespace("ggplot2"))
    p <- p + do.call("geom_errorbar", line.args, envir = asNamespace("ggplot2"))
  } else {
    p <- p + do.call("geom_errorbar", line.args, envir = asNamespace("ggplot2"))
    p <- p + do.call("geom_jitter", point.args, envir = asNamespace("ggplot2"))
  }

  if (rug == 1) {
    p <- p + ggplot2::geom_rug(data = resData, ggplot2::aes(x = .data$x), sides = "b")
  }
  if (rug == 2) {
    p <- p +
      ggplot2::geom_rug(data = resData[resData$pos, ], ggplot2::aes(x = .data$x), sides = "t") +
      ggplot2::geom_rug(data = resData[!resData$pos, ], ggplot2::aes(x = .data$x), sides = "b")
  }

  # Facet
  if (hasBy & !overlay) {
    form <- as.formula(paste("~", v$meta$by))
    nBy <- length(byLevels)
    if (identical(strip_names, TRUE)) {
      p <- p + ggplot2::facet_grid(form, labeller = ggplot2::label_both)
    } else if (identical(strip_names, FALSE)) {
      p <- p + ggplot2::facet_grid(form)
    } else if (is.character(strip_names) & length(strip_names) == nBy) {
      names(strip_names) <- byLevels
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
  p
}
