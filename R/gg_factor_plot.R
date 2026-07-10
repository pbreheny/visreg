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
  fit_data <- data.frame(
    x = v$fit[, v$meta$x],
    fit = v$fit$visreg_fit,
    ymin = v$fit$visreg_lwr,
    ymax = v$fit$visreg_upr
  )
  res_data <- data.frame(x = v$res[, v$meta$x], y = v$res$visreg_res, pos = v$res$visreg_pos)

  if (hasBy) {
    raw_by <- v$fit[, v$meta$by]
    if (is.factor(raw_by)) {
      by_levels <- levels(raw_by)
      fit_by <- factor(raw_by, levels = by_levels)
      res_by <- factor(v$res[, v$meta$by], levels = by_levels)
    } else {
      unique_raw <- unique(raw_by)
      by_levels <- abbr_num(raw_by)
      fit_by <- factor(by_levels[match(raw_by, unique_raw)], levels = by_levels)
      res_by <- factor(by_levels[match(v$res[, v$meta$by], unique_raw)], levels = by_levels)
    }
    fit_data[[v$meta$by]] <- fit_by
    res_data[[v$meta$by]] <- res_by
  }

  # Plotting defaults
  dots <- list(...)
  xlab <- if ("xlab" %in% names(dots)) dots$xlab else v$meta$x
  ylab <- if ("ylab" %in% names(dots)) {
    dots$ylab
  } else if (is.null(v$meta$y_name)) {
    paste("f(", v$meta$x, ")", sep = "")
  } else {
    v$meta$y_name
  }

  p <- ggplot2::ggplot() + ggplot2::xlab(xlab) + ggplot2::ylab(ylab)

  # Band (geom_tile), line (geom_errorbar with ymin=ymax=fit), and jitter
  # (residuals) aesthetic defaults.  Band and line are kept as separate
  # layers, as with the continuous plot, so that the band never overplots
  # the residuals regardless of `top`.
  if (hasBy && overlay) {
    dodge <- ggplot2::position_dodge2(width = 0.7, padding = 0.1)
    fill_args <- list(
      mapping = ggplot2::aes(
        x = .data$x,
        y = (.data$ymin + .data$ymax) / 2,
        height = .data$ymax - .data$ymin,
        fill = .data[[v$meta$by]]
      ),
      data = fit_data,
      position = dodge,
      width = 1
    )
    line_args <- list(
      mapping = ggplot2::aes(
        x = .data$x,
        ymin = .data$fit,
        ymax = .data$fit,
        color = .data[[v$meta$by]]
      ),
      data = fit_data,
      position = dodge,
      width = 1,
      linewidth = 1
    )
    point_args <- list(
      mapping = ggplot2::aes(x = .data$x, y = .data$y, color = .data[[v$meta$by]]),
      data = res_data,
      position = ggplot2::position_jitterdodge(jitter.width = 0.15, dodge.width = 0.7),
      size = 0.8
    )
    n_by <- length(by_levels)
    col <- pal(n_by)
    acol <- pal(n_by, alpha = 0.5)
    if (length(fill)) {
      fill_args[names(fill)] <- fill
    }
    if (length(line)) {
      line_args[names(line)] <- line
    }
    if (length(points)) {
      point_args[names(points)] <- points
    }
    if (is.character(strip_names) && length(strip_names) == n_by) {
      p <- p +
        ggplot2::scale_fill_manual(values = acol, labels = strip_names) +
        ggplot2::scale_color_manual(values = col, labels = strip_names)
    } else {
      p <- p + ggplot2::scale_fill_manual(values = acol) + ggplot2::scale_color_manual(values = col)
    }
  } else {
    fill_args <- list(
      mapping = ggplot2::aes(
        x = .data$x,
        y = (.data$ymin + .data$ymax) / 2,
        height = .data$ymax - .data$ymin
      ),
      data = fit_data,
      fill = "gray85",
      width = 0.6
    )
    line_args <- list(
      mapping = ggplot2::aes(x = .data$x, ymin = .data$fit, ymax = .data$fit),
      data = fit_data,
      width = 0.6,
      linewidth = 1,
      color = "#008DFFFF"
    )
    point_args <- list(
      mapping = ggplot2::aes(x = .data$x, y = .data$y),
      data = res_data,
      position = ggplot2::position_jitter(width = 0.15, height = 0),
      size = 0.8,
      color = "gray50"
    )
    if (length(fill)) {
      fill_args[names(fill)] <- fill
    }
    if (length(line)) {
      line_args[names(line)] <- line
    }
    if (length(points)) point_args[names(points)] <- points
  }

  # Add geoms: band is always the bottom-most layer so it never obscures
  # the residuals; only the relative order of the line and the points
  # depends on `top`
  if (band) {
    p <- p + do.call("geom_tile", fill_args, envir = asNamespace("ggplot2"))
  }
  if (!partial) {
    p <- p + do.call("geom_errorbar", line_args, envir = asNamespace("ggplot2"))
  } else if (top == "line") {
    p <- p + do.call("geom_jitter", point_args, envir = asNamespace("ggplot2"))
    p <- p + do.call("geom_errorbar", line_args, envir = asNamespace("ggplot2"))
  } else {
    p <- p + do.call("geom_errorbar", line_args, envir = asNamespace("ggplot2"))
    p <- p + do.call("geom_jitter", point_args, envir = asNamespace("ggplot2"))
  }

  if (rug == 1) {
    p <- p + ggplot2::geom_rug(data = res_data, ggplot2::aes(x = .data$x), sides = "b")
  }
  if (rug == 2) {
    p <- p +
      ggplot2::geom_rug(data = res_data[res_data$pos, ], ggplot2::aes(x = .data$x), sides = "t") +
      ggplot2::geom_rug(data = res_data[!res_data$pos, ], ggplot2::aes(x = .data$x), sides = "b")
  }

  # Facet
  if (hasBy && !overlay) {
    form <- as.formula(paste("~", v$meta$by))
    n_by <- length(by_levels)
    if (identical(strip_names, TRUE)) {
      p <- p + ggplot2::facet_grid(form, labeller = ggplot2::label_both)
    } else if (identical(strip_names, FALSE)) {
      p <- p + ggplot2::facet_grid(form)
    } else if (is.character(strip_names) && length(strip_names) == n_by) {
      names(strip_names) <- by_levels
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
