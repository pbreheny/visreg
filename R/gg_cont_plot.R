gg_cont_plot <- function(
  v,
  partial,
  band,
  rug,
  jitter,
  strip_names,
  overlay,
  top,
  line,
  fill,
  points
) {
  # Setup data frames
  xx <- v$fit[, v$meta$x]
  fill_data <- data.frame(x = xx, ymin = v$fit$visreg_lwr, ymax = v$fit$visreg_upr)
  line_data <- data.frame(x = xx, y = v$fit$visreg_fit)
  point_data <- data.frame(x = v$res[, v$meta$x], y = v$res$visreg_res)
  if (jitter) {
    point_data$x <- jitter(point_data$x)
  }
  if ("by" %in% names(v$meta)) {
    bb <- factor(v$fit[, v$meta$by])
    fill_data[[v$meta$by]] <- bb
    line_data[[v$meta$by]] <- bb
    point_data[[v$meta$by]] <- factor(v$res[, v$meta$by])
  }

  # Plotting defaults
  xlab <- v$meta$x
  ylab <- if (is.null(v$meta$y_name)) {
    paste("f(", v$meta$x, ")", sep = "")
  } else {
    v$meta$y_name
  }

  p <- ggplot2::ggplot() + ggplot2::xlab(xlab) + ggplot2::ylab(ylab)

  # Aesthetic defaults; each layer carries its own data + mapping, so none
  # of them rely on inheriting aesthetics from a base plot/data
  if ("by" %in% names(v$meta) && overlay) {
    fill_args <- list(
      mapping = ggplot2::aes(
        x = .data$x,
        ymin = .data$ymin,
        ymax = .data$ymax,
        fill = .data[[v$meta$by]]
      ),
      data = fill_data
    )
    line_args <- list(
      mapping = ggplot2::aes(x = .data$x, y = .data$y, color = .data[[v$meta$by]]),
      data = line_data,
      linewidth = 1
    )
    point_args <- list(
      mapping = ggplot2::aes(x = .data$x, y = .data$y, color = .data[[v$meta$by]]),
      data = point_data,
      size = 0.8
    )
    acol <- pal(nlevels(bb), alpha = 0.3)
    col <- pal(nlevels(bb))
    if (length(fill)) {
      fill_args[names(fill)] <- fill
    }
    if (length(line)) {
      line_args[names(line)] <- line
    }
    if (length(points)) {
      point_args[names(points)] <- points
    }
    if (is.character(strip_names) && length(strip_names) == nlevels(bb)) {
      p <- p +
        ggplot2::scale_fill_manual(values = acol, labels = strip_names) +
        ggplot2::scale_color_manual(values = col, labels = strip_names)
    } else {
      p <- p + ggplot2::scale_fill_manual(values = acol) + ggplot2::scale_color_manual(values = col)
    }
  } else {
    fill_args <- list(
      mapping = ggplot2::aes(x = .data$x, ymin = .data$ymin, ymax = .data$ymax),
      data = fill_data,
      fill = "gray85"
    )
    line_args <- list(
      mapping = ggplot2::aes(x = .data$x, y = .data$y),
      data = line_data,
      linewidth = 1,
      color = "#008DFFFF"
    )
    point_args <- list(
      mapping = ggplot2::aes(x = .data$x, y = .data$y),
      data = point_data,
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

  # Add geoms
  if (band) {
    p <- p + do.call("geom_ribbon", fill_args, envir = asNamespace("ggplot2"))
  }
  if (!partial) {
    p <- p + do.call("geom_line", line_args, envir = asNamespace("ggplot2"))
  } else {
    if (top == "line") {
      p <- p + do.call("geom_point", point_args, envir = asNamespace("ggplot2"))
      p <- p + do.call("geom_line", line_args, envir = asNamespace("ggplot2"))
    } else {
      p <- p + do.call("geom_line", line_args, envir = asNamespace("ggplot2"))
      p <- p + do.call("geom_point", point_args, envir = asNamespace("ggplot2"))
    }
  }
  rug_args <- point_args
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
    top_args$data <- point_data[v$res$visreg_pos, ]
    bot_args$data <- point_data[!v$res$visreg_pos, ]
    p <- p + do.call("geom_rug", top_args, envir = asNamespace("ggplot2"))
    p <- p + do.call("geom_rug", bot_args, envir = asNamespace("ggplot2"))
  }

  # Facet
  if ("by" %in% names(v$meta) && !overlay) {
    form <- as.formula(paste("~", v$meta$by))
    if (identical(strip_names, TRUE)) {
      p <- p + ggplot2::facet_grid(form, labeller = ggplot2::label_both)
    } else if (identical(strip_names, FALSE)) {
      p <- p + ggplot2::facet_grid(form)
    } else if (is.character(strip_names) && length(strip_names) == nlevels(bb)) {
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
