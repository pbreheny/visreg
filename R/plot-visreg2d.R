#' Visualization of regression functions for two variables
#'
#' Plot method for visualizing how two variables interact to affect the response in regression
#' models.
#'
#' @param x A [visreg2d()] object.
#' @param plot.type The style of plot to be produced. The following options are
#' supported:
#' - `image`: a filled contour
#' - `gg`: a filled contour plot using ggplot2
#' - `persp`: a 3 dimensional perspective plot
#' - `rgl`: a version of the perspective plot that can be rotated (requires the rgl package to be
#'   installed)
#' @param xlab Axis label for x variable
#' @param ylab Axis label for y variable
#' @param zlab Axis label for outcome
#' @param color For `plot.type='persp'` or `plot.type='rgl'`, the color of the surface. For
#'   `plot.type='image'` or `plot.type='gg'`, a vector of colors used to establish a color palette.
#' @param print_cond If `print_cond==TRUE`, the explanatory variable values conditioned on in a
#'   conditional plot are printed to the console (default: `FALSE`). If `print_cond==TRUE` and
#'   `type=="contrast"`, the conditions will still be printed, but they have no bearing on the plot
#'   unless interactions are present.
#' @param whitespace When `xvar` or `yvar` is a factor, `whitespace` determines the amount of space
#'   in between the factors. Default is 0.2, meaning that 20 percent of the axis is whitespace.
#' @param ... Graphical parameters can be passed to the function to customize the plots.
#'
#' @seealso
#' [visreg2d()] for creating two-dimensional `visreg` objects, and the
#' [surface plots vignette](https://pbreheny.github.io/visreg/surface.html) for examples and
#' details.
#' @references
#' Breheny P and Burchett W. (2017) Visualization of regression models using
#' visreg. *R Journal*, **9**: 56-71.
#' \doi{10.32614/RJ-2017-046}
#'
#' @examples
#' fit <- lm(
#'   Ozone ~ Solar.R + Wind + Temp + I(Wind^2) + I(Temp^2) +
#'     I(Wind * Temp) + I(Wind * Temp^2) + I(Temp * Wind^2) + I(Temp^2 * Wind^2),
#'   data = airquality
#' )
#'
#' visreg2d(fit, x = "Wind", y = "Temp", plot.type = "image")
#' visreg2d(fit,
#'   x = "Wind", y = "Temp", plot.type = "image",
#'   color = c("purple", "green", "red")
#' )
#' visreg2d(fit, x = "Wind", y = "Temp", plot.type = "persp")
#'
#' ## Requires the rgl package
#' \donttest{
#' visreg2d(fit, x = "Wind", y = "Temp", plot.type = "rgl")
#' }
#'
#' ## Requires the ggplot2 package
#' \donttest{
#' visreg2d(fit, x = "Wind", y = "Temp", plot.type = "gg")
#' }
#'
#' @export
plot.visreg2d <- function(
  x,
  plot.type = c("image", "persp", "rgl", "gg"),
  xlab,
  ylab,
  zlab,
  color,
  print_cond = FALSE,
  whitespace = 0.2,
  ...
) {
  plot.type <- match.arg(plot.type)
  if (missing(xlab)) {
    xlab <- x$meta$x
  }
  if (missing(ylab)) {
    ylab <- x$meta$y
  }
  if (missing(zlab)) {
    if (plot.type %in% c("persp", "rgl") && is.expression(x$meta$z)) {
      x$meta$z <- NULL
    } ## persp cannot handle expressions
    zlab <- if (is.null(x$meta$z)) {
      paste("f(", x$meta$x, ", ", x$meta$y, ")", sep = "")
    } else {
      x$meta$z
    }
  }
  if (missing(color)) {
    if (plot.type %in% c("image", "gg")) {
      color <- c(pal(3)[3], "gray90", pal(3)[1])
    } else if (plot.type == "persp") {
      color <- "#2fa4e7"
    } else if (plot.type == "rgl") {
      color <- "gray"
    }
  }
  zz <- x$z

  ## Make factor axes
  if (plot.type == "gg") {
    mx <- my <- lx <- ly <- ggplot2::waiver()
  } else {
    mx <- my <- NULL
    lx <- ly <- TRUE
  }
  if (is.factor(x$x)) {
    x_axis <- factorAxis2d(x$x, whitespace, 99)
    xx <- x_axis$x
    mx <- x_axis$m
    lx <- x_axis$l
    zz <- zz[x_axis$ind, ]
  } else {
    xx <- x$x
  }
  if (is.factor(x$y)) {
    y_axis <- factorAxis2d(x$y, whitespace, 99)
    yy <- y_axis$x
    my <- y_axis$m
    ly <- y_axis$l
    zz <- zz[, y_axis$ind]
  } else {
    yy <- x$y
  }
  xlim <- if (is.factor(x$x)) c(0, 1) else range(x$x)
  ylim <- if (is.factor(x$y)) c(0, 1) else range(x$y)

  if (plot.type == "image") {
    color_palette <- colorRampPalette(color, space = "Lab")
    plot_args <- list(
      x = xx,
      y = yy,
      z = zz,
      xlim = xlim,
      ylim = ylim,
      xlab = xlab,
      ylab = ylab,
      color_palette = color_palette,
      main = zlab
    )
    plot_args$plot.axes <- quote({
      axis(1, at = mx, labels = lx)
      axis(2, at = my, labels = ly)
    })
    new_args <- list(...)
    if (length(new_args)) {
      plot_args[names(new_args)] <- new_args
    }
    do.call("filled.contour", plot_args)
  } else if (plot.type == "persp") {
    ticktype <- ifelse(is.factor(x$x) | is.factor(x$y), "simple", "detailed")
    plot_args <- list(
      x = xx,
      y = yy,
      z = zz,
      xlim = xlim,
      ylim = ylim,
      xlab = xlab,
      ylab = ylab,
      zlab = zlab,
      ticktype = ticktype,
      theta = -30,
      col = color,
      border = "#BEBEBE33",
      shade = 0.5
    )
    new_args <- list(...)
    if (length(new_args)) {
      plot_args[names(new_args)] <- new_args
    }
    p <- do.call("persp", plot_args)
    return(p)
  } else if (plot.type == "rgl") {
    if (!requireNamespace("rgl")) {
      stop("You must first install the rgl package: install.packages('rgl')", call. = FALSE)
    }
    plot_args <- list(x = xx, y = yy, z = zz, xlab = xlab, ylab = ylab, zlab = zlab, color = color)
    new_args <- list(...)
    if (length(new_args)) {
      plot_args[names(new_args)] <- new_args
    }
    # if (i >= 2) rgl::open3d()
    do.call(rgl::persp3d, plot_args)
  } else if (plot.type == "gg") {
    if (!requireNamespace("ggplot2")) {
      stop("You must first install the ggplot2 package: install.packages('ggplot2')", call. = FALSE)
    }
    df <- data.frame(x = xx[row(zz)], y = yy[col(zz)], z = c(zz))
    p <- ggplot2::ggplot(df, ggplot2::aes(.data$x, .data$y)) +
      ggplot2::geom_raster(ggplot2::aes(fill = .data$z)) +
      ggplot2::scale_x_continuous(expand = c(0, 0), labels = lx, breaks = mx) +
      ggplot2::scale_y_continuous(expand = c(0, 0), labels = ly, breaks = my) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::scale_fill_gradientn(
        colors = color,
        na.value = "white",
        guide = ggplot2::guide_colorbar(title = zlab)
      )
    return(p)
  }
}
