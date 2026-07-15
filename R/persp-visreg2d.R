#' Static 3D perspective plot for a visreg2d object
#'
#' `persp()` method for [visreg2d()] objects: draws a static 3-dimensional perspective plot of the
#' fitted surface, using base R's [graphics::persp()].
#'
#' For an interactive version that can be rotated with the mouse, see `rgl::persp3d()` (requires the
#' `rgl` package). For a 2-dimensional raster/contour plot, see [plot.visreg2d()].
#'
#' @param x A [visreg2d()] object.
#' @param xlab Axis label for x variable
#' @param ylab Axis label for y variable
#' @param zlab Axis label for outcome
#' @param color The color of the surface.
#' @param whitespace When `xvar` or `yvar` is a factor, `whitespace` determines the amount of space
#'   in between the factors. Default is 0.2, meaning that 20 percent of the axis is whitespace.
#' @param ... Additional graphical parameters, passed to [graphics::persp()].
#'
#' @returns The viewing transformation matrix, invisibly; see [graphics::persp()].
#'
#' @seealso
#' [visreg2d()] for creating two-dimensional `visreg` objects, [plot.visreg2d()] for the
#' 2-dimensional raster/contour plot, and the
#' [surface plots vignette](https://pbreheny.github.io/visreg/articles/surface.html) for examples
#' and details.
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
#' v <- visreg2d(fit, x = "Wind", y = "Temp", plot = FALSE)
#' persp(v)
#'
#' @export
persp.visreg2d <- function(
  x,
  xlab = NULL,
  ylab = NULL,
  zlab = NULL,
  color = "#2fa4e7",
  whitespace = 0.2,
  ...
) {
  labs <- resolve_2d_labels(x, xlab, ylab, zlab, drop_expr_z = TRUE)
  axes <- prep_2d_axes(x, whitespace, gg = FALSE)
  ticktype <- if (is.factor(x$x) || is.factor(x$y)) "simple" else "detailed"

  plot_args <- list(
    x = axes$xx,
    y = axes$yy,
    z = axes$zz,
    xlim = axes$xlim,
    ylim = axes$ylim,
    xlab = labs$xlab,
    ylab = labs$ylab,
    zlab = labs$zlab,
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
  do.call(graphics::persp, plot_args)
}
