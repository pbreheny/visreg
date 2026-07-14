#' Visualization of regression functions for two variables
#'
#' Plot method for visualizing how two variables interact to affect the response in regression
#' models, as a `ggplot2` raster/contour plot.
#'
#' For 3-dimensional surface plots, see [persp.visreg2d()] (static) or `rgl::persp3d()`
#' (interactive, requires the `rgl` package).
#'
#' @param x A [visreg2d()] object.
#' @param xlab Axis label for x variable
#' @param ylab Axis label for y variable
#' @param zlab Label for the color legend
#' @param color A vector of colors used to establish the color palette for the fill/legend.
#' @param print_cond If `print_cond==TRUE`, the explanatory variable values conditioned on in a
#'   conditional plot are printed to the console (default: `FALSE`). If `print_cond==TRUE` and
#'   `type=="contrast"`, the conditions will still be printed, but they have no bearing on the plot
#'   unless interactions are present.
#' @param whitespace When `xvar` or `yvar` is a factor, `whitespace` determines the amount of space
#'   in between the factors. Default is 0.2, meaning that 20 percent of the axis is whitespace.
#' @param ... Not used.
#'
#' @returns A `ggplot2` object.
#'
#' @seealso
#' [visreg2d()] for creating two-dimensional `visreg` objects, [persp.visreg2d()] for static 3D
#' surface plots, and the
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
#' visreg2d(fit, x = "Wind", y = "Temp")
#' visreg2d(fit, x = "Wind", y = "Temp", color = c("purple", "green", "red"))
#'
#' @export
plot.visreg2d <- function(
  x,
  xlab = NULL,
  ylab = NULL,
  zlab = NULL,
  color,
  print_cond = FALSE,
  whitespace = 0.2,
  ...
) {
  if (missing(color)) {
    color <- c(pal(3)[3], "gray90", pal(3)[1])
  }
  labs <- resolve_2d_labels(x, xlab, ylab, zlab)
  axes <- prep_2d_axes(x, whitespace, gg = TRUE)

  df <- data.frame(x = axes$xx[row(axes$zz)], y = axes$yy[col(axes$zz)], z = c(axes$zz))
  ggplot2::ggplot(df, ggplot2::aes(.data$x, .data$y)) +
    ggplot2::geom_raster(ggplot2::aes(fill = .data$z)) +
    ggplot2::scale_x_continuous(expand = c(0, 0), labels = axes$lx, breaks = axes$mx) +
    ggplot2::scale_y_continuous(expand = c(0, 0), labels = axes$ly, breaks = axes$my) +
    ggplot2::xlab(labs$xlab) +
    ggplot2::ylab(labs$ylab) +
    ggplot2::scale_fill_gradientn(
      colors = color,
      na.value = "white",
      guide = ggplot2::guide_colorbar(title = labs$zlab)
    )
}
