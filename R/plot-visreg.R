#' Visualization of regression functions
#'
#' A function for visualizing regression models quickly and easily. Default
#' plots contain a confidence band, prediction line, and partial residuals.
#' Factors, transformations, conditioning, interactions, and a variety of other
#' options are supported.  The `plot.visreg()` function accepts a `visreg`
#' or `visregList` object as calculated by [visreg()] and creates the plot.
#'
#' @param x A `visreg` or `visregList` object; see [visreg()].
#' @param overlay By default, when `by` is specified, separate panels are used
#' to display each cross-section. If `overlay=TRUE`, these cross-sections are
#' overlaid on top of each other in a single plot.
#' @param print.cond If `print.cond=TRUE`, the explanatory variable values
#' conditioned on in a conditional plot are printed to the console
#' (default: `FALSE`). If `print.cond=TRUE` and `type="contrast"`, the
#' conditions will still be printed, but they have no bearing on the plot
#' unless interactions are present.
#' @param partial If `partial=TRUE` (the default), partial residuals are
#' shown on the plot.
#' @param band If `band=TRUE` (the default), confidence bands are shown on
#' the plot.
#' @param rug By default, partial residuals are plotted. Alternatively, a
#' rug may be plotted along the horizontal axis instead.  Setting `rug=TRUE`
#' turns off partial residuals by default; if one wants both to be plotted, both
#' `rug=TRUE` and `partial=TRUE` need to be specified. Two types of rug plots
#' are available. If `rug=1` or `rug=TRUE`, then a basic rug is drawn on the
#' bottom. If \code{rug=2}, then separate rugs are drawn on the top for
#' observations with positive residuals and on the bottom for observations with
#' negative residuals. Such plots are particularly useful in logistic regression
#' (see examples).
#' @param strip.names When `by=TRUE`, `strip.names=TRUE` adds the
#' name of the `by` variable to the strip at the top of each panel. Default is
#' `FALSE` for factors and `TRUE` for numeric `by` variables.  `strip.names` can
#' also be a character vector, in which case it replaces the strip names
#' altogether with values chosen by the user.
#' @param top By default, the fitted line is plotted on top of the partial
#' residuals; usually this is preferable, but it does run the risk of obscuring
#' certain residuals. To change this behavior and plot the partial residuals on
#' top, specify `top='points'`.
#' @param line List of parameters to pass to [ggplot2::geom_line()] (continuous
#' `xvar`) or [ggplot2::geom_crossbar()] (factor `xvar`) when the fitted line
#' is plotted.
#' @param fill List of parameters to pass to [ggplot2::geom_ribbon()]
#' (continuous `xvar`) or [ggplot2::geom_crossbar()] (factor `xvar`) when
#' shaded confidence regions are plotted.
#' @param points List of parameters to pass to [ggplot2::geom_point()]
#' (continuous `xvar`) or [ggplot2::geom_jitter()] (factor `xvar`) when
#' partial residuals are plotted.
#' @param ... Additional arguments passed on to the plot, such as `xlab`,
#' `ylab`.
#'
#' @author Patrick Breheny and Woodrow Burchett
#'
#' @seealso <https://pbreheny.github.io/visreg/articles/options.html>, [visreg()],
#' [visreg2d()]
#'
#' @references
#' Breheny P and Burchett W. (2017) Visualization of regression models using
#' visreg. *R Journal*, **9**: 56-71.
#' \doi{10.32614/RJ-2017-046}
#'
#' @examples
#' fit <- lm(Ozone ~ Solar.R + Wind + Temp,data=airquality)
#' visreg(fit, "Wind", line=list(color="red"), points=list(size=1, shape=1))
#'
#' ## Changing appearance
#' visreg(fit, "Wind", line=list(color="red"), points=list(size=1, shape=1))
#'
#' ## See ?visreg and https://pbreheny.github.io/visreg for more examples
#' @export

plot.visreg <- function(
  x,
  overlay = FALSE,
  print.cond = FALSE,
  partial = identical(x$meta$trans, I),
  band = TRUE,
  rug = ifelse(partial, 0, 2),
  strip.names = is.numeric(x$fit[, x$meta$by]),
  top = c('line', 'points'),
  line = NULL,
  fill = NULL,
  points = NULL,
  ...
) {
  top <- match.arg(top)
  warn <- FALSE
  if (missing(print.cond)) {
    if (!("by" %in% names(x$meta)) & x$meta$hasInteraction) {
      print.cond <- warn <- TRUE
    }
  }
  if (print.cond) {
    printCond(x, warn)
  }

  if (all(is.na(x$res$visregRes))) {
    if (partial | rug) {
      partial <- FALSE
      rug <- FALSE
      warning(paste0(
        "The generic function residuals() is not set up for this type of model object.  To plot partial residuals, you will need to define your own residuals.",
        x$meta$class[1],
        "() function."
      ))
    }
  }

  if (is.factor(x$fit[, x$meta$x])) {
    ggFactorPlot(
      x,
      partial,
      band,
      rug,
      strip.names,
      overlay,
      top,
      line,
      fill,
      points,
      ...
    )
  } else {
    ggContPlot(
      x,
      partial,
      band,
      rug,
      strip.names,
      overlay,
      top,
      line,
      fill,
      points,
      ...
    )
  }
}
