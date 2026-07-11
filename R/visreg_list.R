#' Join multiple visreg objects together in a list
#'
#' This function takes multiple visreg objects, from separate calls to [visreg()], and joins them
#' together in a single object. The single object will be of type `visreg_list` unless
#' `collapse=TRUE` is specified, in which case the list will be collapsed back down into a single
#' `visreg` object.
#'
#' @param ... `visreg` objects, as produced by calls to [visreg()].
#' @param labels A character vector with length corresponding to the number of `visreg` objects
#'   passed to the function that provides labels for the different objects in subsequent plots. Only
#'   has an effect if `collapse=TRUE`.
#' @param collapse If `TRUE`, the resulting object will be collapsed down into a single `visreg`
#'   object. If `FALSE`, the resulting object will be a `visreg_list`.
#'
#' @returns A `visreg` or `visreg_list` object, depending on the value of `collapse`.
#'
#' @seealso [visreg()], [plot.visreg()]
#'
#' @examples
#' fit <- lm(Ozone ~ Solar.R + Wind + Temp, data = airquality)
#' v1 <- visreg(fit, "Wind", plot = FALSE, alpha = 0.2)
#' v2 <- visreg(fit, "Wind", plot = FALSE, alpha = 0.01)
#' vv1 <- visreg_list(v1, v2, collapse = FALSE)
#' vv2 <- visreg_list(v1, v2,
#'   collapse = TRUE,
#'   labels = c("Confidence: 0.80", "Confidence: 0.99")
#' )
#' op <- par(mfrow = c(1, 2))
#' plot(vv1)
#' par(op)
#' plot(vv2)
#'
#' @export
visreg_list <- function(..., labels, collapse = FALSE) {
  out <- structure(list(...), class = "visreg_list")
  if (collapse) {
    out <- collapse_visreg_list(out, labels = labels)
  }
  out
}
