#' Subset a visreg object
#'
#' Subset a visreg object so that only a portion of the full model is plotted.
#'
#' @param x A `visreg` object.
#' @param sub Logical expression indicating elements to keep, as in [subset()]
#' @param ... Not used.
#'
#' @returns A [visreg()] object.
#'
#' @examples
#' # Fit a model and construct a visreg object
#' airquality$Heat <- cut(airquality$Temp, 3, labels = c("Cool", "Mild", "Hot"))
#' fit <- lm(Ozone ~ Solar.R + Wind * Heat, data = airquality)
#' v <- visreg(fit, "Wind", by = "Heat", plot = FALSE)
#'
#' # Plot only certain levels
#' subset(v, Heat %in% c("Cool", "Hot")) |> plot()
#'
#' # Plot only up to wind 15 mph
#' subset(v, Wind < 15) |> plot()
#'
#' @export
subset.visreg <- function(x, sub, ...) {
  x$fit <- x$fit[eval(match.call()$sub, x$fit), ]
  x$res <- x$res[eval(match.call()$sub, x$res), ]
  x
}
