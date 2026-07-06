#' @export
plot.visregList <- function(x, ...) {
  lapply(x, plot, ...)
}
