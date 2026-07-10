#' @rdname plot.visreg
#' @export
plot.visreg_list <- function(x, ...) {
  lapply(x, plot, ...) |> invisible()
}
