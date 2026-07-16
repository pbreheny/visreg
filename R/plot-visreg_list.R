#' @rdname plot.visreg
#' @export
plot.visreg_list <- function(x, ...) {
  structure(lapply(x, plot, ...), class = c("visreg_plot_list", "list"))
}

#' @rdname plot.visreg
#' @export
print.visreg_plot_list <- function(x, ...) {
  for (p in x) print(p)
  invisible(x)
}

#' @rdname persp.visreg2d
#' @export
persp.visreg_list <- function(x, ...) {
  lapply(x, persp, ...) |> invisible()
}

#' @rdname persp.visreg2d
#' @export
persp3d.visreg_list <- function(x, ...) {
  lapply(x, rgl::persp3d, ...) |> invisible()
}
