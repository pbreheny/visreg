#' @rdname plot.visreg
#' @export
plot.visreg_list <- function(x, ...) {
  lapply(x, plot, ...) |> invisible()
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
