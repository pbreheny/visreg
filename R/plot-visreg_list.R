#' @rdname plot.visreg
#' @export
plot.visreg_list <- function(x, ...) {
  lapply(x, plot, ...) |> invisible()
}

#' @rdname persp.visreg2d @exportS3Method persp visreg_list
persp.visreg_list <- function(x, ...) {
  lapply(x, persp, ...) |> invisible()
}

# rgl::persp3d() method for visreg_list objects; registered dynamically in .onLoad() (R/zzz.R).
persp3d.visreg_list <- function(x, ...) {
  lapply(x, rgl::persp3d, ...) |> invisible()
}
