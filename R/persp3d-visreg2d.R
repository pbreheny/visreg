# rgl::persp3d() method for visreg2d objects. Registered dynamically in .onLoad() (R/zzz.R)
# since rgl is Suggests-only; not exported as an S3method in NAMESPACE.
#
# @param x A visreg2d object.
# @param xlab,ylab,zlab Axis labels.
# @param color The color of the surface.
# @param whitespace Amount of space between factor levels on a factor axis (default 0.2).
# @param ... Additional graphical parameters, passed to rgl::persp3d().
persp3d.visreg2d <- function(
  x,
  xlab = NULL,
  ylab = NULL,
  zlab = NULL,
  color = "gray",
  whitespace = 0.2,
  ...
) {
  labs <- resolve_2d_labels(x, xlab, ylab, zlab, drop_expr_z = TRUE)
  axes <- prep_2d_axes(x, whitespace, gg = FALSE)

  plot_args <- list(
    x = axes$xx,
    y = axes$yy,
    z = axes$zz,
    xlab = labs$xlab,
    ylab = labs$ylab,
    zlab = labs$zlab,
    color = color
  )
  new_args <- list(...)
  if (length(new_args)) {
    plot_args[names(new_args)] <- new_args
  }
  do.call(rgl::persp3d, plot_args)
}
