# Grid/axis layout shared by plot.visreg2d(), persp.visreg2d(), and persp3d.visreg2d().
# Lays factor axes out on [0, 1] via factorAxis2d() and reorders z accordingly; numeric
# axes pass through unchanged.
prep_2d_axes <- function(v, whitespace, gg = FALSE) {
  zz <- v$z
  if (gg) {
    mx <- my <- lx <- ly <- ggplot2::waiver()
  } else {
    mx <- my <- NULL
    lx <- ly <- TRUE
  }

  if (is.factor(v$x)) {
    x_axis <- factorAxis2d(v$x, whitespace, 99)
    xx <- x_axis$x
    mx <- x_axis$m
    lx <- x_axis$l
    zz <- zz[x_axis$ind, ]
  } else {
    xx <- v$x
  }
  if (is.factor(v$y)) {
    y_axis <- factorAxis2d(v$y, whitespace, 99)
    yy <- y_axis$x
    my <- y_axis$m
    ly <- y_axis$l
    zz <- zz[, y_axis$ind]
  } else {
    yy <- v$y
  }
  xlim <- if (is.factor(v$x)) c(0, 1) else range(v$x)
  ylim <- if (is.factor(v$y)) c(0, 1) else range(v$y)

  list(xx = xx, yy = yy, zz = zz, xlim = xlim, ylim = ylim, mx = mx, my = my, lx = lx, ly = ly)
}

# Label defaulting shared by the same three renderers. `drop_expr_z` handles the fact that
# persp()/persp3d() cannot use an expression as a zlab, unlike ggplot2.
resolve_2d_labels <- function(v, xlab, ylab, zlab, drop_expr_z = FALSE) {
  if (is.null(xlab)) xlab <- v$meta$x
  if (is.null(ylab)) ylab <- v$meta$y
  if (is.null(zlab)) {
    z_meta <- v$meta$z
    if (drop_expr_z && is.expression(z_meta)) z_meta <- NULL
    zlab <- if (is.null(z_meta)) paste0("f(", v$meta$x, ", ", v$meta$y, ")") else z_meta
  }
  list(xlab = xlab, ylab = ylab, zlab = zlab)
}
