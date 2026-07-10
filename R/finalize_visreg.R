finalize_visreg <- function(v, plot, ...) {
  if (!plot) {
    return(invisible(v))
  }
  p <- plot(v, ...)
  if (!is.null(p) && inherits(p, "gg")) {
    return(p)
  }
  if (!is.null(p) && inherits(p, "list") && inherits(p[[1]], "gg")) {
    return(p)
  }
  invisible(v)
}
