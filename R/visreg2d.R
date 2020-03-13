visreg2d <- function(fit, xvar, yvar, type=c("conditional", "contrast"), data=NULL, trans=I, scale=c("linear","response"), nn=99, cond=list(), plot=TRUE, ...) {
  # Setup
  if (type[1]=="effect") {
    warning("Please note that type='effect' is deprecated and may not be supported in future versions of visreg.  Use type='contrast' instead.")
    type <- "contrast"
  }
  type <- match.arg(type)
  scale <- match.arg(scale)
  if (scale=="response") trans <- family(fit)$linkinv
  if (missing(xvar) | missing(yvar)) stop("Must specify and x and y variable", call.=FALSE)
  if (!identical(trans, I) & type=="contrast") warning("You are attempting to transform a contrast.  The resulting plot is not guaranteed to be meaningful.", call.=FALSE)
  
  # Set up f
  f <- setupF(fit, c(xvar, yvar), parent.frame(), data)
  if (attr(f, "needsUpdate")) fit <- update(fit, data=f)
  cond <- setupCond(cond, f)[[1]]

  # Calculate v
  v <- setupV2(fit, f, xvar, yvar, nn, cond, type, scale, trans)

  # Plot/return
  if (plot) {
    p <- plot(v, ...)
    if (!is.null(p) && inherits(p, 'gg') || inherits(p, 'list')) return(p)
  }
  invisible(v)
}
