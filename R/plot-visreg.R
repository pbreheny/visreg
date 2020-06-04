plot.visreg <- function(
  x, overlay=FALSE, print.cond=FALSE, whitespace=0.2, partial=identical(x$meta$trans, I),
  band=TRUE, rug=ifelse(partial, 0, 2), strip.names=is.numeric(x$fit[, x$meta$by]),
  legend=TRUE, top=c('line', 'points'), gg=FALSE, line.par=NULL, fill.par=NULL,
  points.par=NULL, ...) {
  
  top <- match.arg(top)
  warn <- FALSE
  if (missing(print.cond)) {
    if (!("by" %in% names(x$meta)) & x$meta$hasInteraction) print.cond <- warn <- TRUE
  }
  if (print.cond) printCond(x, warn)

  if (all(is.na(x$res$visregRes))) {
    partial <- FALSE
    rug <- FALSE
    warning(paste0("The generic function residuals() is not set up for this type of model object.  To plot partial residuals, you will need to define your own residuals.", x$meta$class[1], "() function."))
  }

  if (gg) {
    if (!requireNamespace("ggplot2")) stop("You must first install the ggplot2 package: install.packages('ggplot2')", call.=FALSE)
    if (is.factor(x$fit[, x$meta$x])) {
      p <- ggFactorPlot(x, partial, band, rug, whitespace, strip.names, overlay, top, line.par, fill.par, points.par, ...)
    } else {
      p <- ggContPlot(x, partial, band, rug, whitespace, strip.names, overlay, top, line.par, fill.par, points.par, ...)
    }
    return(p)
  } else {
    if ("by" %in% names(x$meta)) {
      if (overlay) {
        visregOverlayPlot(x, strip.names=strip.names, legend=legend, whitespace=whitespace, partial=partial, band=band, rug=rug, line.par=line.par, fill.par=fill.par, points.par=points.par, ...)
      } else {
        p <- visregLatticePlot(x, strip.names=strip.names, whitespace=whitespace, partial=partial, band=band, rug=rug, top=top, line.par=line.par, fill.par=fill.par, points.par=points.par, ...)
        return(invisible(p))
      }
    } else {
      visregPlot(x, whitespace=whitespace, partial=partial, band=band, rug=rug, top=top, line.par=line.par, fill.par=fill.par, points.par=points.par, ...)
    }
  }
}
