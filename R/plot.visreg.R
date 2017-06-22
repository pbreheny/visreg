plot.visreg <- function(x, overlay=FALSE, print.cond=FALSE, whitespace=0.2, partial=identical(x$meta$trans, I), band=TRUE, rug=!partial,
                        strip.names=is.numeric(x$fit[,x$meta$by]), legend=TRUE, line.par=NULL, fill.par=NULL,
                        points.par=NULL, gg=FALSE, ...) {
  warn <- FALSE
  if (missing(print.cond)) {
    if (!("by" %in% names(x$meta)) & x$meta$hasInteraction) print.cond <- warn <- TRUE
  }
  if (print.cond) printCond(x, warn)

  if (gg) {
    if (!requireNamespace("ggplot2")) stop("You must first install the ggplot2 package: install.packages('ggplot2')")
    if (is.factor(x$fit[,x$meta$x])) {
      p <- ggFactorPlot(x, partial, band, rug, whitespace, strip.names, line.par, fill.par, points.par, ...)
    } else {
      p <- ggContPlot(x, partial, band, rug, whitespace, strip.names, line.par, fill.par, points.par, ...)
    }
    return(p)
  } else {
    if ("by" %in% names(x$meta)) {
      if (overlay) {
        visregOverlayPlot(x, strip.names=strip.names, legend=legend, whitespace=whitespace, partial=partial, band=band, rug=rug, line.par=line.par, fill.par=fill.par, points.par=points.par, ...)
      } else {
        p <- visregLatticePlot(x, strip.names=strip.names, whitespace=whitespace, partial=partial, band=band, rug=rug, line.par=line.par, fill.par=fill.par, points.par=points.par, ...)
        return(invisible(p))
      }
    } else {
      visregPlot(x, whitespace=whitespace, partial=partial, band=band, rug=rug, line.par=line.par, fill.par=fill.par, points.par=points.par, ...)
    }
  }
}
