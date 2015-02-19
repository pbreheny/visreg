plot.visreg <- function(x, overlay=FALSE, print.cond=FALSE, whitespace=0.2, partial=identical(x$meta$trans, I), band=TRUE, rug=!partial,
                        strip.names=is.numeric(x$fit[,x$meta$by]), legend=TRUE, line.par=NULL, fill.par=NULL,
                        points.par=NULL, ...) {
  warn <- FALSE
  if (missing(print.cond)) {
    if (!("by" %in% names(x$meta)) & x$meta$hasInteraction) print.cond <- warn <- TRUE
  }
  if (print.cond) printCond(x, warn)

  if ("by" %in% names(x$meta)) {
    if (overlay) {
      visregOverlayPlot(x, strip.names=strip.names, legend=legend, whitespace=whitespace, partial=partial, band=band, rug=rug, line.par=line.par, fill.par=fill.par, points.par=points.par, ...)
    } else {
      tp <- visregLatticePlot(x, strip.names=strip.names, whitespace=whitespace, partial=partial, band=band, rug=rug, line.par=line.par, fill.par=fill.par, points.par=points.par, ...)
      return(invisible(tp))
    }
  } else {
    visregPlot(x, whitespace=whitespace, partial=partial, band=band, rug=rug, line.par=line.par, fill.par=fill.par, points.par=points.par, ...)
  }
}
