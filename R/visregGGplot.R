visregGGplot <- function(v, partial, band, rug, whitespace, strip.names, line.par, fill.par, points.par, ...) {

  # Setup data frames
  xx <- v$fit[,v$meta$x]
  bb <- v$fit[,v$meta$by]
  fillData <- data.frame(x = c(xx, rev(xx)),
                         y = c(v$fit$visregLwr, rev(v$fit$visregUpr)),
                         z = c(bb, rev(bb)))
  if (is.factor(bb)) {
    fillData$z <- factor(fillData$z, labels=levels(bb))
  }

  lineData <- data.frame(x = xx,
                         y = v$fit$visregFit,
                         z = bb)
  pointData <- data.frame(x = v$res[,v$meta$x],
                          y = v$res$visregRes,
                          z = v$res[,v$meta$by])
  names(fillData)[3] <-names(lineData)[3] <- names(pointData)[3] <- v$meta$by

  xlab <- v$meta$x
  ylab <- if (is.null(v$meta$yName)) paste("f(", v$meta$x, ")", sep="") else v$meta$yName

  p <- qplot(x=x, y=y, data=lineData, xlab=xlab, ylab=ylab, geom="blank")
  if (band) {
    p <- p + geom_polygon(fill="gray", data=fillData)
  }
  if (partial) {
    p <- p + geom_point(data=pointData)
  }
  if (is.factor(xx)) {
    for (i in 1:length(levels(xx))) {
      ldi <- subset(lineData, x==levels(xx)[i])
      ldi$x <-a
      p <- p + geom_line(data=, col="#008DFFFF", size=2)
    }
  } else {
    p <- p + geom_line(data=lineData, col="#008DFFFF", size=2)
  }


  form <- as.formula(paste("~", v$meta$by))
  if (strip.names==TRUE) {
    p <- p + facet_grid(form, labeller=label_both)
  } else {
    p <- p + facet_grid(form)
  }

  plot(p)
  return(p)
}
