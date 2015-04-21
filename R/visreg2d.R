visreg2d <- function(fit, xvar, yvar, type=c("conditional", "contrast", "effect"), trans=I, scale=c("linear","response"), 
                     plot.type=c("image","persp","rgl"), nn=ifelse(plot.type=="persp",49,99), cond=list(), print.cond=FALSE, whitespace=0.2, ...) {
  ## Setup
  type <- match.arg(type)
  scale <- match.arg(scale)
  plot.type <- match.arg(plot.type)
  if (scale=="response") trans <- family(fit)$linkinv
  if (missing(xvar) | missing(yvar)) stop("Must specify and x and y variable")
  if (type=="effect") {
    warning("Please note that type='effect' is deprecated and may not be supported in future versions of visreg.  Use type='contrast' instead.")
    type <- "contrast"
  }
  
  ## Set up f
  f <- setupF(fit, c(xvar, yvar), parent.frame())
  if (attr(f, "needsUpdate")) fit <- update(fit, data=f)
  cond <- setupCond(cond, f)[[1]]

  ## Calculate v
  v <- setupV2(fit, f, xvar, yvar, nn, cond, type, trans)
  zName <- makeYName(fit, scale, trans, type)
  if (plot.type %in% c("persp", "rgl") & is.expression(zName)) zName <- NULL ## persp cannot handle expressions
  
  for (i in 1:v$n) {
    z <- if (v$n > 1) v$z[[i]] else v$z
    zlab <- if (is.null(zName)) paste("f(", xvar, ", ", yvar, ")", sep="") else zName[i]

    ## Make factor axes
    mx <- my <- NULL
    lx <- ly <- TRUE
    if (is.factor(v$x)) {
      xAxis <- factorAxis(v$x, whitespace, nn)
      x <- xAxis$x
      mx <- xAxis$m
      lx <- xAxis$l
      z <- z[xAxis$ind,]
    } else {
      x <- v$x
    }
    if (is.factor(v$y)) {
      yAxis <- factorAxis(v$y,whitespace,nn)
      y <- yAxis$x
      my <- yAxis$m
      ly <- yAxis$l
      z <- z[,yAxis$ind]
    } else {
      y <- v$y
    }
    xlim <- if (is.factor(v$x)) c(0,1) else range(v$x)
    ylim <- if (is.factor(v$y)) c(0,1) else range(v$y)
    
    if (plot.type=="image") {
      color.palette=colorRampPalette(c(pal(3)[3],"gray90",pal(3)[1]),space="Lab")
      plot.args <- list(x=x, y=y, z=z, xlim=xlim, ylim=ylim, xlab=xvar, ylab=yvar, color.palette=color.palette, main=zlab)
      plot.args$plot.axes <- quote({axis(1,at=mx,labels=lx);axis(2,at=my,labels=ly)})
      new.args <- list(...)
      if (length(new.args)) plot.args[names(new.args)] <- new.args
      do.call("filled.contour", plot.args)
    } else if (plot.type=="persp") {
      ticktype <- ifelse(is.factor(v$x) | is.factor(v$y),"simple","detailed")
      plot.args <- list(x=x, y=y, z=z, xlim=xlim, ylim=ylim, xlab=xvar, ylab=yvar, zlab=zlab, ticktype=ticktype, theta=-30)
      new.args <- list(...)
      if (length(new.args)) plot.args[names(new.args)] <- new.args
      do.call("persp", plot.args)
    } else if (plot.type=="rgl") {
      plot.args <- list(x=x, y=y, z=z, xlab=xvar, ylab=yvar, zlab=zlab, color="gray")
      new.args <- list(...)
      if (length(new.args)) plot.args[names(new.args)] <- new.args
      if (i >= 2) rgl::open3d()
      FUN <- get("persp3d", asNamespace("rgl"))
      do.call(FUN, plot.args)
    }
  }
  if (print.cond) printCond(list(list(x=list(cond=v$cond))))
  invisible(v)
}
