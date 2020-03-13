visreg <- function(fit, xvar, by, breaks=3, type=c("conditional", "contrast"), data=NULL, trans=I,
                   scale=c("linear","response"), xtrans, alpha=.05, nn=101, cond=list(), jitter=FALSE, collapse=FALSE,
                   plot=TRUE, ...) {
  # Setup
  if (type[1]=="effect") {
    warning("Please note that type='effect' is deprecated and may not be supported in future versions of visreg.  Use type='contrast' instead.")
    type <- "contrast"
  }
  type <- match.arg(type)
  scale <- match.arg(scale)
  if (scale=="response") {
    if (inherits(fit, "lrm")) {
      trans <- binomial()$linkinv
    } else if (inherits(fit, "betareg")) {
      trans <- fit$link$mean$linkinv
    } else {
      trans <- family(fit)$linkinv
    }
  }
  if (!identical(trans, I) & type=="contrast") warning("You are attempting to transform a contrast.  The resulting plot is not guaranteed to be meaningful.", call.=FALSE)
  
  Data <- setupF(fit, xvar, parent.frame(), data)
  xvar <- attr(Data, "xvar")
  if (attr(Data, "needsUpdate")) {
    if (inherits(fit, 'coxph')) {
      fit <- update(fit, formula=formula(fit), data=Data, model=TRUE)
    } else {
      fit <- update(fit, formula=formula(fit), data=Data)
    }
  }
  cond <- setupCond(cond, Data, by, breaks)

  # Calculate v
  yName <- makeYName(fit, scale, trans, type)
  v <- setupV(fit, Data, xvar, nn, cond, type, trans, xtrans, alpha, jitter, by, yName, ...)
  if (collapse) v <- collapse.visregList(v)

  # Plot/return
  if (plot) {
    p <- plot(v, ...)
    if (!is.null(p) && inherits(p, 'gg')) return(p)
    if (!is.null(p) && inherits(p, 'list') && inherits(p[[1]], 'gg')) return(p)
  }
  invisible(v)
}
