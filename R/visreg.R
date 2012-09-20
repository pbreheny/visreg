## To do: add overlay plot option?
visreg <- function(fit, xvar, by, breaks=4, type=c("conditional","effect"), trans=I, scale=c("linear","response"), xtrans, alpha=.05, nn=101, cond=list(), whitespace=0.2, partial=TRUE, jitter=FALSE, strip.names=FALSE, line.par=NULL, fill.par=NULL, points.par=NULL, ...)
{
  if (missing(type) & class(fit)[1]=="coxph") type <- "effect"
  type <- match.arg(type)
  scale <- match.arg(scale)
  if (!missing(by) & !missing(cond)) stop("Cannot specify 'by' and 'cond' simultaneously")
  if (scale=="response") trans <- family(fit)$linkinv
  
  f <- setupF(fit)
  if (missing(xvar)) xvar <- names(f)[-1]
  for (i in 1:length(xvar)){if (!is.element(xvar[i],names(f))) stop(paste(xvar[i],"not in model"))}
  if ((!missing(by) & missing(xvar)) | (!missing(by) & (length(xvar) > 1))) stop("Cannot specify 'by' and multiple x variables simultaneously")
  if (!missing(by) && is.numeric(f[,by]) && length(levels(as.factor(f[,by]))) <= breaks) {
    attr(f,"needs.update") <- TRUE
    f[,by] <- as.factor(f[,by])
    warning("'By' variable has too few unique values and has been coerced to a factor")
  }
  if (attr(f,"needs.update")) fit <- update(fit,data=f)
  cond <- setupCond(cond,f,by,breaks)
  
  n.y <- if (class(fit)[1]=="mlm") ncol(coef(fit)) else 1
  n.plots <- length(xvar) * n.y
  print(n.plots)
  if (n.plots > 1 && prod(par("mfcol")) < n.plots && dev.interactive()) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
  
  if (missing(by))
  {
    v <- vector("list",length(xvar))
    for (i in 1:length(xvar))
    {
      v[[i]] <- visregPlot(fit, f, xvar[i], nn, cond[[1]], type, trans, xtrans, alpha, jitter, partial, whitespace, line.par, fill.par, points.par, ...)
    }
    names(v) <- xvar
    if (length(xvar)==1) v <- v[[1]]
  }
  else
  {
    v <- visregLatticePlot(fit, f, xvar, nn, cond, type, trans, xtrans, alpha, jitter, partial, whitespace, by, strip.names, line.par, fill.par, points.par, ...)
  }
  return(invisible(v))
}
