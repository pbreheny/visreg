## To-do: modify ylab for glms
## Fix cond() for models with interactions
## Add option for panel name :
## Add padding to ylim
## Add overlay plot option?
visreg <- function(fit,xvar,by,breaks=4,fill=c("mean","zero","median"),type=c("conditional","effect"),trans=as.numeric,scale=c("linear","response"),xtrans,alpha=.05,nn=101,cond=list(),whitespace=0.2,partial=TRUE,jitter=FALSE,...)
  {
    fill <- match.arg(fill)
    type <- match.arg(type)
    scale <- match.arg(scale)
    if ((!missing(by) & missing(xvar)) | (!missing(by) & (length(xvar) > 1))) stop("Cannot specify 'by' and multiple x variables simultaneously")
    if (!missing(by) & !missing(cond)) stop("Cannot specify 'by' and 'cond' simultaneously")
    if (scale=="response") trans <- family(fit)$linkinv
    if (type=="effect") fill <- "zero"

    ## Set up f
    if ("data" %in% names(fit$call)) f <- get_all_vars(fit,eval(fit$call$data,env=environment(fit$terms)))
    else f <- get_all_vars(fit)
    suppressWarnings(f <- f[complete.cases(f),])

    ## Convert logical variables to numeric
    if (any(sapply(f,class)=="logical"))
      {
        for (j in 1:ncol(f)) if (class(f[,j])=="logical") f[,j] <- as.numeric(f[,j])
        fit <- update(fit,data=f)
      }
    if (!missing(by) && is.numeric(f[,by]) && length(levels(as.factor(f[,by]))) <= breaks)
      {
        f[,by] <- as.factor(f[,by])
        fit <- update(fit,data=f)
        warning("'By' variable has too few unique values and has been coerced to a factor")
      }
    cond <- setupCond(cond,by,f,breaks)

    if (missing(xvar)) xvar <- names(f)[-1]
    if (length(xvar) > 1)
      {
        oask <- devAskNewPage(TRUE)
        on.exit(devAskNewPage(oask))
      }

    if (missing(by))
      {
        v <- vector("list",length(xvar))
        for (i in 1:length(xvar))
          {
            v[[i]] <- visregPlot(fit,f,xvar[i],fill,nn,cond[[1]],type,trans,xtrans,alpha,jitter,partial,whitespace,...)
          }
        names(v) <- xvar
        if (length(xvar)==1) v <- v[[1]]
      }
    else
      {
        v <- visregLatticePlot(fit,f,xvar,fill,nn,cond,type,trans,xtrans,alpha,jitter,partial,whitespace,by,...)
      }
    return(invisible(v))
  }
