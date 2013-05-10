setupF <- function(fit, xvar, by) {
  if (class(fit)[1]=="locfit") {
    f <- model.frame(fit)
    for (j in 1:ncol(f)) names(f)[j] <- removeFormulaFormatting(names(f)[j])
  } else {
    if ("data" %in% names(fit$call)) {
      f <- as.data.frame(as.list(get_all_vars(fit,eval(fit$call$data,envir=environment(fit$terms)))))
    } else f <- as.data.frame(as.list(get_all_vars(fit,data=environment(fit$terms))))
  }
  suppressWarnings(f <- f[!apply(is.na(f), 1, any),])
  
  ## Handle some variable type issues
  needsUpdate <- FALSE
  if (any(sapply(model.frame(fit),class)=="character")) needsUpdate <- TRUE
  if (any(sapply(f,class)=="logical")) {
    needsUpdate <- TRUE
    for (j in 1:ncol(f)) if (class(f[,j])[1]=="logical") f[,j] <- as.numeric(f[,j])
  }
  if (missing(xvar)) xvar <- names(f)[-1]
  for (i in 1:length(xvar)){if (!is.element(xvar[i],names(f))) stop(paste(xvar[i],"not in model"))}
  if (!missing(by) & (length(xvar) > 1)) stop("Cannot specify 'by' and multiple x variables simultaneously")
  attr(f, "needsUpdate") <- needsUpdate
  attr(f, "xvar") <- xvar
  f
}
