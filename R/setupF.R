setupF <- function(fit, xvar, call.env) {
  if (class(fit)[1]=="locfit") {
    f <- model.frame(fit)
    for (j in 1:ncol(f)) names(f)[j] <- removeFormulaFormatting(names(f)[j])
  } else {
    if (is.null(fit$call$data)) {
      env <- NULL
      Data <- NULL
    } else if (exists(as.character(fit$call$data), call.env)) {
      env <- call.env
      Data <- eval(fit$call$data, envir=env)
    } else {
      env <- environment(fit$terms)
      Data <- eval(fit$call$data, envir=env)
    }
    f <- as.data.frame(as.list(get_all_vars(fit, Data)))
    if ("subset" %in% names(fit$call)) {
      s <- fit$call$subset
      subset <- eval(substitute(s), Data, env)
      f <- f[which(subset==TRUE),]
    } 
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
  attr(f, "needsUpdate") <- needsUpdate
  attr(f, "xvar") <- xvar
  f
}
