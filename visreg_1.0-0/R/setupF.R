setupF <- function(fit)
  {
    if (class(fit)[1]=="locfit")
      {
        f <- model.frame(fit)
        names(f) <- removeFormulaFormatting(names(f))
      }
    else
      {
        if ("data" %in% names(fit$call)) f <- as.data.frame(as.list(get_all_vars(fit,eval(fit$call$data,env=environment(fit$terms)))))
        else f <- as.data.frame(as.list(get_all_vars(fit,data=environment(fit$terms))))
      }
    suppressWarnings(f <- f[complete.cases(f),])

    ## Handle some variable type issues
    attr(f,"needs.update") <- FALSE
    if (any(sapply(model.frame(fit),class)=="character")) attr(f,"needs.update") <- TRUE
    if (any(sapply(f,class)=="logical"))
      {
        attr(f,"needs.update") <- TRUE
        for (j in 1:ncol(f)) if (class(f[,j])=="logical") f[,j] <- as.numeric(f[,j])
      }
    return(f)
  }

