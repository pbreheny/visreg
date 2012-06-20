## Fix problem that occurs when variables are logical
fixLogical <- function(fit)
  {
    f <- model.frame(fit)
    if ("data" %in% names(fit$call))
      {
        data <- eval(fit$call$data,env=environment(fit$terms))
        f <- cbind(f,data[,setdiff(names(data),names(f))])
      }
    for (j in 1:ncol(f)) if (class(f[,j])=="logical") f[,j] <- as.numeric(f[,j])
    return(update(fit,data=f))
  }
