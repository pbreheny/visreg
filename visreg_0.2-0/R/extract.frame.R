extract.frame <- function(fit)
  {
    f <- model.frame(fit)
    if ("data" %in% names(fit$call))
      {
        data <- eval(fit$call$data,env=environment(fit$terms))
        f <- cbind(f,data[,setdiff(names(data),names(f))])
      }
    return(f)
  }
