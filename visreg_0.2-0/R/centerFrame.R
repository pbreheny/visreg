centerFrame <- function(fit,name)
  {
    if ("data" %in% names(fit$call)) data <- eval(fit$call$data,env=environment(fit$terms))
    f <- model.frame(fit)
    f <- cbind(f,data[,setdiff(names(data),names(f))])
    
    f[,name] <- f[,name] - mean(f[,name])
    x <- as.formula(paste(formula(fit)[2],"~",formula(fit)[3]))
    df <- model.frame(as.formula(paste(formula(fit)[2],"~",formula(fit)[3])),f)
    df <- cbind(df,f[,setdiff(names(f),names(df))])
    return(df)
    ##return(model.frame(as.formula(paste(formula(fit)[2],"~",formula(fit)[3])),f))
  }
