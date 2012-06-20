centerFrame <- function(fit,name)
  {
    f <- model.frame(fit)
    f[,name] <- f[,name] - mean(f[,name])
    return(model.frame(as.formula(paste(formula(fit)[2],"~",formula(fit)[3])),f))
    ##return(update(fit,data=f))
  }
