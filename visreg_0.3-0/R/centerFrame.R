centerFrame <- function(fit,f,name)
  {
    f[,name] <- f[,name] - mean(f[,name])
    x <- as.formula(paste(formula(fit)[2],"~",formula(fit)[3]))
    df <- model.frame(as.formula(paste(formula(fit)[2],"~",formula(fit)[3])),f)
    df <- cbind(df,f[,setdiff(names(f),names(df))])
    return(df)
  }
