setupD <- function(fit,name,fill,nn)
  {
    if ("data" %in% names(fit$call)) data <- eval(fit$call$data,env=environment(fit$terms))
    f <- model.frame(fit)
    f <- cbind(f,data[,setdiff(names(data),names(f))])

    ## Set up n-row data frame for residuals
    x <- f[,name]
    attr(x,"name") <- name
    df <- fillFrame(f,x,fill)
    D <- model.frame(as.formula(paste("~",formula(fit)[3])),df)
    D <- cbind(D,df[,setdiff(names(df),names(D))])

    ## Set up nn-row data frame for prediction
    xx <- seq(min(x),max(x),length=nn)
    attr(xx,"name") <- name
    df <- fillFrame(f,xx,fill)
    DD <- model.frame(as.formula(paste("~",formula(fit)[3])),df)
    DD <- cbind(DD,df[,setdiff(names(df),names(DD))])

    return(list(x=x,xx=xx,D=D,DD=DD))
  }
