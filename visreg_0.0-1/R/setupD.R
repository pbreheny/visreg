setupD <- function(fit,name,fill,nn)
  {
    ## Set up n-row data frame for residuals
    f <- model.frame(fit)
    x <- f[,name]
    attr(x,"name") <- name
    D <- model.frame(as.formula(paste("~",formula(fit)[3])),fillFrame(fit,x,fill))

    ## Set up nn-row data frame for prediction
    xx <- seq(min(x),max(x),length=nn)
    attr(xx,"name") <- name
    DD <- model.frame(as.formula(paste("~",formula(fit)[3])),fillFrame(fit,xx,fill))

    return(list(x=x,xx=xx,D=D,DD=DD))
  }
