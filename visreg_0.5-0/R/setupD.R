setupD <- function(fit,f,name,nn,cond,whitespace)
  {
    ## Set up n-row data frame for residuals
    x <- f[,name]
    attr(x,"name") <- name
    df <- fillFrame(f,x,cond)
    form <- removeFormulaFormatting(formula(fit)[3])
    D <- model.frame(as.formula(paste("~",form)),df)
    D <- cbind(D,df[,setdiff(names(df),names(D)),drop=FALSE])

    ## Set up nn-row data frame for prediction
    if(is.factor(x)) xx <- factor(levels(x),levels=levels(x))
    else xx <- seq(min(x),max(x),length=nn)
    attr(xx,"name") <- name
    df <- fillFrame(f,xx,cond)
    DD <- model.frame(as.formula(paste("~",form)),df)
    DD <- cbind(DD,df[,setdiff(names(df),names(DD)),drop=FALSE])

    return(list(x=x,xx=xx,D=D,DD=DD))
  }
