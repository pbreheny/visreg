setupD <- function(fit,f,name,fill,nn,cond,whitespace)
  {
    ## Set up n-row data frame for residuals
    x <- f[,name]
    attr(x,"name") <- name
    df <- fillFrame(f,x,fill,cond)
    if (class(fit)[1]=="gam")
      {
        form <- gsub("s\\((.+),.*\\)","\\1",formula(fit)[3])
        form <- gsub("s\\((.+)\\)","\\1",formula(fit)[3])
      }
    else form <- formula(fit)[3]
    D <- model.frame(as.formula(paste("~",form)),df)
    D <- cbind(D,df[,setdiff(names(df),names(D)),drop=FALSE])

    ## Set up nn-row data frame for prediction
    if(is.factor(x)) xx <- factor(levels(x),levels=levels(x))
    else xx <- seq(min(x),max(x),length=nn)
    attr(xx,"name") <- name
    df <- fillFrame(f,xx,fill,cond)
    DD <- model.frame(as.formula(paste("~",form)),df)
    DD <- cbind(DD,df[,setdiff(names(df),names(DD)),drop=FALSE])

    return(list(x=x,xx=xx,D=D,DD=DD))
  }
