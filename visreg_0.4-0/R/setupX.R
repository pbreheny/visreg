setupX <- function(fit,f,name,fill,nn,cond)
  {
    ## Set up n x p matrix for (conditional) partial residuals
    x <- f[,name]
    attr(x,"name") <- name
    if (!is.factor(x))
        {
          meanx <- mean(x)
          x <- x-meanx
        }
    df <- fillFrame(f,x,fill,cond)
    X <- model.matrix(as.formula(paste("~",formula(fit)[3])),df)[,is.finite(coef(fit))]

    ## Set up data frame with nn rows for prediction
    if(is.factor(x)) xx <- factor(levels(x),levels=levels(x))
    else xx <- seq(min(x),max(x),length=nn)
    attr(xx,"name") <- name
    df <- fillFrame(f,xx,fill,cond)
    XX <- model.matrix(as.formula(paste("~",formula(fit)[3])),df)[,is.finite(coef(fit))]
    if (!is.factor(x))
      {
        x <- x+meanx
        xx <- xx+meanx
      }
    
    X[,"(Intercept)"] <- XX[,"(Intercept)"] <- 0
    return(list(x=x,xx=xx,X=X,XX=XX))
  }
