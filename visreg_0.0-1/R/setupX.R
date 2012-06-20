setupX <- function(fit,name,fill,nn)
  {
    ## Set up n x p matrix for conditional partial residuals
    f <- model.frame(fit)
    x <- f[,name]
    attr(x,"name") <- name
    meanx <- mean(x)
    x <- x-meanx
    X <- model.matrix(as.formula(paste("~",formula(fit)[3])),fillFrame(fit,x,fill))

    ## Set up data frame with nn rows for prediction
    xx <- seq(min(x),max(x),length=nn)
    attr(xx,"name") <- name
    XX <- model.matrix(as.formula(paste("~",formula(fit)[3])),fillFrame(fit,xx,fill))
    x <- x+meanx
    xx <- xx+meanx
    
    X[,"(Intercept)"] <- XX[,"(Intercept)"] <- 0
    return(list(x=x,xx=xx,X=X,XX=XX))
  }
