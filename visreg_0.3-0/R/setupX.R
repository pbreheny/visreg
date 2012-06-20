setupX <- function(fit,f,name,fill,nn,cond)
  {
    ## Set up n x p matrix for conditional partial residuals
    x <- f[,name]
    if(is.factor(x))
      {
        l = length(x)
        tx <- as.vector(rep(levels(x)[1],l))
        for(i in 2:length(levels(x)))
          {
            tx <- c(tx,rep(levels(x)[i],l))
          }
        x <- factor(tx,levels=levels(x))
      }
    attr(x,"name") <- name
    meanx <- mean(x)
    x <- x-meanx
    df <- fillFrame(f,x,fill,cond)
    ##print(sapply(df,levels))
    X <- model.matrix(as.formula(paste("~",formula(fit)[3])),df)

    ## Set up data frame with nn rows for prediction
    xx <- seq(min(x),max(x),length=nn)
    attr(xx,"name") <- name
    df <- fillFrame(f,xx,fill,cond)
    XX <- model.matrix(as.formula(paste("~",formula(fit)[3])),df)
    x <- x+meanx
    xx <- xx+meanx
    
    X[,"(Intercept)"] <- XX[,"(Intercept)"] <- 0
    return(list(x=x,xx=xx,X=X,XX=XX))
  }
