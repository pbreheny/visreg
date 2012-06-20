setupX <- function(fit,f,name,nn,cond)
  {
    ## Set up n x p matrix for (conditional) partial residuals
    x <- f[,name]
    if (is.factor(x)) x <- factor(c(1,as.integer(x)),labels=levels(x))
    else x <- c(mean(x),x)
    xdf <- data.frame(x)
    names(xdf) <- name
    df <- fillFrame(f,xdf,cond)
    D <- rbind(f,df)
    form <- removeFormulaFormatting(formula(fit)[3])
    X. <- model.matrix(as.formula(paste("~",form)),D)[-(1:nrow(f)),is.finite(coef(fit))]
    X <- t(t(X.[-1,])-X.[1,])

    ## Set up data frame with nn rows for prediction
    if(is.factor(x)) xx <- factor(c(1,1:length(levels(x))),labels=levels(x))
    else xx <- c(mean(x),seq(min(x),max(x),length=nn))
    xxdf <- data.frame(xx)
    names(xxdf) <- name
    df <- fillFrame(f,xxdf,cond)
    DD <- rbind(f,df)
    XX. <- model.matrix(as.formula(paste("~",formula(fit)[3])),DD)[-(1:nrow(f)),is.finite(coef(fit))]
    XX <- t(t(XX.[-1,])-XX.[1,])

    ## Remove extraneous intercept for coxph
    if (class(fit)=="coxph")
      {
        XX <- XX[,-which(colnames(XX)=="(Intercept)"),drop=FALSE]
        X <- X[,-which(colnames(X)=="(Intercept)"),drop=FALSE]
      }

    return(list(x=x[-1],xx=xx[-1],X=X,XX=XX))
  }
