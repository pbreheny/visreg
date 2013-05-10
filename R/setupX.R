setupX <- function(fit,f,name,nn,cond)
{
  ## Set up n x p matrix for (conditional) partial residuals
  x <- f[,name]
  x <- if (is.factor(x)) factor(c(1,as.integer(x)),labels=levels(x)) else c(mean(x),x)
  xdf <- data.frame(x)
  names(xdf) <- name
  df <- fillFrame(f,xdf,cond)
  D <- rbind(f,df)
  form <- formula(fit)[3]
  
  if (class(fit)[1]=="mlm") {
    ind <- apply(is.finite(coef(fit)), 1, all)
    if (!identical(ind, apply(is.finite(coef(fit)), 1, any))) stop("Inconsistent NA/NaN coefficients across outcomes")
  } else ind <- is.finite(coef(fit))
  if (class(fit)[1]=="gam") {
    form <- removeFormulaFormatting(formula(fit)[3])
    D <- model.frame(as.formula(paste("~",form)),df)
    X. <- predict(fit, newdata=as.list(D), type="lpmatrix")    
  } else X. <- model.matrix(as.formula(paste("~",form)),D)[-(1:nrow(f)), ind]
  X <- t(t(X.[-1,])-X.[1,])
  
  ## Set up data frame with nn rows for prediction
  if(is.factor(x)) xx <- factor(c(1,1:length(levels(x))),labels=levels(x))
  else xx <- c(mean(x),seq(min(x),max(x),length=nn))
  xxdf <- data.frame(xx)
  names(xxdf) <- name
  df <- fillFrame(f,xxdf,cond)
  DD <- rbind(f,df)
  if (class(fit)[1]=="gam") {
    DD <- model.frame(as.formula(paste("~",form)),df)
    XX. <- predict(fit, newdata=as.list(DD), type="lpmatrix")
  } else XX. <- model.matrix(as.formula(paste("~",form)),DD)[-(1:nrow(f)), ind]
  XX <- t(t(XX.[-1,])-XX.[1,])
  
  ## Remove extraneous intercept for coxph
  if (class(fit)[1]=="coxph")
  {
    XX <- XX[,-which(colnames(XX)=="(Intercept)"),drop=FALSE]
    X <- X[,-which(colnames(X)=="(Intercept)"),drop=FALSE]
  }
  
  return(list(x=x[-1], xx=xx[-1], X=X, XX=XX, factor=is.factor(x), name=name))
}
