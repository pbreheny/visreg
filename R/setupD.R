setupD <- function(fit,f,name,nn,cond,whitespace)
{
  ## Set up n-row data frame for residuals
  x <- f[,name]
  xdf <- data.frame(x)
  names(xdf) <- name
  df <- fillFrame(f,xdf,cond)
  
  form <- removeFormulaFormatting(formula(fit)[3])
  D <- model.frame(as.formula(paste("~",form)),df)
  D <- cbind(D,df[,setdiff(names(df),names(D)),drop=FALSE])
  
  ## Set up nn-row data frame for prediction
  xx <- if (is.factor(x)) factor(levels(x),levels=levels(x)) else seq(min(x),max(x),length=nn)
  xxdf <- data.frame(xx)
  names(xxdf) <- name
  df <- fillFrame(f,xxdf,cond)
  DD <- model.frame(as.formula(paste("~",form)),df)
  DD <- cbind(DD,df[,setdiff(names(df),names(DD)),drop=FALSE])
  
  return(list(x=x,xx=xx,D=D,DD=DD))
}
