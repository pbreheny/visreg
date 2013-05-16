setupD <- function(fit, f, name, nn, cond, whitespace, ...) {
  ## Set up n-row data frame for residuals
  x <- f[,name]
  xdf <- data.frame(x)
  names(xdf) <- name
  df <- fillFrame(f,xdf,cond)
  
  form <- removeFormulaFormatting(formula(fit)[3])
  D <- model.frame(as.formula(paste("~",form)),df)
  condNames <- setdiff(names(D), name)
  condNames <- intersect(condNames, names(df))
  D <- cbind(D, df[,setdiff(names(df),names(D)), drop=FALSE])
  
  ## Set up nn-row data frame for prediction
  dots <- list(...)
  xx <- if (is.factor(x)) {
    factor(levels(x),levels=levels(x))
  } else {
    if ("xlim" %in% names(dots)) seq(dots$xlim[1], dots$xlim[2], length=nn) else seq(min(x),max(x),length=nn)
  }
  xxdf <- data.frame(xx)
  names(xxdf) <- name
  df <- fillFrame(f,xxdf,cond)
  DD <- model.frame(as.formula(paste("~",form)),df)
  DD <- cbind(DD,df[,setdiff(names(df),names(DD)),drop=FALSE])
  
  list(x=x, xx=xx, D=D, DD=DD, factor=is.factor(x), name=name, cond=D[1,condNames,drop=FALSE])
}
