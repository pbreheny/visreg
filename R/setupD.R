setupD <- function(fit, f, name, nn, cond, whitespace, ...) {
  ## Set up n-row data frame for residuals
  x <- f[, name]
  xdf <- data.frame(x)
  names(xdf) <- name
  df <- fillFrame(f, xdf, cond)

  rhs_form <- formula(fit)
  rhs_form[2] <- NULL
  simple_rhs <- paste(all.vars(rhs_form), collapse = ' + ')
  simple_form <- as.formula(paste("~", simple_rhs))
  D <- model.frame(simple_form, df)
  condNames <- setdiff(names(D), name)
  condNames <- intersect(condNames, names(df))
  D <- cbind(D, df[, setdiff(names(df), names(D)), drop=FALSE])

  ## Set up nn-row data frame for prediction
  dots <- list(...)
  if (is.factor(x)) {
    xx <- factor(levels(x), levels=levels(x))
  } else {
    if ('xtrans' %in% names(dots)) {
      xx <- c(seq(min(x), max(x), length=nn))
      fi <- approxfun(dots$xtrans(x), x)
      xx <- seq(dots$xtrans(min(x)), dots$xtrans(max(x)), len=nn) |> fi()
    } else {
      xx <- c(seq(min(x), max(x), length=nn))
    }
  }
  xxdf <- data.frame(xx)
  names(xxdf) <- name
  df <- fillFrame(f, xxdf, cond)
  DD <- model.frame(simple_form, df)
  DD <- cbind(DD, df[, setdiff(names(df), names(DD)), drop=FALSE])

  list(x=x, xx=xx, D=D, DD=DD, factor=is.factor(x), name=name, cond=D[1, condNames, drop=FALSE])
}
