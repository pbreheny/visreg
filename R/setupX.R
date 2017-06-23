setupX <- function(fit, f, name, nn, cond, ...) {
  ## Set up n x p matrix for (conditional) partial residuals
  x <- f[,name]
	# Get the reference to plot contrasts from `cond` - if not present, fall back on using the mean (numerical) or the first factor
	xref <- if (is.factor(x)) {
		if (name %in% names(cond)) {
			if (cond[[name]] %in% levels(x)) which(levels(x) == cond[[name]]) else xref = type.convert(cond[[name]], na.strings='')
		} else {
			1
		}
	} else {
		if (name %in% names(cond)) cond[[name]] else mean(x)
	}
  x <- if (is.factor(x)) factor(c(xref, as.integer(x)), labels=levels(x)) else c(xref, x)
  xdf <- data.frame(x)
  names(xdf) <- name
  df <- fillFrame(f, xdf, cond)
  D <- rbind(f[,names(df)], df)
  form <- formula(fit)[3]

  if ("lme" %in% class(fit)) {
    b <- nlme::fixed.effects(fit)
  } else if (sum(grep("merMod", class(fit)))) {
    b <- fit@beta
  } else {
    b <- coef(fit)
  }

  if (class(fit)[1]=="mlm") {
    ind <- apply(is.finite(b), 1, all)
    if (!identical(ind, apply(is.finite(b), 1, any))) stop("Inconsistent NA/NaN coefficients across outcomes")
  } else ind <- is.finite(b)
  if (class(fit)[1]=="gam") {
    form <- parseFormula(formula(fit)[3])
    D <- model.frame(as.formula(paste("~",form)),df)
    X. <- predict(fit, newdata=as.list(D), type="lpmatrix")
  } else if (grepl("merMod", class(fit)[1])) {
    form <- formula(fit, fixed.only = TRUE)
    RHS <- formula(substitute(~R, list(R = form[[length(form)]])))
    X. <- model.matrix(RHS, D)[-(1:nrow(f)), ind]
  } else if (grepl("glmmadmb", class(fit)[1])) {
    form <- as.formula(paste("~", as.character(fit$fixed[3])))
    X. <- model.matrix(form,D)[-(1:nrow(f)), ind]
  } else {
    X. <- model.matrix(as.formula(paste("~",form)),D)[-(1:nrow(f)), ind]
  }
  X <- t(t(X.[-1,])-X.[1,])

  ## Set up data frame with nn rows for prediction
  dots <- list(...)
  xx <- if (is.factor(x)) {
    factor(c(xref, 1:length(levels(x))),labels=levels(x))
  } else {
    if ("xlim" %in% names(dots)) c(xref, seq(dots$xlim[1], dots$xlim[2], length=nn)) else c(xref, seq(min(x),max(x),length=nn))
  }
  xxdf <- data.frame(xx)
  names(xxdf) <- name
  df <- fillFrame(f,xxdf,cond)
  DD <- rbind(f[,names(df)],df)
  if (class(fit)[1]=="gam") {
    DD <- model.frame(as.formula(paste("~",form)),df)
    XX. <- predict(fit, newdata=as.list(DD), type="lpmatrix")
  } else if (grepl("merMod", class(fit)[1])) {
    XX. <- model.matrix(RHS, DD)[-(1:nrow(f)), ind]
  } else XX. <- model.matrix(as.formula(paste("~",form)),DD)[-(1:nrow(f)), ind]
  XX <- t(t(XX.[-1,])-XX.[1,])

  ## Remove extraneous columns for coxph
  if ("coxph" %in% class(fit)) {
    remove.xx <- c(grep("(Intercept)", colnames(XX), fixed=TRUE),
                   grep("cluster(", colnames(XX), fixed=TRUE))
    remove.x <- c(grep("(Intercept)", colnames(X), fixed=TRUE),
                  grep("cluster(", colnames(X), fixed=TRUE))
    XX <- XX[, -remove.xx, drop=FALSE]
    X <- X[, -remove.xx, drop=FALSE]
  }
  condNames <- names(model.frame(as.formula(paste("~", parseFormula(formula(fit)[3]))), df))
  condNames <- setdiff(condNames, name)
  condNames <- intersect(condNames, names(df))
  return(list(x=x[-1], xx=xx[-1], X=X, XX=XX, factor=is.factor(x), name=name, cond=df[1,condNames,drop=FALSE]))
}
